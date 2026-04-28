;;;; bouncer.lisp - Core bouncer logic for CLoak
;;;; Orchestrates upstream/downstream connections, message relay, and buffering.

(in-package #:cloak.bouncer)

;;; --- Bouncer ---

(defclass bouncer ()
  ((config :initarg :config :accessor bouncer-config)
   (upstreams :initform (make-hash-table :test 'equal) :accessor bouncer-upstreams
              :documentation "Hash: \"user/network\" -> upstream-connection")
   (clients :initform nil :accessor bouncer-clients
            :documentation "List of connected downstream-client instances")
   (buffers :initform (make-hash-table :test 'equal) :accessor bouncer-buffers
            :documentation "Hash: \"user/network/target\" -> message-buffer")
   (lock :initform (bt:make-lock "bouncer-lock") :accessor bouncer-lock)
   (running-p :initform nil :accessor bouncer-running-p))
  (:documentation "The core CLoak bouncer instance."))

(defvar *bouncer* nil "The active bouncer instance.")

(defun make-bouncer (config)
  "Create a bouncer from CONFIG."
  (make-instance 'bouncer :config config))

;;; --- Upstream Management ---

(defun bouncer--upstream-key (user-name network-name)
  "Generate hash key for an upstream connection."
  (format nil "~a/~a" user-name network-name))

(defun bouncer--get-upstream (bouncer user-name network-name)
  "Get the upstream connection for USER-NAME on NETWORK-NAME."
  (gethash (bouncer--upstream-key user-name network-name)
           (bouncer-upstreams bouncer)))

(defun bouncer--connect-upstreams (bouncer)
  "Connect all configured upstream networks for all users."
  (dolist (user (config-users (bouncer-config bouncer)))
    (dolist (net (user-networks user))
      (let* ((key (bouncer--upstream-key (user-name user) (network-name net)))
             (upstream (make-upstream net
                         :message-handler
                         (lambda (upstream raw-line msg)
                           (bouncer--on-upstream-message bouncer
                                                         (user-name user)
                                                         upstream raw-line msg)))))
        (setf (gethash key (bouncer-upstreams bouncer)) upstream)
        (upstream-connect upstream)))))

;;; --- Buffer Management ---

(defun bouncer--buffer-key (user-name network-name target)
  "Generate hash key for a message buffer."
  (format nil "~a/~a/~a" user-name network-name (string-downcase target)))

(defun bouncer--get-buffer (bouncer user-name network-name target)
  "Get or create the message buffer for a target."
  (let ((key (bouncer--buffer-key user-name network-name target)))
    (or (gethash key (bouncer-buffers bouncer))
        (setf (gethash key (bouncer-buffers bouncer))
              (make-message-buffer :capacity 500)))))

;;; --- Message Relay ---

(defun bouncer--on-upstream-message (bouncer user-name upstream raw-line msg)
  "Handle a message from an upstream IRC server.
Buffer it and relay to any attached clients."
  (let* ((network-name (upstream-network-name upstream))
         (command (cloak.protocol:irc-message-command msg))
         (target (bouncer--message-target msg (upstream-nick upstream))))
    ;; Handle CTCP VERSION requests
    (when (and (string= command "PRIVMSG")
               (let ((text (second (cloak.protocol:irc-message-params msg))))
                 (and text
                      (> (length text) 2)
                      (char= (char text 0) #\Soh)
                      (search "VERSION" text))))
      (let ((sender (cloak.protocol:source-nick
                     (cloak.protocol:irc-message-source msg))))
        (when sender
          (upstream-send upstream
                         (format nil "NOTICE ~a :~aCLoak v~a - Common Lisp IRC Bouncer~a"
                                 sender (string #\Soh)
                                 (asdf:component-version (asdf:find-system "cloak"))
                                 (string #\Soh))))))
    ;; Buffer messages that clients need to see
    (when (member command '("PRIVMSG" "NOTICE" "JOIN" "PART" "QUIT"
                            "KICK" "TOPIC" "MODE" "NICK")
                  :test #'string=)
      (let ((buffer (bouncer--get-buffer bouncer user-name network-name
                                          (or target network-name)))
            (msgid (cdr (assoc "msgid"
                               (cloak.protocol:irc-message-tags msg)
                               :test #'string=))))
        (buffer-push buffer raw-line msgid)))
    ;; Relay to attached clients
    (bt:with-lock-held ((bouncer-lock bouncer))
      (dolist (client (bouncer-clients bouncer))
        (when (and (client-authenticated-p client)
                   (string-equal (client-network client) network-name))
          ;; Don't relay our own messages back (they're already echoed)
          (unless (and (string= command "PRIVMSG")
                       (string-equal (cloak.protocol:source-nick
                                      (cloak.protocol:irc-message-source msg))
                                     (upstream-nick upstream)))
            (client-send client raw-line)))))))

(defun bouncer--message-target (msg our-nick)
  "Determine the buffer target for MSG. DMs use the other party's nick."
  (let ((command (cloak.protocol:irc-message-command msg))
        (params (cloak.protocol:irc-message-params msg)))
    (when (member command '("PRIVMSG" "NOTICE") :test #'string=)
      (let ((target (first params)))
        (if (string-equal target our-nick)
            ;; DM to us - buffer under sender's nick
            (cloak.protocol:source-nick (cloak.protocol:irc-message-source msg))
            target)))))

;;; --- Client Attachment ---

(defun attach-client (bouncer client user-name network-name)
  "Attach CLIENT to BOUNCER for USER-NAME's NETWORK-NAME."
  (bt:with-lock-held ((bouncer-lock bouncer))
    (setf (client-authenticated-p client) t)
    (setf (client-network client) network-name)
    (push client (bouncer-clients bouncer)))
  ;; Set up client message handler
  (setf (client-message-handler client)
        (lambda (client line msg)
          (bouncer--on-client-message bouncer user-name client line msg)))
  (setf (client-disconnect-handler client)
        (lambda (client)
          (detach-client bouncer client)))
  ;; Send welcome and state
  (let ((upstream (bouncer--get-upstream bouncer user-name network-name)))
    (when upstream
      ;; Send 001 welcome
      (client-send client
                   (format nil ":CLoak 001 ~a :Welcome to CLoak bouncer"
                           (upstream-nick upstream)))
      ;; Replay channel state
      (maphash (lambda (chan _v)
                 (declare (ignore _v))
                 (client-send client
                              (format nil ":~a!~a@CLoak JOIN ~a"
                                      (upstream-nick upstream)
                                      user-name chan)))
               (upstream-channels upstream))
      ;; Clear AWAY now that a client is attached
      (bouncer--set-away bouncer client network-name nil)
      ;; Playback buffered messages
      (playback-buffer bouncer client user-name network-name))))

(defun detach-client (bouncer client)
  "Detach CLIENT from BOUNCER."
  (let ((network (client-network client)))
    (bt:with-lock-held ((bouncer-lock bouncer))
      (setf (bouncer-clients bouncer)
            (remove client (bouncer-clients bouncer)))
      ;; Set AWAY if no clients remain for this network
      (unless (find network (bouncer-clients bouncer)
                    :key #'client-network :test #'string-equal)
        (bouncer--set-away bouncer client network t))))
  (format t "[CLoak] Client detached~%")
  (force-output))

;;; --- Client Message Handling ---

(defun bouncer--on-client-message (bouncer user-name client line msg)
  "Handle a message from a downstream IRC client."
  (let* ((command (cloak.protocol:irc-message-command msg))
         (network (client-network client))
         (upstream (bouncer--get-upstream bouncer user-name network)))
    (cond
      ;; QUIT - detach client, don't forward
      ((string= command "QUIT")
       (client-disconnect client))
      ;; PING from client - respond directly
      ((string= command "PING")
       (client-send client
                    (cloak.protocol:irc-pong
                     (or (first (cloak.protocol:irc-message-params msg)) "CLoak"))))
      ;; Everything else - forward to upstream
      (upstream
       (upstream-send upstream line)))))

;;; --- Away Management ---

(defun bouncer--set-away (bouncer client network-name away-p)
  "Set or clear AWAY on the upstream for NETWORK-NAME.
Finds the upstream using any user who owns that network."
  (declare (ignore client))
  (maphash (lambda (key upstream)
             (declare (ignore key))
             (when (and (string-equal (upstream-network-name upstream) network-name)
                        (upstream-connected-p upstream))
               (if away-p
                   (upstream-send upstream "AWAY :Detached from CLoak")
                   (upstream-send upstream "AWAY"))))
           (bouncer-upstreams bouncer)))

;;; --- Playback ---

(defun playback-buffer (bouncer client user-name network-name)
  "Send buffered messages to CLIENT since their last playback."
  (let ((since (client-last-playback client)))
    ;; Replay all buffers for this network
    (maphash (lambda (key buffer)
               (when (alex:starts-with-subseq
                      (format nil "~a/~a/" user-name network-name)
                      key)
                 (dolist (msg (buffer-messages-since buffer since))
                   (client-send client (stored-message-raw msg)))))
             (bouncer-buffers bouncer))
    ;; Update playback timestamp
    (setf (client-last-playback client) (get-universal-time))))

;;; --- Start / Stop ---

(defun start-bouncer (bouncer)
  "Start the CLoak bouncer."
  (format t "~&[CLoak] Starting bouncer v~a~%"
          (asdf:component-version (asdf:find-system "cloak")))
  (setf (bouncer-running-p bouncer) t)
  (setf *bouncer* bouncer)
  ;; Connect all upstreams
  (bouncer--connect-upstreams bouncer)
  ;; Start client listener
  (let ((cfg (bouncer-config bouncer)))
    (start-listener (config-listen-host cfg)
                    (config-listen-port cfg)
                    :tls-cert (config-tls-cert cfg)
                    :tls-key (config-tls-key cfg)
                    :on-connect (lambda (client)
                                  (bouncer--on-new-client bouncer client))))
  (format t "[CLoak] Bouncer running~%")
  bouncer)

(defun stop-bouncer (bouncer)
  "Stop the CLoak bouncer."
  (format t "[CLoak] Stopping bouncer...~%")
  (setf (bouncer-running-p bouncer) nil)
  ;; Disconnect all clients
  (bt:with-lock-held ((bouncer-lock bouncer))
    (dolist (client (bouncer-clients bouncer))
      (client-send client ":CLoak NOTICE * :Bouncer shutting down")
      (client-disconnect client))
    (setf (bouncer-clients bouncer) nil))
  ;; Disconnect all upstreams (disable reconnect first)
  (maphash (lambda (key upstream)
             (declare (ignore key))
             (setf (upstream-reconnect-p upstream) nil)
             (ignore-errors
               (upstream-send upstream
                              (cloak.protocol:irc-quit "CLoak shutting down"))
               (upstream-disconnect upstream)))
           (bouncer-upstreams bouncer))
  ;; Stop listener
  (stop-listener)
  (setf *bouncer* nil)
  (format t "[CLoak] Bouncer stopped~%"))

;;; --- New Client Handling ---

(defun bouncer--on-new-client (bouncer client)
  "Handle a newly connected client. Wait for auth then attach."
  ;; Set a temporary handler that processes registration
  (setf (client-message-handler client)
        (lambda (client line msg)
          (declare (ignore line))
          (bouncer--handle-client-auth bouncer client msg))))

(defun bouncer--handle-client-auth (bouncer client msg)
  "Process authentication from a new CLIENT."
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (string= command "USER")
      ;; Try to authenticate - PASS should contain user/network:password
      (let* ((pass-data (or (client-user client) ""))
             (slash-pos (position #\/ pass-data))
             (colon-pos (position #\: pass-data :from-end t)))
        (if (and slash-pos colon-pos (< slash-pos colon-pos))
            (let* ((user-name (subseq pass-data 0 slash-pos))
                   (network (subseq pass-data (1+ slash-pos) colon-pos))
                   (password (subseq pass-data (1+ colon-pos)))
                   (user-cfg (find-user user-name (bouncer-config bouncer))))
              (if (and user-cfg
                       (verify-password password (user-password-hash user-cfg)))
                  ;; Auth success
                  (progn
                    (format t "[CLoak] Authenticated: ~a -> ~a~%" user-name network)
                    (attach-client bouncer client user-name network))
                  ;; Auth failure
                  (progn
                    (client-send client ":CLoak 464 * :Password incorrect")
                    (client-disconnect client))))
            ;; Bad format
            (progn
              (client-send client
                           ":CLoak NOTICE * :Use PASS user/network:password to authenticate")
              (client-disconnect client)))))))
