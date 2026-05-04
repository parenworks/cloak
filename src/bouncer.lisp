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
   (running-p :initform nil :accessor bouncer-running-p)
   (start-time :initform (get-universal-time) :accessor bouncer-start-time)
   (last-sender :initform nil :accessor bouncer-last-sender
                :documentation "The client that sent the last PRIVMSG/NOTICE, for echo dedup."))
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
  "Connect all configured upstream networks for all users.
Connections are made in a background thread so startup isn't blocked."
  (bt:make-thread
   (lambda ()
     (let ((first-p t))
       (dolist (user (config-users (bouncer-config bouncer)))
         (dolist (net (user-networks user))
           ;; Stagger connections to avoid IRC server throttling
           (unless first-p (sleep 2))
           (setf first-p nil)
           (let* ((key (bouncer--upstream-key (user-name user) (network-name net)))
                  (upstream (make-upstream net
                              :message-handler
                              (lambda (upstream raw-line msg)
                                (bouncer--on-upstream-message bouncer
                                                              (user-name user)
                                                              upstream raw-line msg)))))
             ;; Fire disconnect hooks when upstream drops
             (setf (upstream-on-state-change upstream)
                   (lambda (us new-state)
                     (when (eq new-state :disconnected)
                       (cloak.modules:run-upstream-disconnect-hooks bouncer us))))
             (setf (gethash key (bouncer-upstreams bouncer)) upstream)
             (upstream-connect upstream))))))
   :name "cloak-connect-upstreams"))

;;; --- Buffer Management ---

(defun bouncer--buffer-key (user-name network-name target)
  "Generate hash key for a message buffer."
  (format nil "~a/~a/~a" user-name network-name (string-downcase target)))

(defun bouncer--get-buffer (bouncer user-name network-name target)
  "Get or create the message buffer for a target."
  (let ((key (bouncer--buffer-key user-name network-name target)))
    (or (gethash key (bouncer-buffers bouncer))
        (let ((net-cfg (find-network user-name network-name
                                     (bouncer-config bouncer))))
          (setf (gethash key (bouncer-buffers bouncer))
                (make-message-buffer
                 :capacity (if net-cfg
                               (network-buffer-size net-cfg)
                               500)))))))

(defun bouncer--server-noise-p (command msg)
  "Return T if MSG is server connection noise that should not be buffered.
  This includes server NOTICEs (no ! in source) and NickServ/ChanServ NOTICEs."
  (when (string= command "NOTICE")
    (let ((source (cloak.protocol:irc-message-source msg)))
      (or
       ;; Server NOTICE (source has no ! = it's a server, not a user)
       (and source (not (position #\! source)))
       ;; NickServ / ChanServ notices
       (and source
            (member (cloak.protocol:source-nick source)
                    '("NickServ" "ChanServ") :test #'string-equal))))))

;;; --- Message Relay ---

(defun bouncer--on-upstream-message (bouncer user-name upstream raw-line msg)
  "Handle a message from an upstream IRC server.
Buffer it and relay to any attached clients."
  ;; Run module upstream hooks first
  (let ((hook-result (cloak.modules:run-upstream-hooks bouncer upstream raw-line msg)))
    (when (member hook-result '(:halt :drop))
      (return-from bouncer--on-upstream-message)))
  (let* ((network-name (upstream-network-name upstream))
         (command (cloak.protocol:irc-message-command msg))
         (target (bouncer--message-target msg (upstream-nick upstream))))
    ;; Fire module hooks for connection/channel events
    (cond
      ((string= command "001")
       (cloak.modules:run-upstream-connect-hooks bouncer upstream))
      ((and (string= command "JOIN")
            (string-equal (cloak.protocol:source-nick
                           (cloak.protocol:irc-message-source msg))
                          (upstream-nick upstream)))
       (cloak.modules:run-channel-join-hooks bouncer upstream
         (first (cloak.protocol:irc-message-params msg))))
      ((and (string= command "PART")
            (string-equal (cloak.protocol:source-nick
                           (cloak.protocol:irc-message-source msg))
                          (upstream-nick upstream)))
       (cloak.modules:run-channel-part-hooks bouncer upstream
         (first (cloak.protocol:irc-message-params msg))))
      ((and (string= command "KICK")
            (string-equal (second (cloak.protocol:irc-message-params msg))
                          (upstream-nick upstream)))
       (cloak.modules:run-channel-kick-hooks bouncer upstream
         (first (cloak.protocol:irc-message-params msg))
         (cloak.protocol:source-nick (cloak.protocol:irc-message-source msg))
         (or (third (cloak.protocol:irc-message-params msg)) ""))))
    ;; Detect echo-message (server echoing our own PRIVMSG/NOTICE back)
    (let ((echo-p (and (member command '("PRIVMSG" "NOTICE") :test #'string=)
                       (string-equal (cloak.protocol:source-nick
                                      (cloak.protocol:irc-message-source msg))
                                     (upstream-nick upstream)))))
      ;; Buffer conversation messages (state events like JOIN/PART are
      ;; replayed via attach-client's channel state replay instead)
      ;; Skip: server noise, and echo-messages (already buffered locally by catch-all)
      (when (and (member command '("PRIVMSG" "NOTICE" "TOPIC") :test #'string=)
                (not echo-p)
                (not (bouncer--server-noise-p command msg)))
        (let ((buffer (bouncer--get-buffer bouncer user-name network-name
                                            (or target network-name)))
              (msgid (cdr (assoc "msgid"
                                 (cloak.protocol:irc-message-tags msg)
                                 :test #'string=))))
          (buffer-push buffer raw-line msgid)))
      ;; Relay to attached clients
      ;; Suppress: echo-messages (client displays locally) and server noise
      ;; (Ident checks, NickServ/ChanServ notices - no value to clients)
      (unless (or echo-p (bouncer--server-noise-p command msg))
        (bt:with-lock-held ((bouncer-lock bouncer))
          (dolist (client (bouncer-clients bouncer))
            (when (and (client-authenticated-p client)
                       (string-equal (client-user client) user-name)
                       (string-equal (client-network client) network-name))
              (client-send client raw-line))))))))

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
    (setf (client-user client) user-name)
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
      ;; Replay channel state (JOINs only - NAMES sent on client request)
      ;; NAMES data for 16 channels with 1000+ nicks each would overwhelm
      ;; clients like Emacs. Clients that need NAMES send explicit requests,
      ;; which we answer locally from tracked nick data.
      (maphash (lambda (chan _v)
                 (declare (ignore _v))
                 (client-send client
                              (format nil ":~a!~a@CLoak JOIN ~a"
                                      (upstream-nick upstream)
                                      user-name chan)))
               (upstream-channels upstream))
      ;; Clear AWAY now that a client is attached
      (bouncer--set-away bouncer client network-name nil)
      ;; Fire attach hooks BEFORE playback so modules (e.g. clientbuffer)
      ;; can set client-last-playback to their stored position
      (cloak.modules:run-client-attach-hooks bouncer client user-name network-name)
      ;; Playback buffered messages
      (playback-buffer bouncer client user-name network-name))))

(defun detach-client (bouncer client)
  "Detach CLIENT from BOUNCER."
  ;; Fire module hooks before removing
  (cloak.modules:run-client-detach-hooks bouncer client)
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
  ;; Run module downstream hooks first
  (let ((hook-result (cloak.modules:run-downstream-hooks bouncer client line msg)))
    (when (member hook-result '(:halt :drop))
      (return-from bouncer--on-client-message)))
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
      ;; *status commands
      ((and (string= command "PRIVMSG")
            (let ((target (first (cloak.protocol:irc-message-params msg))))
              (string-equal target "*status")))
       (bouncer--handle-status bouncer user-name client msg))
      ;; JOIN - handle duplicate detection and config persistence
      ((and (string= command "JOIN") upstream)
       (let* ((channels-str (first (cloak.protocol:irc-message-params msg)))
              (channels (split-sequence:split-sequence #\, channels-str)))
         (dolist (chan channels)
           (if (gethash chan (upstream-channels upstream))
               ;; Already in channel - replay state to client instead of forwarding
               (client-send client
                            (format nil ":~a!~a@CLoak JOIN ~a"
                                    (upstream-nick upstream) user-name chan))
               ;; New channel - forward to upstream and persist to autojoin
               (progn
                 (upstream-send upstream (format nil "JOIN ~a" chan))
                 (bouncer--add-autojoin bouncer user-name network chan))))))
      ;; PART - forward and update config
      ((and (string= command "PART") upstream)
       (let ((chan (first (cloak.protocol:irc-message-params msg))))
         (upstream-send upstream line)
         (bouncer--remove-autojoin bouncer user-name network chan)))
      ;; NAMES - respond locally from tracked nick data, throttled
      ((and (string= command "NAMES") upstream)
       (let* ((chan (or (first (cloak.protocol:irc-message-params msg)) "*"))
              (nicks-ht (gethash chan (upstream-channel-nicks upstream)))
              (nick-list nil))
         (when nicks-ht
           (maphash (lambda (nick _v)
                      (declare (ignore _v))
                      (push nick nick-list))
                    nicks-ht))
         ;; Send RPL_NAMREPLY (353) in chunks of ~400 chars to stay under line limit
         ;; Throttle between chunks so clients like Emacs can process
         (when nick-list
           (let ((chunk nil)
                 (chunk-len 0)
                 (chunks-sent 0))
             (dolist (nick nick-list)
               (let ((nick-len (+ (length nick) 1))) ; +1 for space
                 (when (> (+ chunk-len nick-len) 400)
                   ;; Flush current chunk
                   (client-send client
                                (format nil ":CLoak 353 ~a = ~a :~{~a~^ ~}"
                                        (upstream-nick upstream) chan (nreverse chunk)))
                   (incf chunks-sent)
                   ;; Yield every 3 chunks to let client process
                   (when (zerop (mod chunks-sent 3))
                     (sleep 0.02))
                   (setf chunk nil chunk-len 0))
                 (push nick chunk)
                 (incf chunk-len nick-len)))
             ;; Flush remaining
             (when chunk
               (client-send client
                            (format nil ":CLoak 353 ~a = ~a :~{~a~^ ~}"
                                    (upstream-nick upstream) chan (nreverse chunk))))))
         ;; End of names
         (client-send client
                      (format nil ":CLoak 366 ~a ~a :End of /NAMES list"
                              (upstream-nick upstream) chan))))
      ;; WHO - respond locally with end-of-who
      ((and (string= command "WHO") upstream)
       ;; Don't forward to upstream - would flood with 16 channels.
       (let ((mask (or (first (cloak.protocol:irc-message-params msg)) "*")))
         (client-send client
                      (format nil ":CLoak 315 ~a ~a :End of /WHO list"
                              (upstream-nick upstream) mask))))
      ;; PRIVMSG/NOTICE to NickServ/ChanServ - drop if it looks like
      ;; a re-identify attempt (client sending bouncer password to services)
      ((and (member command '("PRIVMSG" "NOTICE") :test #'string=)
            upstream
            (let ((target (first (cloak.protocol:irc-message-params msg))))
              (and (member target '("NickServ" "nickserv") :test #'string-equal)
                   (let ((text (second (cloak.protocol:irc-message-params msg))))
                     (and text (search "/" text))))))
       ;; Drop - client is trying to auth with NickServ using bouncer password
       (format t "[CLoak] Dropped NickServ identify from client (CLoak handles SASL)~%")
       (force-output))
      ;; Everything else - forward to upstream
      (upstream
       ;; Track sender for echo-message dedup
       (when (member command '("PRIVMSG" "NOTICE") :test #'string=)
         (setf (bouncer-last-sender bouncer) client)
         ;; Buffer our own outgoing message (server won't echo it back)
         (let* ((target (first (cloak.protocol:irc-message-params msg)))
                (nick (upstream-nick upstream))
                (buffered-line (format nil ":~a!~a@CLoak ~a ~a :~a"
                                       nick user-name command target
                                       (second (cloak.protocol:irc-message-params msg))))
                (buffer (bouncer--get-buffer bouncer user-name network target)))
           (buffer-push buffer buffered-line)))
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
  (let ((since (client-last-playback client))
        (prefix (format nil "~a/~a/" user-name network-name))
        (total 0))
    ;; Replay all buffers for this network, throttled to avoid overwhelming clients
    (maphash (lambda (key buffer)
               (when (alex:starts-with-subseq prefix key)
                 (let ((msgs (buffer-messages-since buffer since)))
                   (dolist (msg msgs)
                     (incf total)
                     (client-send client (stored-message-raw msg)))
                   ;; Small delay between buffers to let client process
                   (when msgs (sleep 0.05)))))
             (bouncer-buffers bouncer))
    (when (plusp total)
      (format t "[CLoak] Played back ~d messages for ~a~%" total user-name)
      (force-output))
    ;; Update playback timestamp
    (setf (client-last-playback client) (get-universal-time))))

;;; --- Start / Stop ---

(defun start-bouncer (bouncer)
  "Start the CLoak bouncer."
  (format t "~&[CLoak] Starting bouncer v~a~%"
          cloak:*version*)
  (setf (bouncer-running-p bouncer) t)
  (setf *bouncer* bouncer)
  ;; Scan for third-party plugins
  (cloak.modules:scan-plugins)
  ;; Load enabled modules
  (let ((modules (config-enabled-modules (bouncer-config bouncer))))
    (when modules
      (dolist (name modules)
        (cloak.modules:load-module name bouncer))
      (format t "[CLoak] Loaded ~d module~:p~%" (length modules))))
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

;;; --- Autojoin Persistence ---

(defun bouncer--add-autojoin (bouncer user-name network-name channel)
  "Add CHANNEL to the autojoin list for USER-NAME's NETWORK-NAME and save."
  (let ((net-cfg (find-network user-name network-name (bouncer-config bouncer))))
    (when net-cfg
      (unless (member channel (network-autojoin net-cfg) :test #'string-equal)
        (setf (network-autojoin net-cfg)
              (append (network-autojoin net-cfg) (list channel)))
        (save-config (bouncer-config bouncer))
        (format t "[CLoak] Added ~a to autojoin for ~a/~a~%"
                channel user-name network-name)))))

(defun bouncer--remove-autojoin (bouncer user-name network-name channel)
  "Remove CHANNEL from the autojoin list for USER-NAME's NETWORK-NAME and save."
  (let ((net-cfg (find-network user-name network-name (bouncer-config bouncer))))
    (when net-cfg
      (when (member channel (network-autojoin net-cfg) :test #'string-equal)
        (setf (network-autojoin net-cfg)
              (remove channel (network-autojoin net-cfg) :test #'string-equal))
        (save-config (bouncer-config bouncer))
        (format t "[CLoak] Removed ~a from autojoin for ~a/~a~%"
                channel user-name network-name)))))

;;; --- *status Commands ---

(defun bouncer--status-reply (client text)
  "Send a NOTICE from *status to CLIENT."
  (client-send client (format nil ":*status!status@CLoak NOTICE ~a :~a"
                               (or (client-nick client) "*") text)))

(defun bouncer--handle-status (bouncer user-name client msg)
  "Handle a /msg *status command from CLIENT."
  (let* ((text (second (cloak.protocol:irc-message-params msg)))
         (parts (and text (split-sequence:split-sequence #\Space text
                            :remove-empty-subseqs t)))
         (cmd (string-downcase (or (first parts) "")))
         (args (rest parts)))
    (cond
      ((string= cmd "help")
       (bouncer--status-reply client "CLoak bouncer commands:")
       (bouncer--status-reply client "  help           - Show this help")
       (bouncer--status-reply client "  version        - Show CLoak version")
       (bouncer--status-reply client "  listnets       - List configured networks")
       (bouncer--status-reply client "  listchans      - List joined channels")
       (bouncer--status-reply client "  listclients    - List connected clients")
       (bouncer--status-reply client "  connect <net>  - Connect to a network")
       (bouncer--status-reply client "  disconnect <net> - Disconnect from a network")
       (bouncer--status-reply client "  jump <net>     - Reconnect to a network")
       (bouncer--status-reply client "  uptime         - Show bouncer uptime")
       (bouncer--status-reply client "  saveconfig     - Save current configuration")
       (bouncer--status-reply client "  reloadconfig   - Reload configuration from disk"))

      ((string= cmd "version")
       (bouncer--status-reply client
        (format nil "CLoak v~a" cloak:*version*)))

      ((string= cmd "listnets")
       (let ((user-cfg (find-user user-name (bouncer-config bouncer))))
         (if user-cfg
             (dolist (net (user-networks user-cfg))
               (let* ((key (bouncer--upstream-key user-name (network-name net)))
                      (up (gethash key (bouncer-upstreams bouncer)))
                      (status (if (and up (upstream-connected-p up)) "connected" "disconnected")))
                 (bouncer--status-reply client
                  (format nil "  ~a (~a:~d) [~a]"
                          (network-name net) (network-server net)
                          (network-port net) status))))
             (bouncer--status-reply client "No networks configured."))))

      ((string= cmd "listchans")
       (let ((upstream (bouncer--get-upstream bouncer user-name
                         (client-network client))))
         (if (and upstream (> (hash-table-count (upstream-channels upstream)) 0))
             (maphash (lambda (chan _v)
                        (declare (ignore _v))
                        (bouncer--status-reply client (format nil "  ~a" chan)))
                      (upstream-channels upstream))
             (bouncer--status-reply client "No channels joined."))))

      ((string= cmd "listclients")
       (bt:with-lock-held ((bouncer-lock bouncer))
         (let ((count 0))
           (dolist (c (bouncer-clients bouncer))
             (when (string-equal (client-network c) (client-network client))
               (incf count)
               (bouncer--status-reply client
                (format nil "  ~a [~a]" (or (client-nick c) "?")
                        (client-network c)))))
           (bouncer--status-reply client (format nil "~d client(s) attached." count)))))

      ((string= cmd "connect")
       (let ((net-name (first args)))
         (if net-name
             (let ((upstream (bouncer--get-upstream bouncer user-name net-name)))
               (if upstream
                   (if (upstream-connected-p upstream)
                       (bouncer--status-reply client
                        (format nil "Already connected to ~a." net-name))
                       (progn
                         (bouncer--status-reply client
                          (format nil "Connecting to ~a..." net-name))
                         (bt:make-thread
                          (lambda () (upstream-connect upstream))
                          :name "cloak-reconnect")))
                   (bouncer--status-reply client
                    (format nil "Unknown network: ~a" net-name))))
             (bouncer--status-reply client "Usage: connect <network>"))))

      ((string= cmd "disconnect")
       (let ((net-name (first args)))
         (if net-name
             (let ((upstream (bouncer--get-upstream bouncer user-name net-name)))
               (if upstream
                   (progn
                     (setf (upstream-reconnect-p upstream) nil)
                     (upstream-disconnect upstream)
                     (bouncer--status-reply client
                      (format nil "Disconnected from ~a." net-name)))
                   (bouncer--status-reply client
                    (format nil "Unknown network: ~a" net-name))))
             (bouncer--status-reply client "Usage: disconnect <network>"))))

      ((string= cmd "jump")
       (let ((net-name (first args)))
         (if net-name
             (let ((upstream (bouncer--get-upstream bouncer user-name net-name)))
               (if upstream
                   (progn
                     (bouncer--status-reply client
                      (format nil "Reconnecting to ~a..." net-name))
                     (setf (upstream-reconnect-p upstream) t)
                     (when (upstream-connected-p upstream)
                       (upstream-disconnect upstream))
                     (bt:make-thread
                      (lambda () (upstream-connect upstream))
                      :name "cloak-jump"))
                   (bouncer--status-reply client
                    (format nil "Unknown network: ~a" net-name))))
             (bouncer--status-reply client "Usage: jump <network>"))))

      ((string= cmd "uptime")
       (bouncer--status-reply client
        (format nil "Bouncer running since ~a"
                (local-time:format-timestring nil
                 (local-time:universal-to-timestamp
                  (bouncer-start-time bouncer))))))

      ((string= cmd "saveconfig")
       (handler-case
           (progn
             (save-config (bouncer-config bouncer))
             (bouncer--status-reply client "Configuration saved."))
         (error (e)
           (bouncer--status-reply client (format nil "Save failed: ~a" e)))))

      ((string= cmd "reloadconfig")
       (handler-case
           (let ((new-config (load-config)))
             (setf (bouncer-config bouncer) new-config)
             (bouncer--status-reply client "Configuration reloaded."))
         (error (e)
           (bouncer--status-reply client (format nil "Reload failed: ~a" e)))))

      (t
       (bouncer--status-reply client
        (format nil "Unknown command: ~a (try 'help')" cmd))))))

;;; --- New Client Handling ---

(defun bouncer--client-ip (client)
  "Extract the remote IP address string from CLIENT's socket, or \"unknown\"."
  (handler-case
      (let ((sock (slot-value client 'cloak.downstream::socket)))
        (if sock
            (format nil "~a" (iolib:remote-host sock))
            "unknown"))
    (error () "unknown")))

(defun bouncer--on-new-client (bouncer client)
  "Handle a newly connected client. Wait for auth then attach."
  ;; Run new-connection hooks (fail2ban can reject here)
  (let ((ip (bouncer--client-ip client)))
    (when (eq :drop (cloak.modules:run-new-connection-hooks bouncer ip))
      (client-send client ":CLoak NOTICE * :Connection refused")
      (client-disconnect client)
      (return-from bouncer--on-new-client)))
  ;; Set a temporary handler that processes registration
  (setf (client-message-handler client)
        (lambda (client line msg)
          (declare (ignore line))
          (bouncer--handle-client-auth bouncer client msg))))

(defun bouncer--handle-client-auth (bouncer client msg)
  "Process authentication from a new CLIENT."
  (let ((command (cloak.protocol:irc-message-command msg)))
    ;; Handle CAP negotiation during registration
    (when (string= command "CAP")
      (let ((subcmd (first (cloak.protocol:irc-message-params msg))))
        (cond
          ((string-equal subcmd "LS")
           ;; Respond with empty capability list
           (client-send client ":CLoak CAP * LS :"))
          ((string-equal subcmd "END")
           ;; Client finished cap negotiation, nothing to do
           nil)))
      (return-from bouncer--handle-client-auth))
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
                    (cloak.modules:run-auth-failure-hooks bouncer
                      (bouncer--client-ip client))
                    (client-send client ":CLoak 464 * :Password incorrect")
                    (client-disconnect client))))
            ;; Bad format
            (progn
              (cloak.modules:run-auth-failure-hooks bouncer
                (bouncer--client-ip client))
              (client-send client
                           ":CLoak NOTICE * :Use PASS user/network:password to authenticate")
              (client-disconnect client)))))))
