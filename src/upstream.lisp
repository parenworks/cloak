;;;; upstream.lisp - Upstream IRC server connections for CLoak
;;;; Manages persistent connections from the bouncer to IRC networks.

(in-package #:cloak.upstream)

;;; --- Upstream Connection ---

(defclass upstream-connection ()
  ((network-name :initarg :network-name :accessor upstream-network-name)
   (socket :initform nil :accessor upstream-socket)
   (stream :initform nil :accessor upstream-stream)
   (state :initform :disconnected :accessor upstream-state)
   (nick :initarg :nick :accessor upstream-nick)
   (username :initarg :username :accessor upstream-username
             :initform nil)
   (realname :initarg :realname :accessor upstream-realname
             :initform "CLoak User")
   ;; IRC state tracking
   (channels :initform (make-hash-table :test 'equal) :accessor upstream-channels)
   (channel-nicks :initform (make-hash-table :test 'equal) :accessor upstream-channel-nicks
                  :documentation "Hash table mapping channel names to hash tables of nicks.")
   (cap-enabled :initform nil :accessor upstream-cap-enabled)
   (cap-state :initform nil :accessor upstream-cap-state
              :documentation ":ls-received, :req-sent, :sasl-auth, :done")
   (server-name :initform nil :accessor upstream-server-name)
   ;; Health
   (last-activity :initform (get-universal-time) :accessor upstream-last-activity)
   (ping-pending :initform nil :accessor upstream-ping-pending)
   ;; Reconnect
   (reconnect-p :initform t :accessor upstream-reconnect-p
                :documentation "If T, automatically reconnect on disconnect.")
   (reconnect-attempts :initform 0 :accessor upstream-reconnect-attempts)
   ;; Threading
   (read-thread :initform nil :accessor upstream-read-thread)
   (lock :initform (bt:make-lock "upstream-lock") :accessor upstream-lock)
   ;; Callbacks
   (message-handler :initarg :message-handler :accessor upstream-message-handler
                    :initform nil)
   (on-state-change :initarg :on-state-change :accessor upstream-on-state-change
                    :initform nil
                    :documentation "Callback (lambda (upstream new-state) ...) invoked after state transitions.")
   ;; Config
   (config :initarg :config :accessor upstream-config)
   ;; Send throttling
   (last-send-time :initform 0 :accessor upstream-last-send-time
                   :documentation "Internal real time of last send, for flood protection."))
  (:documentation "A persistent connection from CLoak to an IRC server."))

(defun make-upstream (network-config &key message-handler)
  "Create an upstream connection from NETWORK-CONFIG."
  (make-instance 'upstream-connection
    :network-name (cloak.config:network-name network-config)
    :nick (cloak.config:network-nick network-config)
    :username (or (cloak.config:network-username network-config)
                  (cloak.config:network-nick network-config))
    :realname (cloak.config:network-realname network-config)
    :config network-config
    :message-handler message-handler))

;;; --- Connection ---

(defun upstream-connect (upstream)
  "Connect UPSTREAM to its IRC server. Returns T on success."
  ;; Wait for old read-thread to finish before reconnecting
  (let ((old-thread (upstream-read-thread upstream)))
    (when (and old-thread (bt:thread-alive-p old-thread))
      (ignore-errors (bt:join-thread old-thread))))
  (let* ((config (upstream-config upstream))
         (server (cloak.config:network-server config))
         (port (cloak.config:network-port config))
         (use-tls (cloak.config:network-tls config)))
    (handler-case
        (progn
          (cloak-log "~&[CLoak] Connecting to ~a:~d~a...~%"
                  server port (if use-tls " (TLS)" ""))
          (let ((sock (iolib:make-socket :connect :active
                                          :address-family :internet
                                          :type :stream
                                          :external-format '(:utf-8 :eol-style :crlf)
                                          :ipv6 t)))
            (iolib:connect sock (iolib:lookup-hostname server) :port port :wait t)
            (setf (upstream-socket upstream) sock)
            (let ((stream (if use-tls
                             (cl+ssl:make-ssl-client-stream
                              (iolib:socket-os-fd sock)
                              :hostname server
                              :external-format '(:utf-8 :eol-style :crlf))
                             sock)))
              (setf (upstream-stream upstream) stream)
              (setf (upstream-state upstream) :registering)
              (setf (upstream-last-activity upstream) (get-universal-time))
              ;; Start registration
              (upstream--register upstream)
              ;; Start read loop
              (setf (upstream-read-thread upstream)
                    (bt:make-thread
                     (lambda () (upstream--read-loop upstream))
                     :name (format nil "cloak-upstream-~a"
                                   (upstream-network-name upstream))))
              (cloak-log "[CLoak] Connected to ~a~%" (upstream-network-name upstream))
              t)))
      (error (e)
        (cloak-log "[CLoak] Failed to connect to ~a: ~a~%"
                (upstream-network-name upstream) e)
        (upstream-disconnect upstream)
        nil))))

(defun upstream-disconnect (upstream &key quit)
  "Disconnect UPSTREAM from the IRC server.
When QUIT is true, send QUIT first for a clean shutdown."
  (when (and quit (upstream-stream upstream)
               (not (eq (upstream-state upstream) :disconnected)))
    (ignore-errors
      (upstream-send upstream (irc-quit "CLoak disconnect"))
      (force-output (upstream-stream upstream))))
  (bt:with-lock-held ((upstream-lock upstream))
    (setf (upstream-state upstream) :disconnected)
    (when (upstream-socket upstream)
      (ignore-errors (close (upstream-socket upstream)))
      (setf (upstream-socket upstream) nil))
    (when (upstream-stream upstream)
      (setf (upstream-stream upstream) nil))))

(defun upstream-connected-p (upstream)
  "Return T if UPSTREAM is connected."
  (eq (upstream-state upstream) :connected))

;;; --- Send ---

(defun upstream-send (upstream raw-line)
  "Send RAW-LINE to the IRC server via UPSTREAM.
  Includes flood protection: enforces minimum 2s between sends.
  The throttle is inside the lock to prevent concurrent threads from bypassing it."
  (bt:with-lock-held ((upstream-lock upstream))
    (when (upstream-stream upstream)
      ;; Flood protection: sleep inside lock so concurrent callers queue up
      (let* ((now (get-internal-real-time))
             (elapsed (/ (- now (upstream-last-send-time upstream))
                         (float internal-time-units-per-second)))
             (min-interval 2.0))
        (when (< elapsed min-interval)
          (sleep (- min-interval elapsed))))
      (setf (upstream-last-send-time upstream) (get-internal-real-time))
      (unless (string= (subseq raw-line 0 (min 4 (length raw-line))) "PONG")
        (cloak-log "[CLoak] >> ~a: ~a~%" (upstream-network-name upstream) raw-line))
      (handler-case
          (progn
            (write-string raw-line (upstream-stream upstream))
            (write-string (string #\Return) (upstream-stream upstream))
            (write-string (string #\Newline) (upstream-stream upstream))
            (force-output (upstream-stream upstream)))
        (error (e)
          (cloak-log "[CLoak] Send error on ~a: ~a~%"
                     (upstream-network-name upstream) e)
          ;; Don't call disconnect here (would deadlock on lock)
          (setf (upstream-state upstream) :disconnected))))))

;;; --- Registration ---

(defun upstream--register (upstream)
  "Send IRC registration sequence to server."
  (let ((config (upstream-config upstream)))
    ;; CAP negotiation
    (upstream-send upstream "CAP LS 302")
    ;; PASS if needed
    (when (cloak.config:network-password config)
      (upstream-send upstream (irc-pass (cloak.config:network-password config))))
    ;; NICK and USER
    (upstream-send upstream (irc-nick (upstream-nick upstream)))
    (upstream-send upstream (irc-user (or (upstream-username upstream)
                                          (upstream-nick upstream))
                                      (upstream-realname upstream)))))

;;; --- Read Loop ---

(defun upstream--read-loop (upstream)
  "Read and dispatch messages from the IRC server."
  (handler-case
      (loop while (and (upstream-stream upstream)
                       (not (eq (upstream-state upstream) :disconnected)))
            for line = (handler-case
                           (read-line (upstream-stream upstream) nil nil)
                         (error () nil))
            while line
            do (let ((trimmed (string-right-trim '(#\Return #\Newline) line)))
                 (when (plusp (length trimmed))
                   (setf (upstream-last-activity upstream) (get-universal-time))
                   (upstream--handle-line upstream trimmed))))
    (error (e)
      (cloak-log "[CLoak] Read error on ~a: ~a~%"
              (upstream-network-name upstream) e)))
  ;; Cleanup
  (upstream-disconnect upstream)
  (cloak-log "[CLoak] Upstream ~a disconnected~%" (upstream-network-name upstream))
  ;; Notify state change
  (when (upstream-on-state-change upstream)
    (handler-case
        (funcall (upstream-on-state-change upstream) upstream :disconnected)
      (error (e)
        (cloak-log "[CLoak] State change handler error: ~a~%" e))))
  ;; Auto-reconnect
  (when (upstream-reconnect-p upstream)
    ;; If we were connected stably (>30s), reset attempt counter
    (let ((uptime (- (get-universal-time) (upstream-last-activity upstream))))
      (if (> uptime 30)
          (setf (upstream-reconnect-attempts upstream) 0)
          (incf (upstream-reconnect-attempts upstream))))
    (upstream--reconnect-loop upstream)))

(defun upstream--handle-line (upstream line)
  "Parse and handle a raw IRC LINE from the server."
  (let ((msg (parse-message line)))
    ;; Handle PING internally
    (when (string= (irc-message-command msg) "PING")
      (upstream-send upstream (irc-pong (or (first (irc-message-params msg)) "")))
      (return-from upstream--handle-line))
    ;; Handle CAP negotiation
    (when (upstream--handle-cap upstream msg)
      (return-from upstream--handle-line))
    ;; Track state changes
    (upstream--track-state upstream msg)
    ;; Forward to bouncer via callback
    (when (upstream-message-handler upstream)
      (funcall (upstream-message-handler upstream) upstream line msg))))

;;; --- CAP Negotiation & SASL ---

(defun upstream--handle-cap (upstream msg)
  "Handle CAP negotiation messages. Return T if consumed."
  (let ((command (irc-message-command msg)))
    (cond
      ;; CAP LS response
      ((and (string= command "CAP")
            (member "LS" (irc-message-params msg) :test #'string=))
       (let* ((cap-str (car (last (irc-message-params msg))))
              (caps (split-sequence:split-sequence #\Space cap-str
                                                   :remove-empty-subseqs t))
              (want nil))
         (cloak-log "[CLoak] Server caps: ~{~a~^ ~}~%" caps)
         ;; Request caps we want
         (when (member "sasl" caps :test #'string-equal)
           (push "sasl" want))
         (when (member "server-time" caps :test #'string-equal)
           (push "server-time" want))
         (when (member "message-tags" caps :test #'string-equal)
           (push "message-tags" want))
         (when (member "batch" caps :test #'string-equal)
           (push "batch" want))
         (when (member "labeled-response" caps :test #'string-equal)
           (push "labeled-response" want))
         (when (member "echo-message" caps :test #'string-equal)
           (push "echo-message" want))
         (if want
             (progn
               (upstream-send upstream
                             (format nil "CAP REQ :~{~a~^ ~}" want))
               (setf (upstream-cap-state upstream) :req-sent))
             ;; Nothing to request, end CAP
             (progn
               (upstream-send upstream "CAP END")
               (setf (upstream-cap-state upstream) :done))))
       t)
      ;; CAP ACK
      ((and (string= command "CAP")
            (member "ACK" (irc-message-params msg) :test #'string=))
       (let ((acked (car (last (irc-message-params msg)))))
         (cloak-log "[CLoak] CAP ACK: ~a~%" acked)
         (setf (upstream-cap-enabled upstream)
               (split-sequence:split-sequence #\Space acked
                                              :remove-empty-subseqs t))
         ;; If SASL was acked, start authentication
         (if (member "sasl" (upstream-cap-enabled upstream) :test #'string-equal)
             (upstream--start-sasl upstream)
             (progn
               (upstream-send upstream "CAP END")
               (setf (upstream-cap-state upstream) :done))))
       t)
      ;; CAP NAK
      ((and (string= command "CAP")
            (member "NAK" (irc-message-params msg) :test #'string=))
       (cloak-log "[CLoak] CAP NAK: ~a~%" (car (last (irc-message-params msg))))
       (upstream-send upstream "CAP END")
       (setf (upstream-cap-state upstream) :done)
       t)
      ;; AUTHENTICATE response
      ((string= command "AUTHENTICATE")
       (upstream--handle-authenticate upstream msg)
       t)
      ;; SASL success (903)
      ((string= command "903")
       (cloak-log "[CLoak] SASL authentication successful~%")
       (upstream-send upstream "CAP END")
       (setf (upstream-cap-state upstream) :done)
       t)
      ;; SASL failure (902, 904, 905)
      ((member command '("902" "904" "905") :test #'string=)
       (cloak-log "[CLoak] SASL authentication failed: ~a~%"
               (car (last (irc-message-params msg))))
       (upstream-send upstream "CAP END")
       (setf (upstream-cap-state upstream) :done)
       t)
      ;; SASL logged in (900)
      ((string= command "900")
       (cloak-log "[CLoak] Logged in as ~a~%"
               (third (irc-message-params msg)))
       t)
      (t nil))))

(defun upstream--start-sasl (upstream)
  "Begin SASL PLAIN authentication."
  (let ((sasl-type (cloak.config:network-sasl (upstream-config upstream))))
    (if (eq sasl-type :plain)
        (progn
          (cloak-log "[CLoak] Starting SASL PLAIN~%")
          (upstream-send upstream "AUTHENTICATE PLAIN")
          (setf (upstream-cap-state upstream) :sasl-auth))
        ;; No SASL configured, just end CAP
        (progn
          (cloak-log "[CLoak] SASL not configured, ending CAP~%")
          (upstream-send upstream "CAP END")
          (setf (upstream-cap-state upstream) :done)))))

(defun upstream--handle-authenticate (upstream msg)
  "Handle AUTHENTICATE + from server, send credentials."
  (let ((param (first (irc-message-params msg))))
    (when (string= param "+")
      (let* ((config (upstream-config upstream))
             (nick (upstream-nick upstream))
             (password (or (cloak.config:network-password config) ""))
             ;; SASL PLAIN: \0nick\0password
             (payload (format nil "~a~c~a~c~a" nick #\Nul nick #\Nul password))
             (encoded (cl-base64:string-to-base64-string payload)))
        (upstream-send upstream (format nil "AUTHENTICATE ~a" encoded))))))

;;; --- Reconnect ---

(defun calculate-backoff (attempt &key (initial 2) (max 300) (jitter nil))
  "Calculate reconnect delay for ATTEMPT using exponential backoff.
Starts at INITIAL seconds, doubles each attempt, caps at MAX.
If JITTER is T, adds random jitter between base and 1.5x base."
  (let ((base (min max (* initial (expt 2 attempt)))))
    (if jitter
        (+ base (random (1+ (floor base 2))))
        base)))

(defun upstream--reconnect-loop (upstream)
  "Attempt to reconnect UPSTREAM with exponential backoff."
  (loop while (upstream-reconnect-p upstream)
        for attempt = (upstream-reconnect-attempts upstream)
        for delay = (calculate-backoff attempt :jitter t)
        do (cloak-log "[CLoak] Reconnecting ~a in ~d seconds (attempt ~d)~%"
                   (upstream-network-name upstream) delay (1+ attempt))
           (sleep delay)
           (when (upstream-reconnect-p upstream)
             (if (upstream-connect upstream)
                 ;; Connected - only reset attempts if connection stays stable
                 ;; (the next disconnect will check stability via last-activity)
                 (return)
                 (incf (upstream-reconnect-attempts upstream))))))

(defun upstream--track-state (upstream msg)
  "Update internal state from MSG (nick changes, channel tracking, etc.)."
  (let ((command (irc-message-command msg)))
    (cond
      ;; Registration complete
      ((string= command "001")
       (setf (upstream-state upstream) :connected)
       ;; Notify state change
       (when (upstream-on-state-change upstream)
         (handler-case
             (funcall (upstream-on-state-change upstream) upstream :connected)
           (error (e)
             (cloak-log "[CLoak] State change handler error: ~a~%" e))))
       ;; Autojoin channels (staggered to avoid Excess Flood)
       (let ((channels (cloak.config:network-autojoin (upstream-config upstream))))
         (when channels
           (bt:make-thread
            (lambda ()
              (dolist (chan channels)
                (upstream-send upstream (irc-join chan))
                (sleep 1)))
            :name "cloak-autojoin"))))
      ;; Nick in use
      ((string= command "433")
       (let* ((config (upstream-config upstream))
              (desired (cloak.config:network-nick config))
              (alt (cloak.config:network-alt-nick config))
              (current (upstream-nick upstream))
              (new-nick (cond
                          ((string= current desired)
                           (or alt (format nil "~a_" desired)))
                          (t (format nil "~a_" current)))))
         (setf (upstream-nick upstream) new-nick)
         (upstream-send upstream (irc-nick new-nick))))
      ;; QUIT - remove nick from all channels + keepnick
      ((string= command "QUIT")
       (let* ((config (upstream-config upstream))
              (desired (cloak.config:network-nick config))
              (quitter (source-nick (irc-message-source msg))))
         ;; Remove from all channel nick lists
         (maphash (lambda (chan nicks)
                    (declare (ignore chan))
                    (remhash quitter nicks))
                  (upstream-channel-nicks upstream))
         ;; Keepnick: reclaim desired nick when holder quits
         (when (and (not (string-equal (upstream-nick upstream) desired))
                    (string-equal quitter desired))
           (upstream-send upstream (irc-nick desired)))))
      ;; NICK - update nick across all channels + track our own
      ((string= command "NICK")
       (let ((old-nick (source-nick (irc-message-source msg)))
             (new-nick (first (irc-message-params msg))))
         ;; Update in all channel nick lists
         (maphash (lambda (chan nicks)
                    (declare (ignore chan))
                    (let ((prefix (gethash old-nick nicks)))
                      (when prefix
                        (remhash old-nick nicks)
                        (setf (gethash new-nick nicks) prefix))))
                  (upstream-channel-nicks upstream))
         ;; Track our own nick
         (when (string-equal old-nick (upstream-nick upstream))
           (setf (upstream-nick upstream) new-nick))))
      ;; JOIN - track channel + add nick
      ((string= command "JOIN")
       (let ((chan (first (irc-message-params msg)))
             (nick (source-nick (irc-message-source msg))))
         ;; If it's us joining, register the channel
         (when (string-equal nick (upstream-nick upstream))
           (let ((key (second (irc-message-params msg))))
             (setf (gethash chan (upstream-channels upstream))
                   (or key t)))
           ;; Initialize nick set for this channel
           (setf (gethash chan (upstream-channel-nicks upstream))
                 (make-hash-table :test 'equal)))
         ;; Add the nick to the channel's nick set (no prefix for JOINs)
         (let ((nicks (gethash chan (upstream-channel-nicks upstream))))
           (when nicks
             (setf (gethash nick nicks) "")))))
      ;; PART - remove nick from channel
      ((string= command "PART")
       (let ((chan (first (irc-message-params msg)))
             (nick (source-nick (irc-message-source msg))))
         (if (string-equal nick (upstream-nick upstream))
             ;; We left - remove channel entirely
             (progn
               (remhash chan (upstream-channels upstream))
               (remhash chan (upstream-channel-nicks upstream)))
             ;; Someone else left - remove from nick set
             (let ((nicks (gethash chan (upstream-channel-nicks upstream))))
               (when nicks
                 (remhash nick nicks))))))
      ;; KICK - remove kicked nick from channel
      ((string= command "KICK")
       (let ((chan (first (irc-message-params msg)))
             (kicked (second (irc-message-params msg))))
         (if (string-equal kicked (upstream-nick upstream))
             ;; We were kicked - remove channel entirely
             (progn
               (remhash chan (upstream-channels upstream))
               (remhash chan (upstream-channel-nicks upstream)))
             ;; Someone else kicked - remove from nick set
             (let ((nicks (gethash chan (upstream-channel-nicks upstream))))
               (when nicks
                 (remhash kicked nicks))))))
      ;; MODE - track prefix changes (op/voice/etc.)
      ((string= command "MODE")
       (let* ((chan (first (irc-message-params msg)))
              (mode-str (second (irc-message-params msg)))
              (mode-args (cddr (irc-message-params msg)))
              (nicks (gethash chan (upstream-channel-nicks upstream))))
         (when (and nicks mode-str (plusp (length mode-str)))
           ;; Map mode letters to NAMES prefixes
           (let ((adding (char= (char mode-str 0) #\+))
                 (arg-idx 0))
             (loop for ch across mode-str
                   do (cond
                        ((char= ch #\+) (setf adding t))
                        ((char= ch #\-) (setf adding nil))
                        ;; Modes that take a nick argument and map to prefixes
                        ((member ch '(#\o #\v #\h #\a #\q))
                         (when (< arg-idx (length mode-args))
                           (let ((nick (nth arg-idx mode-args))
                                 (prefix (case ch
                                           (#\o "@") (#\v "+") (#\h "%")
                                           (#\a "&") (#\q "~"))))
                             (when (gethash nick nicks)
                               (setf (gethash nick nicks)
                                     (if adding prefix ""))))
                           (incf arg-idx)))
                        ;; Other modes with args (b, e, I, k, etc.) - skip arg
                        ((member ch '(#\b #\e #\I #\k #\l #\f #\j))
                         (incf arg-idx))))))))
      ;; 353 (RPL_NAMREPLY) - bulk populate nick list
      ((string= command "353")
       ;; Params: <nick> <chan-type> <channel> :<nick1> <nick2> ...
       (let* ((chan (third (irc-message-params msg)))
              (names-str (fourth (irc-message-params msg)))
              (nicks (gethash chan (upstream-channel-nicks upstream))))
         (when (and nicks names-str)
           (dolist (entry (split-sequence:split-sequence #\Space names-str
                                                         :remove-empty-subseqs t))
             ;; Extract mode prefix and bare nick
             (let* ((prefix-chars "@+%~&")
                    (prefix (if (and (plusp (length entry))
                                     (find (char entry 0) prefix-chars))
                                (string (char entry 0))
                                ""))
                    (nick (string-left-trim prefix-chars entry)))
               (when (plusp (length nick))
                 (setf (gethash nick nicks) prefix))))))))))

