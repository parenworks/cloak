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
   (cap-enabled :initform nil :accessor upstream-cap-enabled)
   (server-name :initform nil :accessor upstream-server-name)
   ;; Health
   (last-activity :initform (get-universal-time) :accessor upstream-last-activity)
   (ping-pending :initform nil :accessor upstream-ping-pending)
   ;; Threading
   (read-thread :initform nil :accessor upstream-read-thread)
   (lock :initform (bt:make-lock "upstream-lock") :accessor upstream-lock)
   ;; Callbacks
   (message-handler :initarg :message-handler :accessor upstream-message-handler
                    :initform nil)
   ;; Config
   (config :initarg :config :accessor upstream-config))
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
  (let* ((config (upstream-config upstream))
         (server (cloak.config:network-server config))
         (port (cloak.config:network-port config))
         (use-tls (cloak.config:network-tls config)))
    (handler-case
        (progn
          (format t "~&[CLoak] Connecting to ~a:~d~a...~%"
                  server port (if use-tls " (TLS)" ""))
          (let ((sock (usocket:socket-connect server port
                                              :element-type '(unsigned-byte 8))))
            (setf (upstream-socket upstream) sock)
            (let ((raw-stream (usocket:socket-stream sock)))
              (setf (upstream-stream upstream)
                    (if use-tls
                        (cl+ssl:make-ssl-client-stream
                         raw-stream
                         :hostname server
                         :external-format :utf-8)
                        (flexi-streams:make-flexi-stream
                         raw-stream
                         :external-format :utf-8)))
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
              (format t "[CLoak] Connected to ~a~%" (upstream-network-name upstream))
              t)))
      (error (e)
        (format t "[CLoak] Failed to connect to ~a: ~a~%"
                (upstream-network-name upstream) e)
        (upstream-disconnect upstream)
        nil))))

(defun upstream-disconnect (upstream)
  "Disconnect UPSTREAM from the IRC server."
  (bt:with-lock-held ((upstream-lock upstream))
    (setf (upstream-state upstream) :disconnected)
    (when (upstream-stream upstream)
      (ignore-errors (close (upstream-stream upstream)))
      (setf (upstream-stream upstream) nil))
    (when (upstream-socket upstream)
      (ignore-errors (usocket:socket-close (upstream-socket upstream)))
      (setf (upstream-socket upstream) nil))))

(defun upstream-connected-p (upstream)
  "Return T if UPSTREAM is connected."
  (eq (upstream-state upstream) :connected))

;;; --- Send ---

(defun upstream-send (upstream raw-line)
  "Send RAW-LINE to the IRC server via UPSTREAM."
  (bt:with-lock-held ((upstream-lock upstream))
    (when (upstream-stream upstream)
      (handler-case
          (progn
            (write-string raw-line (upstream-stream upstream))
            (write-string (string #\Return) (upstream-stream upstream))
            (write-string (string #\Newline) (upstream-stream upstream))
            (force-output (upstream-stream upstream)))
        (error (e)
          (format t "[CLoak] Send error on ~a: ~a~%"
                  (upstream-network-name upstream) e)
          (upstream-disconnect upstream))))))

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
      (format t "[CLoak] Read error on ~a: ~a~%"
              (upstream-network-name upstream) e)))
  ;; Cleanup
  (upstream-disconnect upstream)
  (format t "[CLoak] Upstream ~a disconnected~%" (upstream-network-name upstream)))

(defun upstream--handle-line (upstream line)
  "Parse and handle a raw IRC LINE from the server."
  (let ((msg (parse-message line)))
    ;; Handle PING internally
    (when (string= (irc-message-command msg) "PING")
      (upstream-send upstream (irc-pong (or (first (irc-message-params msg)) "")))
      (return-from upstream--handle-line))
    ;; Track state changes
    (upstream--track-state upstream msg)
    ;; Forward to bouncer via callback
    (when (upstream-message-handler upstream)
      (funcall (upstream-message-handler upstream) upstream line msg))))

(defun upstream--track-state (upstream msg)
  "Update internal state from MSG (nick changes, channel tracking, etc.)."
  (let ((command (irc-message-command msg)))
    (cond
      ;; Registration complete
      ((string= command "001")
       (setf (upstream-state upstream) :connected)
       ;; Autojoin channels
       (dolist (chan (cloak.config:network-autojoin (upstream-config upstream)))
         (upstream-send upstream (irc-join chan))))
      ;; Nick change
      ((string= command "433") ; Nick in use
       (let ((new-nick (format nil "~a_" (upstream-nick upstream))))
         (setf (upstream-nick upstream) new-nick)
         (upstream-send upstream (irc-nick new-nick))))
      ;; Track our nick
      ((and (string= command "NICK")
            (string-equal (source-nick (irc-message-source msg))
                          (upstream-nick upstream)))
       (setf (upstream-nick upstream) (first (irc-message-params msg))))
      ;; Track channels
      ((string= command "JOIN")
       (when (string-equal (source-nick (irc-message-source msg))
                           (upstream-nick upstream))
         (let ((chan (first (irc-message-params msg))))
           (setf (gethash chan (upstream-channels upstream)) t))))
      ((string= command "PART")
       (when (string-equal (source-nick (irc-message-source msg))
                           (upstream-nick upstream))
         (remhash (first (irc-message-params msg))
                  (upstream-channels upstream))))
      ((string= command "KICK")
       (when (string-equal (second (irc-message-params msg))
                           (upstream-nick upstream))
         (remhash (first (irc-message-params msg))
                  (upstream-channels upstream)))))))
