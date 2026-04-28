;;;; downstream.lisp - Downstream client connections for CLoak
;;;; Accepts IRC client connections and handles authentication/routing.

(in-package #:cloak.downstream)

;;; --- Downstream Client ---

(defclass downstream-client ()
  ((socket :initarg :socket :accessor client-socket)
   (stream :initarg :stream :accessor client-stream)
   (nick :initform nil :accessor client-nick)
   (user :initform nil :accessor client-user)
   (authenticated-p :initform nil :accessor client-authenticated-p)
   (network :initform nil :accessor client-network
            :documentation "Which upstream network this client is attached to.")
   (last-playback :initform 0 :accessor client-last-playback
                  :documentation "Universal time of last buffer playback.")
   (read-thread :initform nil :accessor client-read-thread)
   (lock :initform (bt:make-lock "client-lock") :accessor client-lock)
   ;; Callbacks
   (message-handler :initarg :message-handler :accessor client-message-handler
                    :initform nil)
   (disconnect-handler :initarg :disconnect-handler :accessor client-disconnect-handler
                       :initform nil))
  (:documentation "An IRC client connected to the bouncer."))

;;; --- Client Send ---

(defun client-send (client raw-line)
  "Send RAW-LINE to CLIENT."
  (bt:with-lock-held ((client-lock client))
    (when (client-stream client)
      (handler-case
          (progn
            (write-string raw-line (client-stream client))
            (write-char #\Return (client-stream client))
            (write-char #\Newline (client-stream client))
            (force-output (client-stream client)))
        (error (e)
          (declare (ignore e))
          (client-disconnect client))))))

(defun client-disconnect (client)
  "Disconnect CLIENT cleanly."
  (when (client-stream client)
    (ignore-errors (close (client-stream client)))
    (setf (client-stream client) nil))
  (when (client-socket client)
    (ignore-errors (close (client-socket client)))
    (setf (client-socket client) nil))
  (when (client-disconnect-handler client)
    (funcall (client-disconnect-handler client) client)))

;;; --- Client Read Loop ---

(defun client--read-loop (client)
  "Read and dispatch messages from an IRC client."
  (handler-case
      (loop while (client-stream client)
            for line = (handler-case
                           (read-line (client-stream client) nil nil)
                         (error () nil))
            while line
            do (let ((trimmed (string-right-trim '(#\Return #\Newline) line)))
                 (when (plusp (length trimmed))
                   (client--handle-line client trimmed))))
    (error (e)
      (format t "[CLoak] Client read error: ~a~%" e)))
  (client-disconnect client))

(defun client--handle-line (client line)
  "Parse and handle a raw IRC LINE from a client."
  (let ((msg (parse-message line)))
    (cond
      ;; Pre-auth: collect PASS, NICK, USER
      ((not (client-authenticated-p client))
       (client--handle-registration client msg line))
      ;; Post-auth: forward to bouncer
      ((client-message-handler client)
       (funcall (client-message-handler client) client line msg)))))

(defun client--handle-registration (client msg line)
  "Handle registration messages (PASS/NICK/USER) from CLIENT."
  (let ((command (cloak.protocol:irc-message-command msg)))
    (cond
      ((string= command "PASS")
       ;; Store password for auth check; format: user/network:password
       (setf (slot-value client 'user)
             (first (cloak.protocol:irc-message-params msg))))
      ((string= command "NICK")
       (setf (client-nick client)
             (first (cloak.protocol:irc-message-params msg))))
      ((string= command "USER")
       ;; USER received - try to authenticate
       (when (client-message-handler client)
         (funcall (client-message-handler client) client line msg)))
      ((string= command "CAP")
       ;; Forward CAP negotiation
       (when (client-message-handler client)
         (funcall (client-message-handler client) client line msg)))
      ((string= command "QUIT")
       (client-disconnect client)))))

;;; --- Listener ---

(defvar *listener-socket* nil)
(defvar *listener-thread* nil)

(defun start-listener (host port &key tls-cert tls-key
                                      on-connect)
  "Start listening for IRC client connections on HOST:PORT.
ON-CONNECT is called with each new downstream-client."
  (format t "[CLoak] Starting listener on ~a:~d~a~%"
          host port (if tls-cert " (TLS)" ""))
  (let ((sock (iolib:make-socket :connect :passive
                                 :address-family :internet
                                 :type :stream
                                 :external-format '(:utf-8 :eol-style :crlf)
                                 :ipv6 nil)))
    (setf (iolib:socket-option sock :reuse-address) t)
    (iolib:bind-address sock
                        (if (string= host "0.0.0.0")
                            iolib:+ipv4-unspecified+
                            (iolib:lookup-hostname host))
                        :port port)
    (iolib:listen-on sock :backlog 5)
    (setf *listener-socket* sock)
    (setf *listener-thread*
          (bt:make-thread
           (lambda ()
             (listener--accept-loop *listener-socket*
                                    :tls-cert tls-cert
                                    :tls-key tls-key
                                    :on-connect on-connect))
           :name "cloak-listener"))))

(defun stop-listener ()
  "Stop the client listener."
  (when *listener-socket*
    (ignore-errors (close *listener-socket*))
    (setf *listener-socket* nil))
  (format t "[CLoak] Listener stopped~%"))

(defun listener--accept-loop (server-socket &key tls-cert tls-key on-connect)
  "Accept loop for incoming IRC client connections."
  (handler-case
      (loop while server-socket
            for client-sock = (handler-case
                                  (iolib:accept-connection server-socket :wait t)
                                (error (e)
                                  (format t "[CLoak] Accept wait error: ~a~%" e)
                                  (force-output)
                                  nil))
            when client-sock
              do (handler-case
                     (let* ((stream (if tls-cert
                                       (cl+ssl:make-ssl-server-stream
                                        client-sock
                                        :certificate tls-cert
                                        :key tls-key)
                                       client-sock))
                            (client (make-instance 'downstream-client
                                      :socket client-sock
                                      :stream stream)))
                       (format t "[CLoak] Client connected from ~a~%"
                               (iolib:remote-host client-sock))
                       (force-output)
                       ;; Start client read thread
                       (setf (client-read-thread client)
                             (bt:make-thread
                              (lambda () (client--read-loop client))
                              :name "cloak-client"))
                       ;; Notify bouncer
                       (when on-connect
                         (funcall on-connect client)))
                   (error (e)
                     (format t "[CLoak] Accept error: ~a~%" e)
                     (force-output)
                     (ignore-errors (close client-sock)))))
    (error (e)
      (format t "[CLoak] Listener error: ~a~%" e))))
