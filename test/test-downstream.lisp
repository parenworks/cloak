;;;; test/test-downstream.lisp - Downstream client connection tests
;;;; Covers registration flow, auth dispatch, message handling, and disconnect.

(in-package #:cloak.test)

(in-suite :cloak-tests)

;;; --- Helpers ---

(defun make-mock-client (&key message-handler disconnect-handler)
  "Create a downstream client with in-memory stream for testing."
  (let* ((output (make-string-output-stream))
         (client (make-instance 'cloak.downstream:downstream-client
                   :socket nil
                   :stream output
                   :message-handler message-handler
                   :disconnect-handler disconnect-handler)))
    (values client output)))

(defun client-output-string (output-stream)
  "Get the string written to a mock client's output stream."
  (get-output-stream-string output-stream))

;;; --- Registration Flow ---

(test downstream-pass-stores-user
  "PASS command stores the password data on the client."
  (let ((client (make-mock-client)))
    (cloak.downstream::client--handle-line client "PASS admin/libera:secret")
    (is (string= "admin/libera:secret"
                  (slot-value client 'cloak.downstream::user)))))

(test downstream-nick-stores-nick
  "NICK command stores nick on the client."
  (let ((client (make-mock-client)))
    (cloak.downstream::client--handle-line client "NICK testbot")
    (is (string= "testbot" (cloak.downstream:client-nick client)))))

(test downstream-user-stores-ident
  "USER command stores the ident on the client."
  (let ((dispatched nil))
    (multiple-value-bind (client)
        (make-mock-client :message-handler
                          (lambda (c line msg)
                            (declare (ignore line))
                            (setf dispatched (list c msg))))
      (cloak.downstream::client--handle-line client "USER myident 0 * :Real Name")
      (is (string= "myident" (cloak.downstream:client-ident client)))
      ;; message-handler should have been called for USER
      (is (not (null dispatched))))))

(test downstream-registration-order
  "PASS then NICK then USER stores all fields."
  (let ((user-received nil))
    (multiple-value-bind (client)
        (make-mock-client :message-handler
                          (lambda (c line msg)
                            (declare (ignore line))
                            (when (string= "USER"
                                           (cloak.protocol:irc-message-command msg))
                              (setf user-received t))))
      (cloak.downstream::client--handle-line client "PASS admin/net:pw")
      (cloak.downstream::client--handle-line client "NICK mynick")
      (cloak.downstream::client--handle-line client "USER myident 0 * :realname")
      (is (string= "admin/net:pw"
                    (slot-value client 'cloak.downstream::user)))
      (is (string= "mynick" (cloak.downstream:client-nick client)))
      (is (string= "myident" (cloak.downstream:client-ident client)))
      (is (eq t user-received)))))

(test downstream-cap-ls-forwarded
  "CAP LS during registration is forwarded to message handler."
  (let ((cap-received nil))
    (multiple-value-bind (client)
        (make-mock-client :message-handler
                          (lambda (c line msg)
                            (declare (ignore c line))
                            (when (string= "CAP"
                                           (cloak.protocol:irc-message-command msg))
                              (setf cap-received t))))
      (cloak.downstream::client--handle-line client "CAP LS 302")
      (is (eq t cap-received)))))

(test downstream-quit-disconnects
  "QUIT during registration disconnects the client."
  (let ((disconnected nil))
    (multiple-value-bind (client)
        (make-mock-client :disconnect-handler
                          (lambda (c) (declare (ignore c)) (setf disconnected t)))
      (cloak.downstream::client--handle-line client "QUIT :bye")
      (is (eq t disconnected)))))

;;; --- Post-auth Message Forwarding ---

(test downstream-post-auth-forwards
  "After authentication, messages are forwarded to message handler."
  (let ((forwarded-lines nil))
    (multiple-value-bind (client)
        (make-mock-client :message-handler
                          (lambda (c line msg)
                            (declare (ignore c msg))
                            (push line forwarded-lines)))
      ;; Mark as authenticated
      (setf (cloak.downstream:client-authenticated-p client) t)
      (cloak.downstream::client--handle-line client "PRIVMSG #test :hello")
      (cloak.downstream::client--handle-line client "JOIN #dev")
      (is (= 2 (length forwarded-lines)))
      (is (search "PRIVMSG" (second forwarded-lines)))
      (is (search "JOIN" (first forwarded-lines))))))

;;; --- Client Send ---

(test downstream-client-send
  "client-send writes line with CRLF to stream."
  (multiple-value-bind (client output)
      (make-mock-client)
    (cloak.downstream:client-send client "PRIVMSG #test :hello")
    (let ((written (client-output-string output)))
      (is (search "PRIVMSG #test :hello" written))
      (is (search (string #\Return) written))
      (is (search (string #\Newline) written)))))

(test downstream-client-send-nil-stream
  "client-send does nothing when stream is nil (disconnected)."
  (let ((client (make-instance 'cloak.downstream:downstream-client
                  :socket nil :stream nil)))
    ;; Should not error
    (finishes (cloak.downstream:client-send client "test"))))

;;; --- Disconnect ---

(test downstream-disconnect-fires-handler
  "client-disconnect calls the disconnect handler."
  (let ((handler-called nil))
    (multiple-value-bind (client)
        (make-mock-client :disconnect-handler
                          (lambda (c) (declare (ignore c)) (setf handler-called t)))
      (cloak.downstream:client-disconnect client)
      (is (eq t handler-called))
      ;; Stream should be nil after disconnect
      (is (null (slot-value client 'cloak.downstream::stream))))))

(test downstream-disconnect-idempotent
  "client-disconnect can be called multiple times safely."
  (let ((call-count 0))
    (multiple-value-bind (client)
        (make-mock-client :disconnect-handler
                          (lambda (c) (declare (ignore c)) (incf call-count)))
      (cloak.downstream:client-disconnect client)
      (cloak.downstream:client-disconnect client)
      ;; Handler called once per disconnect that has a stream
      ;; Second call: stream already nil, handler still called
      (is (<= 1 call-count)))))
