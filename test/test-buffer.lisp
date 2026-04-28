;;;; test/test-buffer.lisp - Buffer tests

(in-package #:cloak.test)

(in-suite :cloak-tests)

(test buffer-push-and-retrieve
  "Push messages and retrieve them."
  (let ((buf (make-message-buffer :capacity 5)))
    (buffer-push buf ":nick PRIVMSG #test :msg1")
    (buffer-push buf ":nick PRIVMSG #test :msg2")
    (is (= 2 (buffer-count buf)))
    (let ((msgs (buffer-messages-all buf)))
      (is (= 2 (length msgs)))
      (is (string= ":nick PRIVMSG #test :msg1" (stored-message-raw (first msgs))))
      (is (string= ":nick PRIVMSG #test :msg2" (stored-message-raw (second msgs)))))))

(test buffer-ring-overflow
  "Buffer drops oldest messages when full."
  (let ((buf (make-message-buffer :capacity 3)))
    (buffer-push buf "msg1")
    (buffer-push buf "msg2")
    (buffer-push buf "msg3")
    (buffer-push buf "msg4")
    (is (= 3 (buffer-count buf)))
    (let ((msgs (buffer-messages-all buf)))
      (is (string= "msg2" (stored-message-raw (first msgs))))
      (is (string= "msg4" (stored-message-raw (third msgs)))))))

(test buffer-clear
  "Clear empties the buffer."
  (let ((buf (make-message-buffer :capacity 10)))
    (buffer-push buf "test")
    (buffer-clear buf)
    (is (= 0 (buffer-count buf)))
    (is (null (buffer-messages-all buf)))))
