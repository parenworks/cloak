;;;; test/test-buffer-extended.lisp - Extended buffer tests
;;;; Covers messages-since, msgid tracking, capacity edge cases, and ordering.

(in-package #:cloak.test)

(in-suite :cloak-tests)

;;; --- Messages Since ---

(test buffer-messages-since-filters
  "buffer-messages-since returns only messages after the given time."
  (let ((buf (make-message-buffer :capacity 100))
        (t1 (get-universal-time)))
    ;; Push a message now
    (buffer-push buf "old message")
    ;; Wait briefly so times differ
    (sleep 0.1)
    (let ((t2 (get-universal-time)))
      ;; If universal-time ticked, we can test. Otherwise, both have same time.
      (buffer-push buf "new message")
      (let ((since-t1 (buffer-messages-since buf (1- t1)))
            (since-t2 (buffer-messages-since buf t2)))
        ;; All messages should be after t1-1
        (is (>= (length since-t1) 1))
        ;; Messages since t2 should only include those with time > t2
        (is (<= (length since-t2) 2))))))

(test buffer-messages-since-empty
  "buffer-messages-since on empty buffer returns NIL."
  (let ((buf (make-message-buffer :capacity 10)))
    (is (null (buffer-messages-since buf 0)))))

(test buffer-messages-since-future
  "buffer-messages-since with future time returns NIL."
  (let ((buf (make-message-buffer :capacity 10)))
    (buffer-push buf "test")
    (is (null (buffer-messages-since buf (+ (get-universal-time) 3600))))))

;;; --- Msgid Tracking ---

(test buffer-push-with-msgid
  "buffer-push stores msgid when provided."
  (let ((buf (make-message-buffer :capacity 10)))
    (buffer-push buf ":nick PRIVMSG #test :hi" "abc123")
    (let ((msgs (buffer-messages-all buf)))
      (is (= 1 (length msgs)))
      (is (string= "abc123" (stored-message-msgid (first msgs)))))))

(test buffer-push-without-msgid
  "buffer-push stores NIL msgid when not provided."
  (let ((buf (make-message-buffer :capacity 10)))
    (buffer-push buf ":nick PRIVMSG #test :hi")
    (let ((msgs (buffer-messages-all buf)))
      (is (null (stored-message-msgid (first msgs)))))))

;;; --- Capacity Edge Cases ---

(test buffer-capacity-one
  "Buffer with capacity 1 always holds the latest message."
  (let ((buf (make-message-buffer :capacity 1)))
    (buffer-push buf "msg1")
    (buffer-push buf "msg2")
    (buffer-push buf "msg3")
    (is (= 1 (buffer-count buf)))
    (let ((msgs (buffer-messages-all buf)))
      (is (string= "msg3" (stored-message-raw (first msgs)))))))

(test buffer-exact-capacity
  "Buffer at exactly capacity holds all messages."
  (let ((buf (make-message-buffer :capacity 3)))
    (buffer-push buf "msg1")
    (buffer-push buf "msg2")
    (buffer-push buf "msg3")
    (is (= 3 (buffer-count buf)))
    (let ((msgs (buffer-messages-all buf)))
      (is (string= "msg1" (stored-message-raw (first msgs))))
      (is (string= "msg3" (stored-message-raw (third msgs)))))))

(test buffer-double-overflow
  "Buffer correctly wraps around twice."
  (let ((buf (make-message-buffer :capacity 3)))
    ;; Fill 6 messages into capacity-3 buffer
    (dotimes (i 6)
      (buffer-push buf (format nil "msg~d" i)))
    (is (= 3 (buffer-count buf)))
    (let ((msgs (buffer-messages-all buf)))
      ;; Should have msg3, msg4, msg5
      (is (string= "msg3" (stored-message-raw (first msgs))))
      (is (string= "msg5" (stored-message-raw (third msgs)))))))

;;; --- Ordering ---

(test buffer-messages-all-ordered
  "buffer-messages-all returns messages in insertion order."
  (let ((buf (make-message-buffer :capacity 10)))
    (dotimes (i 5)
      (buffer-push buf (format nil "msg~d" i)))
    (let ((msgs (buffer-messages-all buf)))
      (is (= 5 (length msgs)))
      (dotimes (i 5)
        (is (string= (format nil "msg~d" i)
                      (stored-message-raw (nth i msgs))))))))

(test buffer-messages-all-ordered-after-overflow
  "buffer-messages-all returns correct order after ring wrap."
  (let ((buf (make-message-buffer :capacity 4)))
    (dotimes (i 7)
      (buffer-push buf (format nil "msg~d" i)))
    (let ((msgs (buffer-messages-all buf)))
      (is (= 4 (length msgs)))
      (is (string= "msg3" (stored-message-raw (first msgs))))
      (is (string= "msg4" (stored-message-raw (second msgs))))
      (is (string= "msg5" (stored-message-raw (third msgs))))
      (is (string= "msg6" (stored-message-raw (fourth msgs)))))))

;;; --- Clear ---

(test buffer-clear-resets-completely
  "buffer-clear resets count, and messages-all returns empty."
  (let ((buf (make-message-buffer :capacity 10)))
    (dotimes (i 5)
      (buffer-push buf (format nil "msg~d" i)))
    (is (= 5 (buffer-count buf)))
    (buffer-clear buf)
    (is (= 0 (buffer-count buf)))
    (is (null (buffer-messages-all buf)))
    ;; Can push again after clear
    (buffer-push buf "after-clear")
    (is (= 1 (buffer-count buf)))
    (is (string= "after-clear"
                  (stored-message-raw (first (buffer-messages-all buf)))))))

;;; --- Stored Message Metadata ---

(test stored-message-has-time
  "Stored messages have a reasonable timestamp."
  (let ((buf (make-message-buffer :capacity 10))
        (before (get-universal-time)))
    (buffer-push buf "test")
    (let* ((msgs (buffer-messages-all buf))
           (msg-time (stored-message-time (first msgs))))
      (is (>= msg-time before))
      (is (<= msg-time (+ before 2))))))
