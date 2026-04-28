;;;; buffer.lisp - Message buffering for CLoak
;;;; Ring buffer that stores raw IRC messages for client playback.

(in-package #:cloak.buffer)

;;; --- Stored Message ---

(defstruct stored-message
  "A buffered IRC message with metadata."
  (time (get-universal-time) :type integer)
  (raw "" :type string)            ; raw IRC line as received
  (msgid nil :type (or null string))) ; IRCv3 msgid tag if present

;;; --- Message Buffer (ring buffer) ---

(defclass message-buffer ()
  ((messages :initform (make-array 500 :initial-element nil)
             :accessor buffer-messages)
   (capacity :initarg :capacity :initform 500
             :accessor buffer-capacity)
   (head :initform 0 :accessor buffer-head)
   (count :initform 0 :accessor buffer-count)
   (lock :initform (bt:make-lock "buffer-lock")
         :accessor buffer-lock))
  (:documentation "Thread-safe ring buffer for IRC message storage."))

(defun make-message-buffer (&key (capacity 500))
  "Create a new message buffer with CAPACITY slots."
  (make-instance 'message-buffer
    :capacity capacity))

;;; --- Buffer Operations ---

(defun buffer-push (buffer raw-line &optional msgid)
  "Push RAW-LINE into BUFFER. Oldest message dropped when full."
  (bt:with-lock-held ((buffer-lock buffer))
    (let ((msg (make-stored-message :raw raw-line :msgid msgid))
          (idx (mod (+ (buffer-head buffer) (buffer-count buffer))
                    (buffer-capacity buffer))))
      (setf (aref (buffer-messages buffer) idx) msg)
      (if (< (buffer-count buffer) (buffer-capacity buffer))
          (incf (buffer-count buffer))
          ;; Buffer full, advance head (drop oldest)
          (setf (buffer-head buffer)
                (mod (1+ (buffer-head buffer))
                     (buffer-capacity buffer))))
      msg)))

(defun buffer-messages-all (buffer)
  "Return all messages in BUFFER, oldest first."
  (bt:with-lock-held ((buffer-lock buffer))
    (loop for i from 0 below (buffer-count buffer)
          for idx = (mod (+ (buffer-head buffer) i)
                         (buffer-capacity buffer))
          collect (aref (buffer-messages buffer) idx))))

(defun buffer-messages-since (buffer universal-time)
  "Return messages in BUFFER since UNIVERSAL-TIME, oldest first."
  (bt:with-lock-held ((buffer-lock buffer))
    (loop for i from 0 below (buffer-count buffer)
          for idx = (mod (+ (buffer-head buffer) i)
                         (buffer-capacity buffer))
          for msg = (aref (buffer-messages buffer) idx)
          when (> (stored-message-time msg) universal-time)
            collect msg)))

(defun buffer-clear (buffer)
  "Clear all messages from BUFFER."
  (bt:with-lock-held ((buffer-lock buffer))
    (fill (buffer-messages buffer) nil)
    (setf (buffer-head buffer) 0
          (buffer-count buffer) 0)))
