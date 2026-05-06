;;;; protocol.lisp - IRC message parsing and formatting for CLoak
;;;; Handles IRCv3 message tags, source prefix, commands, and parameters.

(in-package #:cloak.protocol)

;;; --- Thread-safe Logging ---

(defvar *log-lock* (bt:make-lock "cloak-log")
  "Global lock for log output to prevent interleaved lines.")

(defun cloak-log (format-string &rest args)
  "Thread-safe logging to *standard-output*."
  (bt:with-lock-held (*log-lock*)
    (apply #'format t format-string args)
    (force-output)))

;;; --- IRC Message Structure ---

(defstruct irc-message
  "A parsed IRC protocol message."
  (tags nil :type list)          ; alist of (key . value) tag pairs
  (source nil :type (or null string)) ; nick!user@host prefix
  (command "" :type string)      ; PRIVMSG, JOIN, 001, etc.
  (params nil :type list))       ; list of parameter strings

;;; --- Source Parsing ---

(defstruct (irc-source (:constructor make-irc-source (nick user host)))
  "Parsed nick!user@host source prefix."
  (nick "" :type string)
  (user "" :type string)
  (host "" :type string))

(defun parse-source (source-string)
  "Parse SOURCE-STRING (nick!user@host) into an irc-source struct."
  (when source-string
    (let ((bang (position #\! source-string))
          (at (position #\@ source-string)))
      (cond
        ((and bang at (< bang at))
         (make-irc-source (subseq source-string 0 bang)
                          (subseq source-string (1+ bang) at)
                          (subseq source-string (1+ at))))
        (at
         (make-irc-source (subseq source-string 0 at)
                          ""
                          (subseq source-string (1+ at))))
        (t
         (make-irc-source source-string "" ""))))))

(defun source-nick (source-string)
  "Extract just the nick from a SOURCE-STRING."
  (when source-string
    (let ((bang (position #\! source-string)))
      (if bang
          (subseq source-string 0 bang)
          source-string))))

;;; --- Tag Parsing (IRCv3) ---

(defun parse-tags (tag-string)
  "Parse TAG-STRING into an alist of (key . value) pairs.
Tags are semicolon-separated key=value pairs."
  (when (and tag-string (plusp (length tag-string)))
    (loop for tag in (split-sequence:split-sequence #\; tag-string)
          collect (let ((eq-pos (position #\= tag)))
                    (if eq-pos
                        (cons (subseq tag 0 eq-pos)
                              (unescape-tag-value (subseq tag (1+ eq-pos))))
                        (cons tag nil))))))

(defun unescape-tag-value (value)
  "Unescape IRCv3 tag VALUE.
\\: -> ; \\s -> space \\r -> CR \\n -> LF \\\\ -> \\"
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length value))
          do (if (and (char= (char value i) #\\)
                      (< (1+ i) (length value)))
                 (progn
                   (case (char value (1+ i))
                     (#\: (write-char #\; out))
                     (#\s (write-char #\Space out))
                     (#\r (write-char #\Return out))
                     (#\n (write-char #\Newline out))
                     (#\\ (write-char #\\ out))
                     (t (write-char (char value (1+ i)) out)))
                   (incf i 2))
                 (progn
                   (write-char (char value i) out)
                   (incf i))))))

(defun format-tags (tags)
  "Format TAGS alist back to IRC tag string."
  (when tags
    (format nil "~{~a~^;~}"
            (mapcar (lambda (pair)
                      (if (cdr pair)
                          (format nil "~a=~a" (car pair)
                                  (escape-tag-value (cdr pair)))
                          (car pair)))
                    tags))))

(defun escape-tag-value (value)
  "Escape VALUE for use in an IRC tag."
  (with-output-to-string (out)
    (loop for c across value
          do (case c
               (#\; (write-string "\\:" out))
               (#\Space (write-string "\\s" out))
               (#\Return (write-string "\\r" out))
               (#\Newline (write-string "\\n" out))
               (#\\ (write-string "\\\\" out))
               (t (write-char c out))))))

;;; --- Message Parsing ---

(defun parse-message (line)
  "Parse a raw IRC LINE into an irc-message struct.
Handles IRCv3 tags, source prefix, command, and parameters."
  (let ((pos 0)
        (len (length line))
        tags source command params)
    ;; Parse tags (@key=value;key2=value2)
    (when (and (< pos len) (char= (char line pos) #\@))
      (let ((space (position #\Space line :start (1+ pos))))
        (when space
          (setf tags (parse-tags (subseq line (1+ pos) space)))
          (setf pos (1+ space)))))
    ;; Skip whitespace
    (loop while (and (< pos len) (char= (char line pos) #\Space))
          do (incf pos))
    ;; Parse source (:nick!user@host)
    (when (and (< pos len) (char= (char line pos) #\:))
      (let ((space (position #\Space line :start (1+ pos))))
        (when space
          (setf source (subseq line (1+ pos) space))
          (setf pos (1+ space)))))
    ;; Skip whitespace
    (loop while (and (< pos len) (char= (char line pos) #\Space))
          do (incf pos))
    ;; Parse command
    (let ((space (position #\Space line :start pos)))
      (setf command (if space
                        (subseq line pos space)
                        (subseq line pos)))
      (setf pos (if space (1+ space) len)))
    ;; Skip whitespace
    (loop while (and (< pos len) (char= (char line pos) #\Space))
          do (incf pos))
    ;; Parse parameters
    (loop while (< pos len)
          do (if (char= (char line pos) #\:)
                 ;; Trailing parameter (rest of line)
                 (progn
                   (push (subseq line (1+ pos)) params)
                   (setf pos len))
                 ;; Middle parameter
                 (let ((space (position #\Space line :start pos)))
                   (push (subseq line pos (or space len)) params)
                   (setf pos (if space (1+ space) len))))
             ;; Skip whitespace between params
             (loop while (and (< pos len) (char= (char line pos) #\Space))
                   do (incf pos)))
    (make-irc-message :tags tags
                      :source source
                      :command (string-upcase command)
                      :params (nreverse params))))

;;; --- Message Formatting ---

(defun format-message (msg)
  "Format an irc-message MSG back to a raw IRC line string."
  (with-output-to-string (out)
    ;; Tags
    (when (irc-message-tags msg)
      (write-char #\@ out)
      (write-string (format-tags (irc-message-tags msg)) out)
      (write-char #\Space out))
    ;; Source
    (when (irc-message-source msg)
      (write-char #\: out)
      (write-string (irc-message-source msg) out)
      (write-char #\Space out))
    ;; Command
    (write-string (irc-message-command msg) out)
    ;; Params
    (let ((params (irc-message-params msg)))
      (when params
        (loop for (param . rest) on params
              do (write-char #\Space out)
                 (if (and (null rest)
                          (or (find #\Space param)
                              (and (plusp (length param))
                                   (char= (char param 0) #\:))
                              (zerop (length param))))
                     (progn (write-char #\: out)
                            (write-string param out))
                     (write-string param out)))))))

;;; --- Common Message Constructors ---

(defun irc-pass (password)
  (format-message (make-irc-message :command "PASS" :params (list password))))

(defun irc-nick (nick)
  (format-message (make-irc-message :command "NICK" :params (list nick))))

(defun irc-user (username realname)
  (format-message (make-irc-message :command "USER"
                                     :params (list username "0" "*" realname))))

(defun irc-join (channel &optional key)
  (format-message (make-irc-message :command "JOIN"
                                     :params (if key (list channel key) (list channel)))))

(defun irc-part (channel &optional message)
  (format-message (make-irc-message :command "PART"
                                     :params (if message (list channel message) (list channel)))))

(defun irc-quit (&optional message)
  (format-message (make-irc-message :command "QUIT"
                                     :params (when message (list message)))))

(defun irc-privmsg (target text)
  (format-message (make-irc-message :command "PRIVMSG"
                                     :params (list target text))))

(defun irc-notice (target text)
  (format-message (make-irc-message :command "NOTICE"
                                     :params (list target text))))

(defun irc-ping (token)
  (format-message (make-irc-message :command "PING" :params (list token))))

(defun irc-pong (token)
  (format-message (make-irc-message :command "PONG" :params (list token))))

(defun irc-cap (subcommand &rest args)
  (format-message (make-irc-message :command "CAP"
                                     :params (cons subcommand args))))

(defun irc-authenticate (data)
  (format-message (make-irc-message :command "AUTHENTICATE" :params (list data))))
