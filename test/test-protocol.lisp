;;;; test/test-protocol.lisp - Protocol parsing tests

(in-package #:cloak.test)

(in-suite :cloak-tests)

(test parse-simple-message
  "Parse a simple IRC message."
  (let ((msg (parse-message "PING :irc.libera.chat")))
    (is (string= "PING" (irc-message-command msg)))
    (is (equal '("irc.libera.chat") (irc-message-params msg)))
    (is (null (irc-message-source msg)))
    (is (null (irc-message-tags msg)))))

(test parse-privmsg
  "Parse a PRIVMSG with source."
  (let ((msg (parse-message ":nick!user@host PRIVMSG #channel :Hello world")))
    (is (string= "PRIVMSG" (irc-message-command msg)))
    (is (string= "nick!user@host" (irc-message-source msg)))
    (is (equal '("#channel" "Hello world") (irc-message-params msg)))))

(test parse-tagged-message
  "Parse an IRCv3 message with tags."
  (let ((msg (parse-message "@time=2026-04-28T12:00:00Z;msgid=abc123 :nick!user@host PRIVMSG #test :tagged")))
    (is (string= "PRIVMSG" (irc-message-command msg)))
    (is (string= "2026-04-28T12:00:00Z" (cdr (assoc "time" (irc-message-tags msg) :test #'string=))))
    (is (string= "abc123" (cdr (assoc "msgid" (irc-message-tags msg) :test #'string=))))))

(test format-message-roundtrip
  "Format a message back to string."
  (let* ((msg (make-irc-message :command "PRIVMSG"
                                 :params '("#test" "Hello world")))
         (line (format-message msg)))
    (is (string= "PRIVMSG #test :Hello world" line))))

(test parse-source-full
  "Parse nick!user@host source."
  (let ((src (parse-source "nick!user@host.com")))
    (is (string= "nick" (irc-source-nick src)))
    (is (string= "user" (irc-source-user src)))
    (is (string= "host.com" (irc-source-host src)))))

(test source-nick-extraction
  "Extract nick from source string."
  (is (string= "nick" (source-nick "nick!user@host")))
  (is (string= "server.name" (source-nick "server.name"))))
