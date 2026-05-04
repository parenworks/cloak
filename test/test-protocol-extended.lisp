;;;; test/test-protocol-extended.lisp - Extended protocol parsing tests
;;;; Covers edge cases, tag roundtrips, message constructors, and source parsing.

(in-package #:cloak.test)

(in-suite :cloak-tests)

;;; --- Parser Edge Cases ---

(test parse-empty-trailing
  "Parse message with empty trailing parameter."
  (let ((msg (parse-message ":server PRIVMSG #test :")))
    (is (string= "PRIVMSG" (irc-message-command msg)))
    (is (equal '("#test" "") (irc-message-params msg)))))

(test parse-multiple-spaces
  "Parser handles multiple spaces between parameters."
  (let ((msg (parse-message ":nick!u@h  PRIVMSG  #test  :hello")))
    (is (string= "PRIVMSG" (irc-message-command msg)))
    (is (string= "nick!u@h" (irc-message-source msg)))
    (is (equal '("#test" "hello") (irc-message-params msg)))))

(test parse-command-only
  "Parse a message with just a command."
  (let ((msg (parse-message "QUIT")))
    (is (string= "QUIT" (irc-message-command msg)))
    (is (null (irc-message-params msg)))
    (is (null (irc-message-source msg)))))

(test parse-numeric-command
  "Parse a numeric reply command."
  (let ((msg (parse-message ":irc.server.com 001 nick :Welcome to the network")))
    (is (string= "001" (irc-message-command msg)))
    (is (string= "irc.server.com" (irc-message-source msg)))
    (is (equal '("nick" "Welcome to the network") (irc-message-params msg)))))

(test parse-many-middle-params
  "Parse a message with many middle parameters (no trailing)."
  (let ((msg (parse-message ":server 005 nick CHANTYPES=#& PREFIX=(ov)@+ NETWORK=TestNet")))
    (is (string= "005" (irc-message-command msg)))
    (is (= 4 (length (irc-message-params msg))))
    (is (string= "nick" (first (irc-message-params msg))))))

(test parse-colon-in-middle-param
  "Trailing parameter starting with : is captured correctly."
  (let ((msg (parse-message ":nick PRIVMSG #test :this has :colons: in it")))
    (is (string= "this has :colons: in it"
                  (second (irc-message-params msg))))))

(test parse-case-insensitive-command
  "Commands are uppercased during parsing."
  (let ((msg (parse-message "privmsg #test :hello")))
    (is (string= "PRIVMSG" (irc-message-command msg)))))

(test parse-nick-only-source
  "Parse source with no user or host."
  (let ((msg (parse-message ":server.name NOTICE * :hello")))
    (is (string= "server.name" (irc-message-source msg)))))

;;; --- Tag Parsing and Roundtrip ---

(test parse-tag-without-value
  "Parse a tag with no value (key only)."
  (let ((msg (parse-message "@draft/label :nick PRIVMSG #test :hi")))
    (is (= 1 (length (irc-message-tags msg))))
    (is (string= "draft/label" (caar (irc-message-tags msg))))
    (is (null (cdar (irc-message-tags msg))))))

(test parse-multiple-tags
  "Parse multiple tags separated by semicolons."
  (let ((msg (parse-message "@time=2026-01-01T00:00:00Z;msgid=abc;label :nick PRIVMSG #test :hi")))
    (is (= 3 (length (irc-message-tags msg))))
    (is (string= "2026-01-01T00:00:00Z"
                  (cdr (assoc "time" (irc-message-tags msg) :test #'string=))))
    (is (string= "abc"
                  (cdr (assoc "msgid" (irc-message-tags msg) :test #'string=))))
    (is (null (cdr (assoc "label" (irc-message-tags msg) :test #'string=))))))

(test tag-escape-roundtrip
  "Tag escape and unescape are inverses."
  (let* ((original "hello; world\\ \r\n")
         (escaped (cloak.protocol::escape-tag-value original))
         (unescaped (cloak.protocol::unescape-tag-value escaped)))
    (is (string= original unescaped))))

(test tag-escape-special-chars
  "Escape encodes semicolons, spaces, backslashes, CR, LF."
  (let ((escaped (cloak.protocol::escape-tag-value "a;b c\\d")))
    (is (search "\\:" escaped))
    (is (search "\\s" escaped))
    (is (search "\\\\" escaped))))

(test format-tags-roundtrip
  "Format then parse tags produces the same alist."
  (let* ((tags '(("time" . "2026-01-01") ("msgid" . "xyz") ("label" . nil)))
         (formatted (cloak.protocol:format-tags tags))
         (parsed (cloak.protocol:parse-tags formatted)))
    (is (string= "2026-01-01"
                  (cdr (assoc "time" parsed :test #'string=))))
    (is (string= "xyz"
                  (cdr (assoc "msgid" parsed :test #'string=))))
    (is (null (cdr (assoc "label" parsed :test #'string=))))))

;;; --- Source Parsing Edge Cases ---

(test parse-source-nick-at-host
  "Parse source with nick@host but no user."
  (let ((src (cloak.protocol:parse-source "nick@host.com")))
    (is (string= "nick" (cloak.protocol:irc-source-nick src)))
    (is (string= "" (cloak.protocol:irc-source-user src)))
    (is (string= "host.com" (cloak.protocol:irc-source-host src)))))

(test parse-source-server-name
  "Server name (no ! or @) returns nick = whole string."
  (let ((src (cloak.protocol:parse-source "irc.libera.chat")))
    (is (string= "irc.libera.chat" (cloak.protocol:irc-source-nick src)))
    (is (string= "" (cloak.protocol:irc-source-user src)))
    (is (string= "" (cloak.protocol:irc-source-host src)))))

(test parse-source-nil
  "parse-source with NIL returns NIL."
  (is (null (cloak.protocol:parse-source nil))))

(test source-nick-nil
  "source-nick with NIL returns NIL."
  (is (null (cloak.protocol:source-nick nil))))

;;; --- Message Constructors ---

(test constructor-irc-pass
  "irc-pass produces PASS command."
  (let* ((line (cloak.protocol:irc-pass "secret"))
         (msg (parse-message line)))
    (is (string= "PASS" (irc-message-command msg)))
    (is (equal '("secret") (irc-message-params msg)))))

(test constructor-irc-nick
  "irc-nick produces NICK command."
  (let* ((line (cloak.protocol:irc-nick "testbot"))
         (msg (parse-message line)))
    (is (string= "NICK" (irc-message-command msg)))
    (is (equal '("testbot") (irc-message-params msg)))))

(test constructor-irc-user
  "irc-user produces USER command with 4 params."
  (let* ((line (cloak.protocol:irc-user "myuser" "My Real Name"))
         (msg (parse-message line)))
    (is (string= "USER" (irc-message-command msg)))
    (is (= 4 (length (irc-message-params msg))))
    (is (string= "myuser" (first (irc-message-params msg))))
    (is (string= "My Real Name" (fourth (irc-message-params msg))))))

(test constructor-irc-join
  "irc-join produces JOIN command."
  (let* ((line (cloak.protocol:irc-join "#test"))
         (msg (parse-message line)))
    (is (string= "JOIN" (irc-message-command msg)))
    (is (equal '("#test") (irc-message-params msg)))))

(test constructor-irc-join-with-key
  "irc-join with key produces JOIN with two params."
  (let* ((line (cloak.protocol:irc-join "#secret" "mykey"))
         (msg (parse-message line)))
    (is (string= "JOIN" (irc-message-command msg)))
    (is (equal '("#secret" "mykey") (irc-message-params msg)))))

(test constructor-irc-part
  "irc-part produces PART command."
  (let* ((line (cloak.protocol:irc-part "#test" "goodbye"))
         (msg (parse-message line)))
    (is (string= "PART" (irc-message-command msg)))
    (is (equal '("#test" "goodbye") (irc-message-params msg)))))

(test constructor-irc-quit
  "irc-quit with message produces QUIT command."
  (let* ((line (cloak.protocol:irc-quit "leaving"))
         (msg (parse-message line)))
    (is (string= "QUIT" (irc-message-command msg)))
    (is (equal '("leaving") (irc-message-params msg)))))

(test constructor-irc-quit-no-message
  "irc-quit without message produces bare QUIT."
  (let* ((line (cloak.protocol:irc-quit))
         (msg (parse-message line)))
    (is (string= "QUIT" (irc-message-command msg)))
    (is (null (irc-message-params msg)))))

(test constructor-irc-privmsg
  "irc-privmsg produces correct PRIVMSG."
  (let* ((line (cloak.protocol:irc-privmsg "#test" "hello world"))
         (msg (parse-message line)))
    (is (string= "PRIVMSG" (irc-message-command msg)))
    (is (string= "#test" (first (irc-message-params msg))))
    (is (string= "hello world" (second (irc-message-params msg))))))

(test constructor-irc-notice
  "irc-notice produces correct NOTICE."
  (let* ((line (cloak.protocol:irc-notice "nick" "hey there"))
         (msg (parse-message line)))
    (is (string= "NOTICE" (irc-message-command msg)))
    (is (string= "nick" (first (irc-message-params msg))))
    (is (string= "hey there" (second (irc-message-params msg))))))

(test constructor-irc-ping-pong
  "irc-ping and irc-pong roundtrip tokens."
  (let* ((ping-line (cloak.protocol:irc-ping "token123"))
         (ping-msg (parse-message ping-line))
         (pong-line (cloak.protocol:irc-pong "token123"))
         (pong-msg (parse-message pong-line)))
    (is (string= "PING" (irc-message-command ping-msg)))
    (is (string= "token123" (first (irc-message-params ping-msg))))
    (is (string= "PONG" (irc-message-command pong-msg)))
    (is (string= "token123" (first (irc-message-params pong-msg))))))

(test constructor-irc-cap
  "irc-cap produces CAP command."
  (let* ((line (cloak.protocol:irc-cap "LS" "302"))
         (msg (parse-message line)))
    (is (string= "CAP" (irc-message-command msg)))
    (is (string= "LS" (first (irc-message-params msg))))
    (is (string= "302" (second (irc-message-params msg))))))

(test constructor-irc-authenticate
  "irc-authenticate produces AUTHENTICATE command."
  (let* ((line (cloak.protocol:irc-authenticate "+"))
         (msg (parse-message line)))
    (is (string= "AUTHENTICATE" (irc-message-command msg)))
    (is (string= "+" (first (irc-message-params msg))))))

;;; --- Format / Parse Roundtrip ---

(test format-parse-roundtrip-simple
  "Format then parse a simple message preserves structure."
  (let* ((orig (make-irc-message :command "PRIVMSG"
                                  :source "nick!user@host"
                                  :params '("#test" "hello world")))
         (line (cloak.protocol:format-message orig))
         (parsed (parse-message line)))
    (is (string= "nick!user@host" (irc-message-source parsed)))
    (is (string= "PRIVMSG" (irc-message-command parsed)))
    (is (equal '("#test" "hello world") (irc-message-params parsed)))))

(test format-parse-roundtrip-tagged
  "Format then parse a tagged message preserves tags."
  (let* ((orig (make-irc-message :command "PRIVMSG"
                                  :tags '(("time" . "2026") ("msgid" . "abc"))
                                  :source "nick!u@h"
                                  :params '("#test" "hi")))
         (line (cloak.protocol:format-message orig))
         (parsed (parse-message line)))
    (is (string= "2026" (cdr (assoc "time" (irc-message-tags parsed)
                                     :test #'string=))))
    (is (string= "abc" (cdr (assoc "msgid" (irc-message-tags parsed)
                                    :test #'string=))))))

(test format-empty-trailing-param
  "Format preserves empty trailing parameter."
  (let* ((msg (make-irc-message :command "TOPIC"
                                 :params '("#test" "")))
         (line (cloak.protocol:format-message msg))
         (parsed (parse-message line)))
    (is (equal '("#test" "") (irc-message-params parsed)))))
