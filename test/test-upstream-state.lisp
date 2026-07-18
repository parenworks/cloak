;;;; test/test-upstream-state.lisp - Upstream connection state tests
;;;; Covers CAP handling, registration state, 433 nick-in-use, state transitions,
;;;; keepnick on QUIT, and internal message handling.

(in-package #:cloak.test)

(in-suite :cloak-tests)

;;; --- Helpers ---

(defun make-state-upstream (&key (nick "testbot") (alt-nick nil) (sasl nil)
                                  (sasl-account nil) (password nil))
  "Create an upstream for state testing (not connected to a real server)."
  (let* ((net-cfg (make-instance 'cloak.config:network-config
                    :name "testnet" :server "irc.test" :port 6667
                    :tls nil :nick nick :alt-nick alt-nick
                    :sasl sasl :sasl-account sasl-account :password password
                    :autojoin '("#test")))
         (upstream (cloak.upstream:make-upstream net-cfg)))
    (setf (cloak.upstream:upstream-state upstream) :registering)
    upstream))

(defun feed-state (upstream raw-line)
  "Parse RAW-LINE and feed to upstream--track-state."
  (let ((msg (cloak.protocol:parse-message raw-line)))
    (cloak.upstream::upstream--track-state upstream msg)))

;;; --- Registration and State Transitions ---

(test upstream-initial-state
  "New upstream starts disconnected."
  (let ((net-cfg (make-instance 'cloak.config:network-config
                   :name "test" :server "irc.test" :port 6667
                   :tls nil :nick "bot")))
    (let ((up (cloak.upstream:make-upstream net-cfg)))
      (is (eq :disconnected (cloak.upstream:upstream-state up)))
      (is (not (cloak.upstream:upstream-connected-p up))))))

(test upstream-001-sets-connected
  "RPL_WELCOME (001) sets state to :connected."
  (let ((up (make-state-upstream)))
    (is (eq :registering (cloak.upstream:upstream-state up)))
    (feed-state up ":server 001 testbot :Welcome")
    (is (eq :connected (cloak.upstream:upstream-state up)))
    (is (cloak.upstream:upstream-connected-p up))))

(test upstream-001-fires-state-change
  "RPL_WELCOME fires the on-state-change callback."
  (let ((up (make-state-upstream))
        (callback-state nil))
    (setf (cloak.upstream:upstream-on-state-change up)
          (lambda (upstream new-state)
            (declare (ignore upstream))
            (setf callback-state new-state)))
    (feed-state up ":server 001 testbot :Welcome")
    (is (eq :connected callback-state))))

;;; --- Nick in Use (433) ---

(test upstream-433-uses-alt-nick
  "433 (nick in use) switches to alt nick when configured."
  (let ((up (make-state-upstream :nick "primary" :alt-nick "backup")))
    (setf (cloak.upstream:upstream-nick up) "primary")
    (feed-state up ":server 433 * primary :Nickname is already in use")
    (is (string= "backup" (cloak.upstream:upstream-nick up)))))

(test upstream-433-appends-underscore
  "433 appends underscore when no alt nick and already on primary."
  (let ((up (make-state-upstream :nick "testbot")))
    (setf (cloak.upstream:upstream-nick up) "testbot")
    (feed-state up ":server 433 * testbot :Nickname is already in use")
    (is (string= "testbot_" (cloak.upstream:upstream-nick up)))))

(test upstream-433-appends-underscore-repeatedly
  "Multiple 433s keep appending underscores."
  (let ((up (make-state-upstream :nick "testbot")))
    (setf (cloak.upstream:upstream-nick up) "testbot")
    (feed-state up ":server 433 * testbot :Nick in use")
    (is (string= "testbot_" (cloak.upstream:upstream-nick up)))
    (feed-state up ":server 433 * testbot_ :Nick in use")
    (is (string= "testbot__" (cloak.upstream:upstream-nick up)))))

;;; --- Keepnick on QUIT ---

(test upstream-keepnick-reclaims-on-quit
  "When holder of desired nick QUITs, upstream reclaims it."
  (let ((up (make-state-upstream :nick "desired")))
    ;; Simulate being forced to use alt nick
    (setf (cloak.upstream:upstream-nick up) "desired_")
    ;; Join channels so nick tracking works
    (feed-state up ":desired_!u@h JOIN #test")
    (feed-state up ":desired!other@h JOIN #test")
    ;; Desired nick holder quits
    (feed-state up ":desired!other@h QUIT :bye")
    ;; Nick should not change immediately (upstream-send would send NICK)
    ;; but the nick tracking should have removed "desired" from channels
    (let ((nicks (channel-nick-list up "#test")))
      (is (equal '("desired_") nicks)))))

;;; --- CAP Handling ---

(test upstream-cap-ls-parses-caps
  "CAP LS response is consumed by handle-cap."
  (let ((up (make-state-upstream)))
    (let ((msg (cloak.protocol:parse-message
                 ":server CAP * LS :sasl server-time echo-message")))
      (is (cloak.upstream::upstream--handle-cap up msg)))))

(test upstream-cap-ack-stores-enabled
  "CAP ACK stores enabled capabilities."
  (let ((up (make-state-upstream :sasl :plain :password "pass")))
    ;; Simulate ACK
    (let ((msg (cloak.protocol:parse-message
                 ":server CAP * ACK :sasl server-time")))
      (cloak.upstream::upstream--handle-cap up msg))
    (is (member "sasl" (cloak.upstream:upstream-cap-enabled up) :test #'string-equal))
    (is (member "server-time" (cloak.upstream:upstream-cap-enabled up) :test #'string-equal))))

(test upstream-cap-nak-ends-cap
  "CAP NAK ends CAP negotiation."
  (let ((up (make-state-upstream)))
    (let ((msg (cloak.protocol:parse-message
                 ":server CAP * NAK :sasl")))
      (cloak.upstream::upstream--handle-cap up msg))
    (is (eq :done (cloak.upstream::upstream-cap-state up)))))

(test upstream-sasl-uses-configured-account
  "SASL PLAIN authenticates with the configured services account."
  (let* ((up (make-state-upstream :nick "display-nick" :sasl :plain
                                  :sasl-account "services-account" :password "secret"))
         (output (make-string-output-stream))
         (payload (format nil "~cservices-account~csecret" #\Nul #\Nul))
         (expected (cl-base64:string-to-base64-string payload)))
    (setf (cloak.upstream::upstream-stream up) output)
    (cloak.upstream::upstream--handle-authenticate
     up (cloak.protocol:parse-message "AUTHENTICATE +"))
    (is (search (format nil "AUTHENTICATE ~a" expected)
                (get-output-stream-string output)))))

(test upstream-sasl-account-falls-back-to-nick
  "Existing configs without a SASL account continue to use the nick."
  (let* ((up (make-state-upstream :nick "display-nick" :sasl :plain
                                  :password "secret"))
         (output (make-string-output-stream))
         (payload (format nil "~cdisplay-nick~csecret" #\Nul #\Nul))
         (expected (cl-base64:string-to-base64-string payload)))
    (setf (cloak.upstream::upstream-stream up) output)
    (cloak.upstream::upstream--handle-authenticate
     up (cloak.protocol:parse-message "AUTHENTICATE +"))
    (is (search (format nil "AUTHENTICATE ~a" expected)
                (get-output-stream-string output)))))

(test upstream-sasl-903-ends-cap
  "SASL success (903) ends CAP negotiation."
  (let ((up (make-state-upstream :sasl :plain)))
    (let ((msg (cloak.protocol:parse-message
                 ":server 903 testbot :SASL authentication successful")))
      (cloak.upstream::upstream--handle-cap up msg))
    (is (eq :done (cloak.upstream::upstream-cap-state up)))))

(test upstream-sasl-904-ends-cap
  "SASL failure (904) ends CAP negotiation."
  (let ((up (make-state-upstream :sasl :plain)))
    (let ((msg (cloak.protocol:parse-message
                 ":server 904 testbot :SASL authentication failed")))
      (cloak.upstream::upstream--handle-cap up msg))
    (is (eq :done (cloak.upstream::upstream-cap-state up)))))

(test upstream-non-cap-not-consumed
  "Non-CAP messages are not consumed by handle-cap."
  (let ((up (make-state-upstream)))
    (let ((msg (cloak.protocol:parse-message ":nick!u@h PRIVMSG #test :hi")))
      (is (null (cloak.upstream::upstream--handle-cap up msg))))))

;;; --- State Tracking Extended ---

(test upstream-channel-tracking-multiple
  "Multiple channels are tracked independently."
  (let ((up (make-state-upstream)))
    (setf (cloak.upstream:upstream-state up) :connected)
    (feed-state up ":testbot!u@h JOIN #alpha")
    (feed-state up ":testbot!u@h JOIN #beta")
    (feed-state up ":alice!a@b JOIN #alpha")
    (feed-state up ":bob!b@c JOIN #beta")
    (is (equal '("alice" "testbot") (channel-nick-list up "#alpha")))
    (is (equal '("bob" "testbot") (channel-nick-list up "#beta")))
    ;; Part one channel
    (feed-state up ":testbot!u@h PART #alpha")
    (is (null (gethash "#alpha" (cloak.upstream:upstream-channels up))))
    ;; Other channel unaffected
    (is (equal '("bob" "testbot") (channel-nick-list up "#beta")))))

(test upstream-nick-change-updates-all-channels
  "NICK change updates nick in all channels."
  (let ((up (make-state-upstream)))
    (setf (cloak.upstream:upstream-state up) :connected)
    (feed-state up ":testbot!u@h JOIN #a")
    (feed-state up ":testbot!u@h JOIN #b")
    (feed-state up ":alice!a@b JOIN #a")
    (feed-state up ":alice!a@b JOIN #b")
    (feed-state up ":alice!a@b NICK alice2")
    (is (equal '("alice2" "testbot") (channel-nick-list up "#a")))
    (is (equal '("alice2" "testbot") (channel-nick-list up "#b")))))

(test upstream-quit-removes-from-all-channels
  "QUIT removes user from every channel."
  (let ((up (make-state-upstream)))
    (setf (cloak.upstream:upstream-state up) :connected)
    (feed-state up ":testbot!u@h JOIN #a")
    (feed-state up ":testbot!u@h JOIN #b")
    (feed-state up ":alice!a@b JOIN #a")
    (feed-state up ":alice!a@b JOIN #b")
    (feed-state up ":alice!a@b QUIT :bye")
    (is (equal '("testbot") (channel-nick-list up "#a")))
    (is (equal '("testbot") (channel-nick-list up "#b")))))

(test upstream-kick-removes-only-from-channel
  "KICK removes user only from the kicked channel."
  (let ((up (make-state-upstream)))
    (setf (cloak.upstream:upstream-state up) :connected)
    (feed-state up ":testbot!u@h JOIN #a")
    (feed-state up ":testbot!u@h JOIN #b")
    (feed-state up ":alice!a@b JOIN #a")
    (feed-state up ":alice!a@b JOIN #b")
    (feed-state up ":op!o@h KICK #a alice :reason")
    (is (equal '("testbot") (channel-nick-list up "#a")))
    (is (equal '("alice" "testbot") (channel-nick-list up "#b")))))
