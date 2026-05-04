;;;; test/test-nick-tracking.lisp - Tests for per-channel nick tracking

(in-package #:cloak.test)

(in-suite :cloak-tests)

;;; --- Helper: create a minimal upstream for testing ---

(defun make-nick-tracking-upstream ()
  "Create a minimal upstream with a test config for nick tracking tests."
  (let* ((net-cfg (make-instance 'cloak.config:network-config
                    :name "test" :server "irc.test.net" :port 6697
                    :tls t :nick "testbot" :autojoin '("#test")))
         (upstream (cloak.upstream:make-upstream net-cfg)))
    ;; Simulate being connected
    (setf (cloak.upstream:upstream-state upstream) :connected)
    upstream))

(defun feed-message (upstream raw-line)
  "Parse RAW-LINE and feed it to upstream--track-state."
  (let ((msg (cloak.protocol:parse-message raw-line)))
    (cloak.upstream::upstream--track-state upstream msg)))

(defun channel-nick-list (upstream channel)
  "Return a sorted list of nicks in CHANNEL."
  (let ((ht (gethash channel (cloak.upstream:upstream-channel-nicks upstream))))
    (when ht
      (let ((nicks nil))
        (maphash (lambda (k v) (declare (ignore v)) (push k nicks)) ht)
        (sort nicks #'string<)))))

;;; --- Tests ---

(test nick-tracking-join-creates-channel
  "When we JOIN a channel, channel-nicks entry is created."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (is (not (null (gethash "#test" (cloak.upstream:upstream-channel-nicks up)))))
    (is (equal '("testbot") (channel-nick-list up "#test")))))

(test nick-tracking-353-populates
  "RPL_NAMREPLY (353) populates the nick list."
  (let ((up (make-nick-tracking-upstream)))
    ;; First join so the channel entry exists
    (feed-message up ":testbot!user@host JOIN #test")
    ;; Then server sends NAMES reply
    (feed-message up ":server 353 testbot = #test :@alice +bob charlie testbot")
    (is (equal '("alice" "bob" "charlie" "testbot")
               (channel-nick-list up "#test")))))

(test nick-tracking-353-strips-prefixes
  "Mode prefixes (@+%~&) are stripped from nicks in 353."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (feed-message up ":server 353 testbot = #test :@op +voice %halfop ~owner &admin")
    (is (equal '("admin" "halfop" "op" "owner" "testbot" "voice")
               (channel-nick-list up "#test")))))

(test nick-tracking-other-join
  "When another user joins, they are added to the nick set."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (feed-message up ":alice!a@b JOIN #test")
    (is (equal '("alice" "testbot") (channel-nick-list up "#test")))))

(test nick-tracking-part-removes
  "When a user PARTs, they are removed from the channel."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (feed-message up ":alice!a@b JOIN #test")
    (feed-message up ":alice!a@b PART #test :bye")
    (is (equal '("testbot") (channel-nick-list up "#test")))))

(test nick-tracking-our-part-removes-channel
  "When we PART, the channel and its nicks are removed entirely."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (feed-message up ":alice!a@b JOIN #test")
    (feed-message up ":testbot!user@host PART #test")
    (is (null (gethash "#test" (cloak.upstream:upstream-channel-nicks up))))
    (is (null (gethash "#test" (cloak.upstream:upstream-channels up))))))

(test nick-tracking-quit-removes-from-all
  "When a user QUITs, they are removed from all channels."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (feed-message up ":testbot!user@host JOIN #dev")
    (feed-message up ":alice!a@b JOIN #test")
    (feed-message up ":alice!a@b JOIN #dev")
    (feed-message up ":alice!a@b QUIT :bye")
    (is (equal '("testbot") (channel-nick-list up "#test")))
    (is (equal '("testbot") (channel-nick-list up "#dev")))))

(test nick-tracking-nick-change
  "When a user changes nick, all channels are updated."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (feed-message up ":alice!a@b JOIN #test")
    (feed-message up ":alice!a@b NICK alice_away")
    (is (equal '("alice_away" "testbot") (channel-nick-list up "#test")))))

(test nick-tracking-our-nick-change
  "When we change nick, upstream-nick is updated and channels reflect it."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (feed-message up ":testbot!user@host NICK newbot")
    (is (string= "newbot" (cloak.upstream:upstream-nick up)))
    (is (equal '("newbot") (channel-nick-list up "#test")))))

(test nick-tracking-kick-removes
  "When a user is KICKed, they are removed from that channel."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (feed-message up ":alice!a@b JOIN #test")
    (feed-message up ":op!o@h KICK #test alice :bye")
    (is (equal '("testbot") (channel-nick-list up "#test")))))

(test nick-tracking-we-get-kicked
  "When we are KICKed, the channel is removed entirely."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (feed-message up ":alice!a@b JOIN #test")
    (feed-message up ":op!o@h KICK #test testbot :bye")
    (is (null (gethash "#test" (cloak.upstream:upstream-channel-nicks up))))
    (is (null (gethash "#test" (cloak.upstream:upstream-channels up))))))

(test nick-tracking-multiple-353
  "Multiple 353 messages accumulate nicks."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #test")
    (feed-message up ":server 353 testbot = #test :alice bob")
    (feed-message up ":server 353 testbot = #test :charlie dave")
    (is (equal '("alice" "bob" "charlie" "dave" "testbot")
               (channel-nick-list up "#test")))))

(test nick-tracking-empty-channel
  "A channel with no one else returns just our nick."
  (let ((up (make-nick-tracking-upstream)))
    (feed-message up ":testbot!user@host JOIN #empty")
    (is (equal '("testbot") (channel-nick-list up "#empty")))))
