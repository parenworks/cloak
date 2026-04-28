;;;; test/test-bouncer.lisp - Bouncer and upstream tests

(in-package #:cloak.test)

(in-suite :cloak-tests)

;;; --- Reconnect backoff tests ---

(test backoff-initial-delay
  "First reconnect attempt uses initial delay."
  (is (= 2 (cloak.upstream:calculate-backoff 0 :initial 2 :max 300))))

(test backoff-exponential-growth
  "Delay doubles each attempt."
  (is (= 2 (cloak.upstream:calculate-backoff 0 :initial 2 :max 300)))
  (is (= 4 (cloak.upstream:calculate-backoff 1 :initial 2 :max 300)))
  (is (= 8 (cloak.upstream:calculate-backoff 2 :initial 2 :max 300)))
  (is (= 16 (cloak.upstream:calculate-backoff 3 :initial 2 :max 300))))

(test backoff-capped-at-max
  "Delay never exceeds max."
  (is (= 300 (cloak.upstream:calculate-backoff 20 :initial 2 :max 300)))
  (is (= 60 (cloak.upstream:calculate-backoff 20 :initial 2 :max 60))))

(test backoff-jitter-range
  "Jittered backoff stays within expected range."
  (let ((results (loop repeat 50
                       collect (cloak.upstream:calculate-backoff 3 :initial 2 :max 300 :jitter t))))
    (is (every (lambda (r) (and (>= r 8) (<= r 24))) results))))

;;; --- CTCP parsing tests ---

(test ctcp-version-detection
  "CTCP VERSION is detected in a PRIVMSG."
  (let* ((soh (string (code-char 1)))
         (raw (format nil ":nick!user@host PRIVMSG cloak-test :~aVERSION~a" soh soh))
         (msg (cloak.protocol:parse-message raw)))
    (is (string= "PRIVMSG" (cloak.protocol:irc-message-command msg)))
    (let ((text (second (cloak.protocol:irc-message-params msg))))
      (is (char= (code-char 1) (char text 0)))
      (is (char= (code-char 1) (char text (1- (length text))))))))

;;; --- Password hashing tests ---

(test password-hash-roundtrip
  "Hashed password verifies correctly."
  (let ((hash (cloak.config:hash-password "secret123")))
    (is (cloak.config:verify-password "secret123" hash))
    (is (not (cloak.config:verify-password "wrong" hash)))))

(test password-hash-unique-salts
  "Different calls produce different hashes for same password."
  (let ((h1 (cloak.config:hash-password "secret123"))
        (h2 (cloak.config:hash-password "secret123")))
    (is (not (string= h1 h2)))
    (is (cloak.config:verify-password "secret123" h1))
    (is (cloak.config:verify-password "secret123" h2))))

;;; --- *status command tests ---

(test status-parse-command
  "Parse *status commands from PRIVMSG."
  (let ((msg (cloak.protocol:parse-message
              ":testclient PRIVMSG *status :help")))
    (is (string= "PRIVMSG" (cloak.protocol:irc-message-command msg)))
    (is (string= "*status" (first (cloak.protocol:irc-message-params msg))))
    (is (string= "help" (second (cloak.protocol:irc-message-params msg))))))

;;; --- Alt nick tests ---

(test alt-nick-config
  "Alt nick is preserved through config serialization."
  (let* ((net (make-instance 'cloak.config:network-config
                :name "test" :server "irc.test.net" :port 6697
                :tls t :nick "primary" :alt-nick "backup"
                :autojoin '("#test")))
         (plist (cloak.config::config-to-plist net)))
    (is (string= "backup" (getf plist :alt-nick)))))

;;; --- Block MOTD config test ---

(test block-motd-config
  "Block MOTD option roundtrips through config."
  (let* ((net (make-instance 'cloak.config:network-config
                :name "test" :server "irc.test.net" :port 6697
                :tls t :nick "user" :block-motd t))
         (plist (cloak.config::config-to-plist net))
         (restored (cloak.config::plist-to-network plist)))
    (is (eq t (cloak.config:network-block-motd restored)))))

;;; --- Config roundtrip ---

(test config-roundtrip
  "Config can be serialized and deserialized."
  (let* ((net (make-instance 'cloak.config:network-config
                :name "test"
                :server "irc.test.net"
                :port 6697
                :tls t
                :nick "testuser"
                :autojoin '("#test" "#dev")))
         (user (make-instance 'cloak.config:user-config
                 :name "admin"
                 :password-hash "hash123"
                 :admin-p t
                 :networks (list net)))
         (cfg (make-instance 'cloak.config:bouncer-config
                :users (list user)))
         (plist (cloak.config::config-to-plist cfg))
         (restored (cloak.config::plist-to-config plist)))
    (is (= 1 (length (cloak.config:config-users restored))))
    (let ((u (first (cloak.config:config-users restored))))
      (is (string= "admin" (cloak.config:user-name u)))
      (is (= 1 (length (cloak.config:user-networks u))))
      (let ((n (first (cloak.config:user-networks u))))
        (is (string= "test" (cloak.config:network-name n)))
        (is (string= "irc.test.net" (cloak.config:network-server n)))
        (is (equal '("#test" "#dev") (cloak.config:network-autojoin n)))))))
