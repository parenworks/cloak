;;;; test/test-config-extended.lisp - Extended config tests
;;;; Covers defaults, lookups, serialization edge cases, and password hashing.

(in-package #:cloak.test)

(in-suite :cloak-tests)

;;; --- Default Config Values ---

(test config-defaults
  "Bouncer-config has sensible defaults."
  (let ((cfg (make-instance 'cloak.config:bouncer-config)))
    (is (string= "0.0.0.0" (cloak.config:config-listen-host cfg)))
    (is (= 6697 (cloak.config:config-listen-port cfg)))
    (is (eq t (cloak.config:config-listen-tls cfg)))
    (is (string= "127.0.0.1" (cloak.config:config-web-host cfg)))
    (is (= 8076 (cloak.config:config-web-port cfg)))
    (is (eq :info (cloak.config:config-log-level cfg)))
    (is (null (cloak.config:config-tls-cert cfg)))
    (is (null (cloak.config:config-tls-key cfg)))))

(test network-config-defaults
  "Network-config has sensible defaults."
  (let ((net (make-instance 'cloak.config:network-config
               :name "test" :server "irc.test" :nick "nick")))
    (is (= 6697 (cloak.config:network-port net)))
    (is (eq t (cloak.config:network-tls net)))
    (is (string= "CLoak User" (cloak.config:network-realname net)))
    (is (null (cloak.config:network-password net)))
    (is (null (cloak.config:network-sasl net)))
    (is (null (cloak.config:network-alt-nick net)))
    (is (null (cloak.config:network-autojoin net)))
    (is (= 500 (cloak.config:network-buffer-size net)))
    (is (null (cloak.config:network-block-motd net)))))

(test user-config-defaults
  "User-config defaults to non-admin with empty networks."
  (let ((user (make-instance 'cloak.config:user-config
                :name "bob" :password-hash "hash")))
    (is (null (cloak.config:user-admin-p user)))
    (is (null (cloak.config:user-networks user)))))

;;; --- Lookup Functions ---

(test find-user-by-name
  "find-user finds user case-insensitively."
  (let* ((user (make-instance 'cloak.config:user-config
                 :name "Alice" :password-hash "hash"))
         (cfg (make-instance 'cloak.config:bouncer-config
                 :users (list user))))
    (is (eq user (cloak.config:find-user "Alice" cfg)))
    (is (eq user (cloak.config:find-user "alice" cfg)))
    (is (eq user (cloak.config:find-user "ALICE" cfg)))))

(test find-user-missing
  "find-user returns NIL for unknown user."
  (let ((cfg (make-instance 'cloak.config:bouncer-config :users nil)))
    (is (null (cloak.config:find-user "nobody" cfg)))))

(test find-network-by-name
  "find-network finds network under a user case-insensitively."
  (let* ((net (make-instance 'cloak.config:network-config
                :name "Libera" :server "irc.libera.chat" :nick "me"
                :port 6697 :tls t))
         (user (make-instance 'cloak.config:user-config
                 :name "bob" :password-hash "hash" :networks (list net)))
         (cfg (make-instance 'cloak.config:bouncer-config :users (list user))))
    (is (eq net (cloak.config:find-network "bob" "Libera" cfg)))
    (is (eq net (cloak.config:find-network "bob" "libera" cfg)))
    (is (null (cloak.config:find-network "bob" "freenode" cfg)))
    (is (null (cloak.config:find-network "unknown" "Libera" cfg)))))

;;; --- Full Config Roundtrip ---

(test config-full-roundtrip
  "Full config survives serialize -> deserialize with all fields."
  (let* ((net (make-instance 'cloak.config:network-config
                :name "libera" :server "irc.libera.chat" :port 6697
                :tls t :nick "user" :username "myident"
                :realname "Real Name" :password "pass123"
                :sasl :plain :alt-nick "user_"
                :autojoin '("#lisp" "#test") :buffer-size 1000
                :block-motd t))
         (user (make-instance 'cloak.config:user-config
                 :name "admin" :password-hash "sha256:salt:hash"
                 :admin-p t :networks (list net)))
         (cfg (make-instance 'cloak.config:bouncer-config
                 :listen-host "127.0.0.1" :listen-port 7000
                 :listen-tls nil :tls-cert "/path/cert" :tls-key "/path/key"
                 :web-host "0.0.0.0" :web-port 9000
                 :log-level :warn
                 :enabled-modules '("block-motd" "auto-away")
                 :users (list user)))
         (plist (cloak.config:config-to-plist cfg))
         (restored (cloak.config::plist-to-config plist)))
    ;; Top-level config
    (is (string= "127.0.0.1" (cloak.config:config-listen-host restored)))
    (is (= 7000 (cloak.config:config-listen-port restored)))
    (is (null (cloak.config:config-listen-tls restored)))
    (is (string= "/path/cert" (cloak.config:config-tls-cert restored)))
    (is (string= "/path/key" (cloak.config:config-tls-key restored)))
    (is (string= "0.0.0.0" (cloak.config:config-web-host restored)))
    (is (= 9000 (cloak.config:config-web-port restored)))
    (is (eq :warn (cloak.config:config-log-level restored)))
    (is (equal '("block-motd" "auto-away")
               (cloak.config:config-enabled-modules restored)))
    ;; User
    (is (= 1 (length (cloak.config:config-users restored))))
    (let ((u (first (cloak.config:config-users restored))))
      (is (string= "admin" (cloak.config:user-name u)))
      (is (string= "sha256:salt:hash" (cloak.config:user-password-hash u)))
      (is (eq t (cloak.config:user-admin-p u)))
      ;; Network
      (is (= 1 (length (cloak.config:user-networks u))))
      (let ((n (first (cloak.config:user-networks u))))
        (is (string= "libera" (cloak.config:network-name n)))
        (is (string= "irc.libera.chat" (cloak.config:network-server n)))
        (is (= 6697 (cloak.config:network-port n)))
        (is (eq t (cloak.config:network-tls n)))
        (is (string= "user" (cloak.config:network-nick n)))
        (is (string= "myident" (cloak.config:network-username n)))
        (is (string= "Real Name" (cloak.config:network-realname n)))
        (is (string= "pass123" (cloak.config:network-password n)))
        (is (eq :plain (cloak.config:network-sasl n)))
        (is (string= "user_" (cloak.config:network-alt-nick n)))
        (is (equal '("#lisp" "#test") (cloak.config:network-autojoin n)))
        (is (= 1000 (cloak.config:network-buffer-size n)))
        (is (eq t (cloak.config:network-block-motd n)))))))

(test config-plist-to-config-defaults
  "plist-to-config fills defaults for missing keys."
  (let ((restored (cloak.config::plist-to-config '())))
    (is (string= "0.0.0.0" (cloak.config:config-listen-host restored)))
    (is (= 6697 (cloak.config:config-listen-port restored)))
    (is (string= "127.0.0.1" (cloak.config:config-web-host restored)))
    (is (= 8076 (cloak.config:config-web-port restored)))
    (is (eq :info (cloak.config:config-log-level restored)))))

(test config-multi-user-multi-network
  "Config with multiple users and networks roundtrips."
  (let* ((net1 (make-instance 'cloak.config:network-config
                 :name "net1" :server "a.com" :nick "n1" :port 6667 :tls nil))
         (net2 (make-instance 'cloak.config:network-config
                 :name "net2" :server "b.com" :nick "n2" :port 6697 :tls t))
         (user1 (make-instance 'cloak.config:user-config
                  :name "alice" :password-hash "h1" :networks (list net1 net2)))
         (user2 (make-instance 'cloak.config:user-config
                  :name "bob" :password-hash "h2" :admin-p t :networks nil))
         (cfg (make-instance 'cloak.config:bouncer-config
                 :users (list user1 user2)))
         (plist (cloak.config:config-to-plist cfg))
         (restored (cloak.config::plist-to-config plist)))
    (is (= 2 (length (cloak.config:config-users restored))))
    (let ((u1 (first (cloak.config:config-users restored)))
          (u2 (second (cloak.config:config-users restored))))
      (is (string= "alice" (cloak.config:user-name u1)))
      (is (= 2 (length (cloak.config:user-networks u1))))
      (is (string= "bob" (cloak.config:user-name u2)))
      (is (null (cloak.config:user-networks u2)))
      (is (eq t (cloak.config:user-admin-p u2))))))

;;; --- Password Hashing Edge Cases ---

(test password-empty-string
  "Empty password can be hashed and verified."
  (let ((hash (cloak.config:hash-password "")))
    (is (cloak.config:verify-password "" hash))
    (is (not (cloak.config:verify-password "x" hash)))))

(test password-unicode
  "Unicode password hashes and verifies."
  (let ((hash (cloak.config:hash-password "p@ssw0rd!")))
    (is (cloak.config:verify-password "p@ssw0rd!" hash))
    (is (not (cloak.config:verify-password "p@ssw0rd" hash)))))

(test password-long
  "Long password hashes and verifies."
  (let* ((long-pw (make-string 500 :initial-element #\a))
         (hash (cloak.config:hash-password long-pw)))
    (is (cloak.config:verify-password long-pw hash))
    (is (not (cloak.config:verify-password "a" hash)))))

(test verify-password-malformed-hash
  "verify-password returns NIL for malformed hash strings."
  (is (null (cloak.config:verify-password "test" "not-a-hash")))
  (is (null (cloak.config:verify-password "test" "sha256:short")))
  (is (null (cloak.config:verify-password "test" ""))))

(test default-password-detection
  "default-password-p detects the default password."
  (let ((user (make-instance 'cloak.config:user-config
                :name "admin"
                :password-hash (cloak.config:hash-password
                                cloak.config::*default-password*))))
    (is (cloak.config:default-password-p user)))
  (let ((user (make-instance 'cloak.config:user-config
                :name "admin"
                :password-hash (cloak.config:hash-password "secure123"))))
    (is (not (cloak.config:default-password-p user)))))
