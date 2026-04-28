;;;; test/test-bouncer.lisp - Bouncer integration tests (stubs)

(in-package #:cloak.test)

(in-suite :cloak-tests)

;; Integration tests will be added as features stabilize.
;; These require mock sockets or a local IRC server.

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
