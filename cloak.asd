;;;; cloak.asd - ASDF system definition for CLoak IRC bouncer

(defsystem "cloak"
  :name "cloak"
  :version "0.1.0"
  :author "Glenn Thompson"
  :license "MIT"
  :description "An IRC bouncer written in Common Lisp with a Fluxion web interface"
  :long-description "CLoak is a ZNC-comparable IRC bouncer that maintains persistent
connections to IRC networks and relays traffic to/from multiple clients.
Features include IRCv3 support, SASL authentication, message buffering
with per-client playback, TLS on both sides, and a live web admin panel
built with Fluxion."
  :depends-on ("iolib"
               "bordeaux-threads"
               "cl+ssl"
               "flexi-streams"
               "cl-base64"
               "split-sequence"
               "cl-ppcre"
               "alexandria"
               "local-time"
               "cl-json"
               "fluxion")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "config")
     (:file "protocol")
     (:file "buffer")
     (:file "upstream")
     (:file "downstream")
     (:file "bouncer")
     (:file "modules")
     (:file "main")))
   (:module "web"
    :serial t
    :components
    ((:file "package")
     (:file "components")
     (:file "app"))))
  :in-order-to ((test-op (test-op "cloak/test"))))

(defsystem "cloak/test"
  :depends-on ("cloak" "fiveam")
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test-protocol")
                             (:file "test-buffer")
                             (:file "test-bouncer"))))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run! :cloak-tests)))
