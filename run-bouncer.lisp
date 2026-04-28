;;;; run-bouncer.lisp - Full bouncer test: upstream + client listener
;;;; Usage: sbcl --load run-bouncer.lisp
;;;;
;;;; Then connect your IRC client to localhost:6667
;;;; PASS glenn/libera:test
;;;; NICK yournick
;;;; USER yournick 0 * :realname

(push #P"/home/glenn/SourceCode/cloak/" asdf:*central-registry*)
(ql:quickload "cloak" :silent t)

;; Configure: non-TLS listener on 6667 for local testing
(setf cloak.config:*config*
      (make-instance 'cloak.config:bouncer-config
        :listen-host "127.0.0.1"
        :listen-port 6667
        :listen-tls nil
        :web-port 8076
        :users (list
                (make-instance 'cloak.config:user-config
                  :name "glenn"
                  :password-hash (cloak.config:hash-password "test")
                  :admin-p t
                  :networks (list
                             (make-instance 'cloak.config:network-config
                               :name "libera"
                               :server "irc.libera.chat"
                               :port 6697
                               :tls t
                               :nick "cloak-test"
                               :autojoin '("#cloak-test")))))))

(format t "~%")
(format t "   ██████╗██╗      ██████╗  █████╗ ██╗  ██╗~%")
(format t "  ██╔════╝██║     ██╔═══██╗██╔══██╗██║ ██╔╝~%")
(format t "  ██║     ██║     ██║   ██║███████║█████╔╝ ~%")
(format t "  ██║     ██║     ██║   ██║██╔══██║██╔═██╗ ~%")
(format t "  ╚██████╗███████╗╚██████╔╝██║  ██║██║  ██╗~%")
(format t "   ╚═════╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝~%")
(format t "~%  CLoak IRC Bouncer - Test Mode~%~%")

(let ((bouncer (cloak.bouncer:make-bouncer cloak.config:*config*)))
  (cloak.bouncer:start-bouncer bouncer)
  (format t "~%============================================~%")
  (format t "  Connect your IRC client to: 127.0.0.1:6667~%")
  (format t "  Server password: glenn/libera:test~%")
  (format t "============================================~%~%")
  (format t "[CLoak] Press Ctrl+C to stop~%~%")
  (handler-case
      (loop (sleep 1))
    (#+sbcl sb-sys:interactive-interrupt
     #-sbcl condition ()
     (format t "~%[CLoak] Shutting down...~%")
     (cloak.bouncer:stop-bouncer bouncer))))

(format t "[CLoak] Stopped.~%")
(uiop:quit)
