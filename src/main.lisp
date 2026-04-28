;;;; main.lisp - Entry point for CLoak IRC bouncer

(in-package #:cloak)

(defvar *version* "0.1.0")

(defun version ()
  "Return CLoak version string."
  *version*)

(defun start (&key (config-path cloak.config:*config-path*)
                   (web t))
  "Start the CLoak IRC bouncer.
Loads config from CONFIG-PATH, starts upstream connections,
client listener, and optionally the web admin interface."
  (format t "~&~%")
  (format t "   ██████╗██╗      ██████╗  █████╗ ██╗  ██╗~%")
  (format t "  ██╔════╝██║     ██╔═══██╗██╔══██╗██║ ██╔╝~%")
  (format t "  ██║     ██║     ██║   ██║███████║█████╔╝ ~%")
  (format t "  ██║     ██║     ██║   ██║██╔══██║██╔═██╗ ~%")
  (format t "  ╚██████╗███████╗╚██████╔╝██║  ██║██║  ██╗~%")
  (format t "   ╚═════╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝~%")
  (format t "~%  CLoak IRC Bouncer v~a~%" *version*)
  (format t "  Common Lisp • Fluxion Web UI~%~%")

  ;; Load configuration
  (let ((config (cloak.config:load-config config-path)))
    ;; Create and start bouncer
    (let ((bouncer (cloak.bouncer:make-bouncer config)))
      (cloak.bouncer:start-bouncer bouncer)
      ;; Start web admin if requested
      (when web
        (format t "[CLoak] Web admin: http://~a:~d~%"
                (cloak.config:config-web-host config)
                (cloak.config:config-web-port config))
        ;; TODO: Start Fluxion web app
        )
      bouncer)))

(defun stop ()
  "Stop the running CLoak bouncer."
  (when cloak.bouncer:*bouncer*
    (cloak.bouncer:stop-bouncer cloak.bouncer:*bouncer*)))

(defun main ()
  "Command-line entry point for CLoak.
Starts the bouncer and blocks until interrupted."
  (let ((bouncer (start)))
    (when bouncer
      (format t "[CLoak] Press Ctrl+C to stop~%")
      (handler-case
          (loop (sleep 1))
        (#+sbcl sb-sys:interactive-interrupt
         #+ccl ccl:interrupt-signal-condition
         #-(or sbcl ccl) condition
         ()
         (format t "~%[CLoak] Shutting down...~%")
         (stop))))))
