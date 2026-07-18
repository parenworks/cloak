;;;; main.lisp - Entry point for CLoak IRC bouncer

(in-package #:cloak)

(defvar *version* "0.5.0")

(defun version ()
  "Return CLoak version string."
  *version*)

(defun start (&key (config-path cloak.config:*config-path*)
                   (web t))
  "Start the CLoak IRC bouncer.
Loads config from CONFIG-PATH, starts upstream connections,
client listener, and optionally the web admin interface."
  (cloak-log "~&~%")
  (cloak-log "   ██████╗██╗      ██████╗  █████╗ ██╗  ██╗~%")
  (cloak-log "  ██╔════╝██║     ██╔═══██╗██╔══██╗██║ ██╔╝~%")
  (cloak-log "  ██║     ██║     ██║   ██║███████║█████╔╝ ~%")
  (cloak-log "  ██║     ██║     ██║   ██║██╔══██║██╔═██╗ ~%")
  (cloak-log "  ╚██████╗███████╗╚██████╔╝██║  ██║██║  ██╗~%")
  (cloak-log "   ╚═════╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝~%")
  (cloak-log "~%  CLoak IRC Bouncer v~a~%" *version*)
  (cloak-log "  Common Lisp • Fluxion Web UI~%~%")

  ;; Load configuration
  (let ((config (cloak.config:load-config config-path)))
    ;; Create and start bouncer
    (let ((bouncer (cloak.bouncer:make-bouncer config)))
      (cloak.bouncer:start-bouncer bouncer)
      ;; Start web admin if requested
      (when web
        (let ((web-package (find-package '#:cloak.web)))
          (if web-package
              (funcall (find-symbol "START-WEB-ADMIN" web-package)
                       (cloak.config:config-web-host config)
                       (cloak.config:config-web-port config))
              (cloak-log "[CLoak] Web admin unavailable; load cloak/web to enable it~%"))))
      bouncer)))

(defun stop ()
  "Stop the running CLoak bouncer."
  (let ((web-package (find-package '#:cloak.web)))
    (when web-package
      (cloak-log "[CLoak] Stopping web admin~%")
      (ignore-errors
        (funcall (find-symbol "STOP-WEB-ADMIN" web-package)))))
  (when cloak.bouncer:*bouncer*
    (cloak.bouncer:stop-bouncer cloak.bouncer:*bouncer*)))

(defvar *shutdown-requested* nil
  "Flag set by signal handlers to request clean shutdown.")

(defun main ()
  "Command-line entry point for CLoak.
Starts the bouncer and blocks until interrupted."
  (setf *shutdown-requested* nil)
  ;; Handle SIGTERM for clean shutdown (systemd, kill, etc.)
  #+sbcl
  (sb-sys:enable-interrupt sb-unix:sigterm
    (lambda (sig info context)
      (declare (ignore sig info context))
      (setf *shutdown-requested* t)))
  (let ((bouncer (start)))
    (when bouncer
      (cloak-log "[CLoak] Press Ctrl+C to stop~%")
      (handler-case
          (loop
            (sleep 1)
            (when *shutdown-requested*
              (cloak-log "~%[CLoak] SIGTERM received, shutting down...~%")
              (stop)
              (return)))
        (#+sbcl sb-sys:interactive-interrupt
         #+ccl ccl:interrupt-signal-condition
         #-(or sbcl ccl) condition
         ()
         (cloak-log "~%[CLoak] Shutting down...~%")
         (stop))))))
