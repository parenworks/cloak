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
        (funcall (uiop:find-symbol* '#:start-web-admin '#:cloak.web)
                 (cloak.config:config-web-host config)
                 (cloak.config:config-web-port config)))
      bouncer)))

(defun stop ()
  "Stop the running CLoak bouncer."
  (format t "[CLoak] Stopping web admin~%")
  (ignore-errors
    (funcall (uiop:find-symbol* '#:stop-web-admin '#:cloak.web)))
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
      (format t "[CLoak] Press Ctrl+C to stop~%")
      (handler-case
          (loop
            (sleep 1)
            (when *shutdown-requested*
              (format t "~%[CLoak] SIGTERM received, shutting down...~%")
              (stop)
              (return)))
        (#+sbcl sb-sys:interactive-interrupt
         #+ccl ccl:interrupt-signal-condition
         #-(or sbcl ccl) condition
         ()
         (format t "~%[CLoak] Shutting down...~%")
         (stop))))))
