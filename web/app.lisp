;;;; web/app.lisp - Fluxion web application for CLoak admin

(in-package #:cloak.web)

(defvar *web-app* nil "The running Fluxion web app instance.")

(defun start-web-admin (host port)
  "Start the CLoak web admin interface on HOST:PORT."
  (format t "[CLoak] Starting web admin on http://~a:~d~%" host port)
  ;; TODO: Build out Fluxion app with routes:
  ;; /         -> Dashboard
  ;; /networks -> Network management
  ;; /users    -> User management
  ;; /buffers  -> Buffer viewer
  ;; /modules  -> Module management
  ;; /config   -> Config editor
  ;; /logs     -> Log viewer
  )

(defun stop-web-admin ()
  "Stop the CLoak web admin interface."
  (when *web-app*
    (format t "[CLoak] Stopping web admin~%")
    ;; TODO: (server:stop *web-app*)
    (setf *web-app* nil)))
