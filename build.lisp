;;;; build.lisp - Build CLoak as a standalone executable
;;;; Usage: sbcl --load build.lisp
;;;;
;;;; Produces: bin/cloak

(require :asdf)
(push #P"./" asdf:*central-registry*)

;; Load all dependencies
(ql:quickload "cloak" :silent t)

;; Ensure Clack handler is fully loaded (Clack lazy-loads handlers
;; at runtime via ASDF/Quicklisp which fails in standalone images)
(ql:quickload "clack-handler-hunchentoot" :silent t)

;; Ensure output directory exists
(ensure-directories-exist #P"bin/")

(format t "[CLoak] Building standalone executable...~%")

;; Clear ASDF source registry and deregister the "asdf" system itself
;; so the binary doesn't try to find build-machine paths or upgrade
;; ASDF at runtime on the deployment server.
(asdf:clear-source-registry)
(asdf:clear-system "asdf")
(setf asdf:*central-registry* nil)

(sb-ext:save-lisp-and-die
 #P"bin/cloak"
 :toplevel #'cloak:main
 :executable t
 :purify t
 #+sb-core-compression :compression
 #+sb-core-compression 9)
