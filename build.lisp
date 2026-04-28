;;;; build.lisp - Build CLoak as a standalone executable
;;;; Usage: sbcl --load build.lisp
;;;;
;;;; Produces: bin/cloak

(require :asdf)
(push #P"./" asdf:*central-registry*)

;; Load all dependencies
(ql:quickload "cloak" :silent t)

;; Ensure output directory exists
(ensure-directories-exist #P"bin/")

(format t "[CLoak] Building standalone executable...~%")

(sb-ext:save-lisp-and-die
 #P"bin/cloak"
 :toplevel #'cloak:main
 :executable t
 :purify t
 #+sb-core-compression :compression
 #+sb-core-compression 9)
