;;;; modules.lisp - Module system for CLoak
;;;; Extensible module framework, similar to ZNC's module system.

(in-package #:cloak.modules)

;;; --- Module Protocol ---

(defclass module ()
  ((name :initarg :name :accessor module-name)
   (description :initarg :description :accessor module-description
                :initform ""))
  (:documentation "Base class for CLoak modules."))

(defgeneric on-load (module bouncer)
  (:documentation "Called when MODULE is loaded into BOUNCER.")
  (:method ((module module) bouncer)
    (declare (ignore bouncer))
    (format t "[CLoak] Module loaded: ~a~%" (module-name module))))

(defgeneric on-unload (module bouncer)
  (:documentation "Called when MODULE is unloaded from BOUNCER.")
  (:method ((module module) bouncer)
    (declare (ignore bouncer))
    (format t "[CLoak] Module unloaded: ~a~%" (module-name module))))

(defgeneric on-upstream-message (module upstream raw-line msg)
  (:documentation "Called for each message from an upstream server.
Return :halt to prevent further processing.")
  (:method ((module module) upstream raw-line msg)
    (declare (ignore upstream raw-line msg))
    nil))

(defgeneric on-downstream-message (module client raw-line msg)
  (:documentation "Called for each message from a downstream client.
Return :halt to prevent further processing.")
  (:method ((module module) client raw-line msg)
    (declare (ignore client raw-line msg))
    nil))

;;; --- Module Registry ---

(defvar *modules* (make-hash-table :test 'equal)
  "Registry of available module classes by name.")

(defvar *active-modules* nil
  "List of active module instances.")

(defun register-module (name class)
  "Register a module CLASS under NAME."
  (setf (gethash name *modules*) class))

(defun find-module (name)
  "Find a registered module by NAME."
  (gethash name *modules*))

(defun load-module (name bouncer)
  "Instantiate and load module NAME into BOUNCER."
  (let ((class (find-module name)))
    (when class
      (let ((module (make-instance class :name name)))
        (on-load module bouncer)
        (push module *active-modules*)
        module))))

(defun unload-module (name bouncer)
  "Unload module NAME from BOUNCER."
  (let ((module (find name *active-modules*
                      :key #'module-name :test #'string-equal)))
    (when module
      (on-unload module bouncer)
      (setf *active-modules* (remove module *active-modules*))
      t)))
