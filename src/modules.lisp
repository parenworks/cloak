;;;; modules.lisp - Module system for CLoak
;;;; Extensible module framework with plugin support.
;;;; Third-party modules: drop .lisp files into ~/.config/cloak/modules/

(in-package #:cloak.modules)

;;; --- Module Protocol ---

(defclass module ()
  ((name :initarg :name :accessor module-name)
   (description :initarg :description :accessor module-description
                :initform "")
   (version :initarg :version :accessor module-version
            :initform "1.0")
   (author :initarg :author :accessor module-author
           :initform nil)
   (scope :initarg :scope :accessor module-scope
          :initform :global
          :documentation "Module scope: :global or :network")
   (network :initarg :network :accessor module-network
            :initform nil
            :documentation "When scope is :network, which network this instance is for.")
   (timers :initform nil :accessor module-timers
           :documentation "List of active timer threads for this module."))
  (:documentation "Base class for CLoak modules."))

;;; --- Lifecycle Hooks ---

(defgeneric on-load (module bouncer)
  (:documentation "Called when MODULE is loaded into BOUNCER.")
  (:method ((module module) bouncer)
    (declare (ignore bouncer))
    (format t "[CLoak] Module loaded: ~a~%" (module-name module))))

(defgeneric on-unload (module bouncer)
  (:documentation "Called when MODULE is unloaded from BOUNCER.")
  (:method ((module module) bouncer)
    (declare (ignore bouncer))
    ;; Stop all timers
    (dolist (timer (module-timers module))
      (when (bt:thread-alive-p timer)
        (bt:destroy-thread timer)))
    (setf (module-timers module) nil)
    (format t "[CLoak] Module unloaded: ~a~%" (module-name module))))

;;; --- Message Hooks ---

(defgeneric on-upstream-message (module bouncer upstream raw-line msg)
  (:documentation "Called for each message from an upstream server.
Return :halt to prevent further processing, :drop to silently consume.")
  (:method ((module module) bouncer upstream raw-line msg)
    (declare (ignore bouncer upstream raw-line msg))
    nil))

(defgeneric on-downstream-message (module bouncer client raw-line msg)
  (:documentation "Called for each message from a downstream client.
Return :halt to prevent further processing, :drop to silently consume.")
  (:method ((module module) bouncer client raw-line msg)
    (declare (ignore bouncer client raw-line msg))
    nil))

;;; --- Connection Hooks ---

(defgeneric on-client-attach (module bouncer client user-name network-name)
  (:documentation "Called when a client attaches to the bouncer.")
  (:method ((module module) bouncer client user-name network-name)
    (declare (ignore bouncer client user-name network-name))
    nil))

(defgeneric on-client-detach (module bouncer client)
  (:documentation "Called when a client detaches from the bouncer.")
  (:method ((module module) bouncer client)
    (declare (ignore bouncer client))
    nil))

(defgeneric on-upstream-connect (module bouncer upstream)
  (:documentation "Called when an upstream connection is established (001 received).")
  (:method ((module module) bouncer upstream)
    (declare (ignore bouncer upstream))
    nil))

(defgeneric on-upstream-disconnect (module bouncer upstream)
  (:documentation "Called when an upstream connection is lost.")
  (:method ((module module) bouncer upstream)
    (declare (ignore bouncer upstream))
    nil))

;;; --- Auth Hooks ---

(defgeneric on-new-connection (module bouncer client-ip)
  (:documentation "Called when a new client connects, before auth.
Return :drop to reject the connection.")
  (:method ((module module) bouncer client-ip)
    (declare (ignore bouncer client-ip))
    nil))

(defgeneric on-auth-failure (module bouncer client-ip)
  (:documentation "Called when a client fails authentication.")
  (:method ((module module) bouncer client-ip)
    (declare (ignore bouncer client-ip))
    nil))

;;; --- Channel Hooks ---

(defgeneric on-channel-join (module bouncer upstream channel)
  (:documentation "Called when we join a channel on an upstream.")
  (:method ((module module) bouncer upstream channel)
    (declare (ignore bouncer upstream channel))
    nil))

(defgeneric on-channel-part (module bouncer upstream channel)
  (:documentation "Called when we part a channel on an upstream.")
  (:method ((module module) bouncer upstream channel)
    (declare (ignore bouncer upstream channel))
    nil))

(defgeneric on-channel-kick (module bouncer upstream channel kicker reason)
  (:documentation "Called when we are kicked from a channel.")
  (:method ((module module) bouncer upstream channel kicker reason)
    (declare (ignore bouncer upstream channel kicker reason))
    nil))

;;; --- Settings ---

(defgeneric module-settings-html (module)
  (:documentation "Return HTML string for this module's settings form, or NIL.")
  (:method ((module module))
    nil))

(defgeneric on-save-settings (module params)
  (:documentation "Called when settings are saved from the web UI.
PARAMS is an alist of form field names to values.")
  (:method ((module module) params)
    (declare (ignore params))
    nil))

;;; --- Timer Support ---

(defun start-module-timer (module name interval function)
  "Start a periodic timer for MODULE. FUNCTION is called every INTERVAL seconds.
NAME is used for the thread name. Returns the timer thread."
  (let ((thread
          (bt:make-thread
           (lambda ()
             (loop
               (sleep interval)
               (handler-case (funcall function)
                 (error (e)
                   (format t "[CLoak] Module ~a timer error: ~a~%"
                           (module-name module) e)))))
           :name (format nil "cloak-mod-~a-~a" (module-name module) name))))
    (push thread (module-timers module))
    thread))

;;; --- Persistent Storage ---

(defun module-data-dir ()
  "Return the directory for module persistent data."
  (let ((env (uiop:getenv "XDG_DATA_HOME")))
    (merge-pathnames "cloak/modules/"
                     (if (and env (plusp (length env)))
                         (uiop:ensure-directory-pathname env)
                         (merge-pathnames ".local/share/" (user-homedir-pathname))))))

(defun module-data-path (module-name)
  "Return the data file path for MODULE-NAME."
  (merge-pathnames (format nil "~a.sexp" module-name) (module-data-dir)))

(defun load-module-data (module-name)
  "Load persisted data for MODULE-NAME. Returns a plist or NIL."
  (let ((path (module-data-path module-name)))
    (when (probe-file path)
      (handler-case
          (with-open-file (in path :direction :input)
            (read in nil nil))
        (error (e)
          (format t "[CLoak] Failed to load data for module ~a: ~a~%"
                  module-name e)
          nil)))))

(defun save-module-data (module-name data)
  "Save DATA (a plist or s-expression) for MODULE-NAME."
  (let ((path (module-data-path module-name)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-pretty* t)
            (*print-case* :downcase))
        (prin1 data out)
        (terpri out)))
    path))

;;; --- Module Registry ---

(defvar *module-registry* (make-hash-table :test 'equal)
  "Registry of available module classes by name.
Maps module-name (string) -> plist (:class :description :scope :version :author :source).")

(defvar *active-modules* (make-hash-table :test 'equal)
  "Active module instances. Maps module-name (string) -> module instance.")

(defun register-module (name class &key (description "") (scope :global)
                                        (version "1.0") (author nil))
  "Register a module CLASS under NAME with metadata."
  (setf (gethash name *module-registry*)
        (list :class class :description description :scope scope
              :version version :author author
              :source (if (find-class class nil) :built-in :plugin))))

(defmacro define-module (name (&rest superclasses) (&rest slots) &rest options)
  "Define and register a CLoak module.
NAME is a string. SUPERCLASSES defaults to (module).
OPTIONS are class options plus :description, :scope, :version, :author
which are extracted for registration.

Example:
  (define-module \"my-module\" ()
    ((setting :initform nil :accessor my-setting))
    (:description \"Does something cool\")
    (:version \"1.0\")
    (:author \"Me\")
    (:scope :global))"
  (let* ((class-name (intern (string-upcase (format nil "~a-module" name))
                             *package*))
         (supers (or superclasses '(module)))
         (description (second (assoc :description options)))
         (scope (or (second (assoc :scope options)) :global))
         (version (or (second (assoc :version options)) "1.0"))
         (author (second (assoc :author options)))
         ;; Filter out our special options, keep CLOS ones
         (class-options (remove-if (lambda (opt)
                                     (member (car opt) '(:description :scope
                                                         :version :author)))
                                   options)))
    `(progn
       (defclass ,class-name (,@supers)
         ,slots
         ,@class-options
         (:documentation ,(or description (format nil "CLoak module: ~a" name))))
       (register-module ,name ',class-name
         :description ,(or description "")
         :scope ,scope
         :version ,version
         :author ,author))))

(defun find-module-registration (name)
  "Find a registered module entry by NAME. Returns plist or NIL."
  (gethash name *module-registry*))

(defun list-registered-modules ()
  "Return list of (name . plist) for all registered modules."
  (let (result)
    (maphash (lambda (name entry)
               (push (cons name entry) result))
             *module-registry*)
    (sort result #'string< :key #'car)))

(defun module-active-p (name)
  "Return T if module NAME is currently loaded."
  (gethash name *active-modules*))

(defun active-module (name)
  "Return the active module instance for NAME, or NIL."
  (gethash name *active-modules*))

(defun list-active-modules ()
  "Return list of all active module instances."
  (let (result)
    (maphash (lambda (name mod)
               (declare (ignore name))
               (push mod result))
             *active-modules*)
    (sort result #'string< :key #'module-name)))

(defun load-module (name bouncer)
  "Instantiate and load module NAME into BOUNCER."
  (when (module-active-p name)
    (format t "[CLoak] Module ~a already loaded~%" name)
    (return-from load-module (active-module name)))
  (let ((entry (find-module-registration name)))
    (unless entry
      (format t "[CLoak] Unknown module: ~a~%" name)
      (return-from load-module nil))
    (let* ((class (getf entry :class))
           (desc (getf entry :description))
           (scope (getf entry :scope))
           (ver (getf entry :version))
           (auth (getf entry :author))
           (module (make-instance class
                     :name name
                     :description desc
                     :scope scope
                     :version (or ver "1.0")
                     :author auth)))
      (on-load module bouncer)
      (setf (gethash name *active-modules*) module)
      module)))

(defun unload-module (name bouncer)
  "Unload module NAME from BOUNCER."
  (let ((module (active-module name)))
    (when module
      (on-unload module bouncer)
      (remhash name *active-modules*)
      t)))

;;; --- Module Hook Dispatch ---

(defun run-upstream-hooks (bouncer upstream raw-line msg)
  "Run all active module upstream hooks. Return :halt or :drop to stop processing."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (let ((result (on-upstream-message module bouncer upstream raw-line msg)))
                   (when (member result '(:halt :drop))
                     (return-from run-upstream-hooks result)))
               (error (e)
                 (format t "[CLoak] Module ~a upstream hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*)
  nil)

(defun run-downstream-hooks (bouncer client raw-line msg)
  "Run all active module downstream hooks. Return :halt or :drop to stop processing."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (let ((result (on-downstream-message module bouncer client raw-line msg)))
                   (when (member result '(:halt :drop))
                     (return-from run-downstream-hooks result)))
               (error (e)
                 (format t "[CLoak] Module ~a downstream hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*)
  nil)

(defun run-client-attach-hooks (bouncer client user-name network-name)
  "Run all active module client-attach hooks."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (on-client-attach module bouncer client user-name network-name)
               (error (e)
                 (format t "[CLoak] Module ~a attach hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*))

(defun run-client-detach-hooks (bouncer client)
  "Run all active module client-detach hooks."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (on-client-detach module bouncer client)
               (error (e)
                 (format t "[CLoak] Module ~a detach hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*))

(defun run-upstream-connect-hooks (bouncer upstream)
  "Run all active module upstream-connect hooks."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (on-upstream-connect module bouncer upstream)
               (error (e)
                 (format t "[CLoak] Module ~a connect hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*))

(defun run-upstream-disconnect-hooks (bouncer upstream)
  "Run all active module upstream-disconnect hooks."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (on-upstream-disconnect module bouncer upstream)
               (error (e)
                 (format t "[CLoak] Module ~a disconnect hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*))

(defun run-channel-join-hooks (bouncer upstream channel)
  "Run all active module channel-join hooks."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (on-channel-join module bouncer upstream channel)
               (error (e)
                 (format t "[CLoak] Module ~a join hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*))

(defun run-channel-part-hooks (bouncer upstream channel)
  "Run all active module channel-part hooks."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (on-channel-part module bouncer upstream channel)
               (error (e)
                 (format t "[CLoak] Module ~a part hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*))

(defun run-channel-kick-hooks (bouncer upstream channel kicker reason)
  "Run all active module channel-kick hooks."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (on-channel-kick module bouncer upstream channel kicker reason)
               (error (e)
                 (format t "[CLoak] Module ~a kick hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*))

(defun run-new-connection-hooks (bouncer client-ip)
  "Run all active module new-connection hooks. Return :drop to reject."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (let ((result (on-new-connection module bouncer client-ip)))
                   (when (eq result :drop)
                     (return-from run-new-connection-hooks :drop)))
               (error (e)
                 (format t "[CLoak] Module ~a new-connection hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*)
  nil)

(defun run-auth-failure-hooks (bouncer client-ip)
  "Run all active module auth-failure hooks."
  (maphash (lambda (name module)
             (declare (ignore name))
             (handler-case
                 (on-auth-failure module bouncer client-ip)
               (error (e)
                 (format t "[CLoak] Module ~a auth-failure hook error: ~a~%"
                         (module-name module) e))))
           *active-modules*))

;;; --- Plugin Loader ---

(defun plugin-directory ()
  "Return the directory for user-installed modules."
  (merge-pathnames "cloak/modules/"
                   (cloak.config:xdg-config-home)))

(defun scan-plugins ()
  "Scan the plugin directory and load any .lisp files found.
Returns the number of plugins loaded."
  (let ((dir (plugin-directory))
        (count 0))
    (when (probe-file dir)
      (dolist (file (directory (merge-pathnames "*.lisp" dir)))
        (handler-case
            (progn
              (format t "[CLoak] Loading plugin: ~a~%" (file-namestring file))
              (load file)
              (incf count))
          (error (e)
            (format t "[CLoak] Failed to load plugin ~a: ~a~%"
                    (file-namestring file) e)))))
    (when (plusp count)
      (format t "[CLoak] Loaded ~d plugin~:p from ~a~%" count dir))
    count))
