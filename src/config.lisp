;;;; config.lisp - Configuration system for CLoak
;;;; Config is a Lisp s-expression file, human-editable and web-editable.

(in-package #:cloak.config)

;;; --- Structures ---

(defclass bouncer-config ()
  ((listen-host :initarg :listen-host :accessor config-listen-host
                :initform "0.0.0.0")
   (listen-port :initarg :listen-port :accessor config-listen-port
                :initform 6697)
   (listen-tls :initarg :listen-tls :accessor config-listen-tls
               :initform t)
   (tls-cert :initarg :tls-cert :accessor config-tls-cert
             :initform nil)
   (tls-key :initarg :tls-key :accessor config-tls-key
            :initform nil)
   (web-host :initarg :web-host :accessor config-web-host
             :initform "127.0.0.1")
   (web-port :initarg :web-port :accessor config-web-port
             :initform 8076)
   (users :initarg :users :accessor config-users
          :initform nil)
   (log-level :initarg :log-level :accessor config-log-level
              :initform :info)
   (enabled-modules :initarg :enabled-modules :accessor config-enabled-modules
                    :initform '("ctcp-version" "block-motd" "auto-away")
                    :documentation "List of module names to load on startup.")))

(defclass user-config ()
  ((name :initarg :name :accessor user-name)
   (password-hash :initarg :password-hash :accessor user-password-hash)
   (networks :initarg :networks :accessor user-networks
             :initform nil)
   (admin-p :initarg :admin-p :accessor user-admin-p
            :initform nil)))

(defclass network-config ()
  ((name :initarg :name :accessor network-name)
   (server :initarg :server :accessor network-server)
   (port :initarg :port :accessor network-port
         :initform 6697)
   (tls :initarg :tls :accessor network-tls
        :initform t)
   (nick :initarg :nick :accessor network-nick)
   (username :initarg :username :accessor network-username
             :initform nil)
   (realname :initarg :realname :accessor network-realname
             :initform "CLoak User")
   (password :initarg :password :accessor network-password
             :initform nil)
   (sasl :initarg :sasl :accessor network-sasl
         :initform nil)
   (alt-nick :initarg :alt-nick :accessor network-alt-nick
             :initform nil
             :documentation "Alternate nick if primary is taken.")
   (autojoin :initarg :autojoin :accessor network-autojoin
             :initform nil)
   (buffer-size :initarg :buffer-size :accessor network-buffer-size
                :initform 500)
   (block-motd :initarg :block-motd :accessor network-block-motd
               :initform nil
               :documentation "If T, suppress MOTD on client connect.")))

;;; --- Globals ---

(defvar *config* nil
  "Current bouncer configuration.")

(defun xdg-config-home ()
  "Return the XDG_CONFIG_HOME directory, defaulting to ~/.config/."
  (let ((env (uiop:getenv "XDG_CONFIG_HOME")))
    (if (and env (plusp (length env)))
        (uiop:ensure-directory-pathname env)
        (merge-pathnames ".config/" (user-homedir-pathname)))))

(defvar *config-path* (merge-pathnames "cloak/config.lisp"
                                        (xdg-config-home))
  "Path to the configuration file.")

;;; --- Serialization ---

(defgeneric config-to-plist (obj)
  (:documentation "Serialize a config object to a plist for writing."))

(defmethod config-to-plist ((net network-config))
  (list :name (network-name net)
        :server (network-server net)
        :port (network-port net)
        :tls (network-tls net)
        :nick (network-nick net)
        :username (network-username net)
        :realname (network-realname net)
        :password (network-password net)
        :sasl (network-sasl net)
        :alt-nick (network-alt-nick net)
        :autojoin (network-autojoin net)
        :buffer-size (network-buffer-size net)
        :block-motd (network-block-motd net)))

(defmethod config-to-plist ((user user-config))
  (list :name (user-name user)
        :password-hash (user-password-hash user)
        :admin-p (user-admin-p user)
        :networks (mapcar #'config-to-plist (user-networks user))))

(defmethod config-to-plist ((cfg bouncer-config))
  (list :listen-host (config-listen-host cfg)
        :listen-port (config-listen-port cfg)
        :listen-tls (config-listen-tls cfg)
        :tls-cert (config-tls-cert cfg)
        :tls-key (config-tls-key cfg)
        :web-host (config-web-host cfg)
        :web-port (config-web-port cfg)
        :log-level (config-log-level cfg)
        :enabled-modules (config-enabled-modules cfg)
        :users (mapcar #'config-to-plist (config-users cfg))))

;;; --- Deserialization ---

(defun plist-to-network (plist)
  "Create a network-config from PLIST."
  (make-instance 'network-config
    :name (getf plist :name)
    :server (getf plist :server)
    :port (or (getf plist :port) 6697)
    :tls (getf plist :tls t)
    :nick (getf plist :nick)
    :username (getf plist :username)
    :realname (or (getf plist :realname) "CLoak User")
    :password (getf plist :password)
    :sasl (getf plist :sasl)
    :alt-nick (getf plist :alt-nick)
    :autojoin (getf plist :autojoin)
    :buffer-size (or (getf plist :buffer-size) 500)
    :block-motd (getf plist :block-motd)))

(defun plist-to-user (plist)
  "Create a user-config from PLIST."
  (make-instance 'user-config
    :name (getf plist :name)
    :password-hash (getf plist :password-hash)
    :admin-p (getf plist :admin-p)
    :networks (mapcar #'plist-to-network (getf plist :networks))))

(defun plist-to-config (plist)
  "Create a bouncer-config from PLIST."
  (make-instance 'bouncer-config
    :listen-host (or (getf plist :listen-host) "0.0.0.0")
    :listen-port (or (getf plist :listen-port) 6697)
    :listen-tls (getf plist :listen-tls t)
    :tls-cert (getf plist :tls-cert)
    :tls-key (getf plist :tls-key)
    :web-host (or (getf plist :web-host) "127.0.0.1")
    :web-port (or (getf plist :web-port) 8076)
    :log-level (or (getf plist :log-level) :info)
    :enabled-modules (or (getf plist :enabled-modules)
                         '("ctcp-version" "block-motd" "auto-away"))
    :users (mapcar #'plist-to-user (getf plist :users))))

;;; --- File I/O ---

(defvar *legacy-config-path* (merge-pathnames ".cloak/config.lisp"
                                               (user-homedir-pathname))
  "Legacy config path for backward compatibility.")

(defun load-config (&optional (path *config-path*))
  "Load configuration from PATH. Returns bouncer-config.
Falls back to legacy ~/.cloak/config.lisp if XDG path does not exist."
  (unless (probe-file path)
    (if (probe-file *legacy-config-path*)
        (progn
          (format t "[CLoak] Migrating config from ~a to ~a~%"
                  *legacy-config-path* path)
          (ensure-directories-exist path)
          (uiop:copy-file *legacy-config-path* path))
        (generate-default-config path)))
  (let ((plist (with-open-file (in path :direction :input)
                 (read in))))
    (setf *config* (plist-to-config plist))
    *config*))

(defun save-config (&optional (config *config*) (path *config-path*))
  "Save CONFIG to PATH as a readable s-expression."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
    (let ((*print-pretty* t)
          (*print-case* :downcase)
          (*print-right-margin* 80))
      (format out ";;;; CLoak configuration file~%")
      (format out ";;;; Generated by CLoak v~a~%" cloak:*version*)
      (format out ";;;; Edit manually or via web interface at http://~a:~d~2%"
              (config-web-host config) (config-web-port config))
      (prin1 (config-to-plist config) out)
      (terpri out)))
  path)

(defparameter *default-password* "changeme"
  "Default admin password for new installations.")

(defun generate-default-config (&optional (path *config-path*))
  "Generate a default configuration file at PATH."
  (let ((config (make-instance 'bouncer-config
                  :users (list (make-instance 'user-config
                                 :name "admin"
                                 :password-hash (hash-password *default-password*)
                                 :admin-p t
                                 :networks nil)))))
    (save-config config path)
    (format t "~&[CLoak] Generated default config at ~a~%" path)
    (format t "[CLoak] Default login: admin / ~a~%" *default-password*)
    (format t "[CLoak] You will be prompted to change the password on first login.~%")
    config))

(defun default-password-p (user-cfg)
  "Return T if USER-CFG still uses the default password."
  (verify-password *default-password* (user-password-hash user-cfg)))

;;; --- Password Hashing ---

(defun hash-password (password)
  "Hash PASSWORD using SHA256 with a random salt.
Returns a string in the format \"sha256:salt:hash\"."
  (let* ((salt-bytes (ironclad:random-data 16))
         (salt-hex (ironclad:byte-array-to-hex-string salt-bytes))
         (digest (ironclad:digest-sequence
                  :sha256
                  (flexi-streams:string-to-octets
                   (concatenate 'string salt-hex password)
                   :external-format :utf-8)))
         (hash-hex (ironclad:byte-array-to-hex-string digest)))
    (format nil "sha256:~a:~a" salt-hex hash-hex)))

(defun verify-password (password hash-string)
  "Verify PASSWORD against HASH-STRING (format: \"sha256:salt:hash\").
Returns T if the password matches."
  (let* ((parts (split-sequence:split-sequence #\: hash-string :count 3))
         (method (first parts))
         (salt (second parts))
         (stored-hash (third parts)))
    (when (and (string= method "sha256") salt stored-hash)
      (let* ((digest (ironclad:digest-sequence
                      :sha256
                      (flexi-streams:string-to-octets
                       (concatenate 'string salt password)
                       :external-format :utf-8)))
             (computed (ironclad:byte-array-to-hex-string digest)))
        (string= computed stored-hash)))))

;;; --- Lookups ---

(defun find-user (name &optional (config *config*))
  "Find user by NAME in CONFIG."
  (find name (config-users config)
        :key #'user-name :test #'string-equal))

(defun find-network (user-name network-name &optional (config *config*))
  "Find NETWORK-NAME under USER-NAME in CONFIG."
  (alex:when-let ((user (find-user user-name config)))
    (find network-name (user-networks user)
          :key #'network-name :test #'string-equal)))
