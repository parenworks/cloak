;;;; web/app.lisp - Fluxion web application for CLoak admin

(in-package #:cloak.web)

(defvar *web-app* nil "The running Fluxion web app instance.")

;;; -------------------------------------------------------
;;; CSS (embedded for single-binary deployment)
;;; -------------------------------------------------------

(defvar *css-path*
  (merge-pathnames "web/static/cloak.css"
                   (asdf:system-source-directory "cloak"))
  "Path to the CLoak CSS file (used at build time).")

(defvar *embedded-css* nil
  "CSS string embedded at load time for standalone deployment.")

(defun load-css ()
  "Return embedded CSS, or load from file as fallback."
  (or *embedded-css*
      (if (probe-file *css-path*)
          (alex:read-file-into-string *css-path*)
          "")))

;; Embed CSS at load time so the binary doesn't need source files
(setf *embedded-css*
      (if (probe-file *css-path*)
          (alex:read-file-into-string *css-path*)
          nil))

;; Embed Fluxion JS at load time for standalone deployment
(defvar *embedded-fluxion-js* (fluxion.client:client-js-string)
  "Fluxion client JS embedded at build time.")

;;; -------------------------------------------------------
;;; Page rendering
;;; -------------------------------------------------------

(defun render-app-page (session)
  "Render a full app page using the app-shell component."
  (let ((shell (server:session-component session "app-shell")))
    (render:render-page
     :title (format nil "CLoak - ~a" (string-capitalize (shell-current-page shell)))
     :csrf-token (server:session-csrf-token session)
     :head-html (format nil "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\"><link href=\"https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;700&display=swap\" rel=\"stylesheet\"><style>~a</style>" (load-css))
     :body-html (comp:render shell))))

(defun setup-shell (session page-name)
  "Configure the app-shell for the given page and return the session."
  (let* ((shell (server:session-component session "app-shell"))
         (comp-id (page-component-id page-name))
         (content (server:session-component session comp-id)))
    ;; Remove old child from composition tree
    (when (shell-content shell)
      (comp:remove-child shell (shell-content shell)))
    (setf (shell-current-page shell) page-name)
    (setf (shell-user-name shell) (or (server:session-user session) ""))
    (setf (shell-content shell) content)
    ;; Wire new child into composition tree
    (when content
      (comp:add-child shell content))
    session))

(defun page-component-id (page-name)
  "Map a page name to its session component ID."
  (cond
    ((string= page-name "dashboard") "dashboard")
    ((string= page-name "networks") "networks-page")
    ((string= page-name "users") "users-page")
    ((string= page-name "user-detail") "user-detail")
    ((string= page-name "network-edit") "network-edit")
    ((string= page-name "buffers") "buffers-page")
    ((string= page-name "modules") "modules-page")
    ((string= page-name "config") "config-page")
    (t "dashboard")))

(defun render-login-page (session)
  "Render the login page."
  (let ((login (server:session-component session "login-form")))
    (render:render-page
     :title "CLoak - Sign In"
     :csrf-token (server:session-csrf-token session)
     :head-html (format nil "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\"><link href=\"https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;700&display=swap\" rel=\"stylesheet\"><style>~a</style>" (load-css))
     :body-html (comp:render login))))

(defun render-change-password-page (session)
  "Render the change password page."
  (let ((form (server:session-component session "change-password")))
    (render:render-page
     :title "CLoak - Change Password"
     :csrf-token (server:session-csrf-token session)
     :head-html (format nil "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\"><link href=\"https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;700&display=swap\" rel=\"stylesheet\"><style>~a</style>" (load-css))
     :body-html (comp:render form))))

(defun must-change-password-p (session)
  "Return T if the logged-in user still has the default password."
  (let* ((username (server:session-user session))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b)))
         (user-cfg (when cfg (config:find-user username cfg))))
    (and user-cfg (config:default-password-p user-cfg))))

;;; -------------------------------------------------------
;;; Actions
;;; -------------------------------------------------------

;;; --- App shell navigation ---

(comp:defaction app-shell :navigate (c params)
  (let* ((page (cdr (assoc :page params)))
         (session (comp:component-session c)))
    (when (and page session)
      (let ((new-content (server:session-component session (page-component-id page))))
        ;; Remove old child, wire new one
        (when (shell-content c)
          (comp:remove-child c (shell-content c)))
        (setf (shell-current-page c) page)
        (setf (shell-content c) new-content)
        (when new-content
          (comp:add-child c new-content)))))
  nil)

(comp:defaction app-shell :navigate-user (c params)
  (let* ((username (cdr (assoc :username params)))
         (session (comp:component-session c)))
    (when (and username session)
      (let ((detail (server:session-component session "user-detail")))
        (when (shell-content c)
          (comp:remove-child c (shell-content c)))
        (setf (detail-target-user detail) username)
        (setf (detail-message detail) nil)
        (setf (shell-current-page c) "user-detail")
        (setf (shell-content c) detail)
        (comp:add-child c detail))))
  nil)

(comp:defaction app-shell :navigate-network (c params)
  (let* ((username (cdr (assoc :username params)))
         (netname (cdr (assoc :network params)))
         (session (comp:component-session c)))
    (when (and username netname session)
      (let ((editor (server:session-component session "network-edit")))
        (when (shell-content c)
          (comp:remove-child c (shell-content c)))
        (setf (netedit-target-user editor) username)
        (setf (netedit-target-network editor) netname)
        (setf (netedit-message editor) nil)
        (setf (shell-current-page c) "network-edit")
        (setf (shell-content c) editor)
        (comp:add-child c editor))))
  nil)

;;; --- Auth actions ---

(comp:defaction login-form :login (c params)
  (let* ((username (cdr (assoc :username params)))
         (password (cdr (assoc :password params)))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b)))
         (user-cfg (when cfg
                     (config:find-user username cfg))))
    (if (and user-cfg
             (config:verify-password password
                                     (config:user-password-hash user-cfg)))
        ;; Auth success - authenticate session and redirect
        (let ((session (comp:component-session c)))
          (when session
            (server:authenticate session username))
          (list (events:make-redirect-event "/")))
        ;; Auth failure
        (progn
          (setf (login-error-message c) "Invalid username or password.")
          nil))))

(comp:defaction config-page :save (c)
  c ; component already marked dirty by defaction
  (let ((b (bouncer-instance)))
    (when b
      (handler-case
          (progn
            (config:save-config (bouncer:bouncer-config b))
            (setf (config-message c) "Configuration saved to disk.")
            nil)
        (error (e)
          (setf (config-message c) (format nil "Save failed: ~a" e))
          nil)))))

(comp:defaction config-page :reload (c)
  c ; component already marked dirty by defaction
  (let ((b (bouncer-instance)))
    (when b
      (handler-case
          (let ((new-cfg (config:load-config)))
            (setf (bouncer:bouncer-config b) new-cfg)
            (setf (config-message c) "Configuration reloaded from disk.")
            nil)
        (error (e)
          (setf (config-message c) (format nil "Reload failed: ~a" e))
          nil)))))

(comp:defaction config-page :save-listener (c params)
  c ; component already marked dirty by defaction
  (let ((b (bouncer-instance)))
    (when b
      (let ((cfg (bouncer:bouncer-config b)))
        (setf (config:config-listen-host cfg)
              (or (cdr (assoc :listen-host params)) "0.0.0.0"))
        (setf (config:config-listen-port cfg)
              (parse-integer (or (cdr (assoc :listen-port params)) "6697") :junk-allowed t))
        (setf (config:config-listen-tls cfg)
              (string-equal "yes" (cdr (assoc :listen-tls params))))
        (let ((cert (cdr (assoc :tls-cert params)))
              (key (cdr (assoc :tls-key params))))
          (setf (config:config-tls-cert cfg)
                (if (or (null cert) (string= cert "")) nil cert))
          (setf (config:config-tls-key cfg)
                (if (or (null key) (string= key "")) nil key)))
        (handler-case
            (progn
              (config:save-config cfg)
              (setf (config-message c) "Listener settings saved. Restart CLoak to apply."))
          (error (e)
            (setf (config-message c) (format nil "Save failed: ~a" e)))))))
  nil)

(comp:defaction config-page :save-web (c params)
  c ; component already marked dirty by defaction
  (let ((b (bouncer-instance)))
    (when b
      (let ((cfg (bouncer:bouncer-config b)))
        (setf (config:config-web-host cfg)
              (or (cdr (assoc :web-host params)) "127.0.0.1"))
        (setf (config:config-web-port cfg)
              (parse-integer (or (cdr (assoc :web-port params)) "8076") :junk-allowed t))
        (let ((level (cdr (assoc :log-level params))))
          (setf (config:config-log-level cfg)
                (cond ((string-equal level "debug") :debug)
                      ((string-equal level "info") :info)
                      ((string-equal level "warn") :warn)
                      ((string-equal level "error") :error)
                      (t :info))))
        (handler-case
            (progn
              (config:save-config cfg)
              (setf (config-message c) "Web admin settings saved. Restart CLoak to apply."))
          (error (e)
            (setf (config-message c) (format nil "Save failed: ~a" e)))))))
  nil)

(comp:defaction config-page :save-playback (c params)
  c ; component already marked dirty by defaction
  (let ((b (bouncer-instance)))
    (when b
      (let* ((cfg (bouncer:bouncer-config b))
             (raw (cdr (assoc :playback-lines params)))
             (n (and raw (parse-integer raw :junk-allowed t))))
        (when (and n (>= n 0))
          (setf (config:config-playback-lines cfg) n))
        (handler-case
            (progn
              (config:save-config cfg)
              (setf (config-message c)
                    (format nil "Playback set to ~d lines per channel. Applies on next attach."
                            (config:config-playback-lines cfg))))
          (error (e)
            (setf (config-message c) (format nil "Save failed: ~a" e)))))))
  nil)

(comp:defaction change-password-form :change (c params)
  (let* ((new-pw (cdr (assoc :new-password params)))
         (confirm (cdr (assoc :confirm-password params)))
         (b (bouncer-instance))
         (session (comp:component-session c))
         (username (when session (server:session-user session))))
    (cond
      ((or (null new-pw) (< (length new-pw) 6))
       (setf (change-pw-error c) "Password must be at least 6 characters.")
       nil)
      ((not (string= new-pw confirm))
       (setf (change-pw-error c) "Passwords do not match.")
       nil)
      ((and b username)
       (let* ((cfg (bouncer:bouncer-config b))
              (user-cfg (config:find-user username cfg)))
         (when user-cfg
           (setf (config:user-password-hash user-cfg)
                 (config:hash-password new-pw))
           (config:save-config cfg)
           (list (events:make-redirect-event "/")))))
      (t
       (setf (change-pw-error c) "Unable to change password.")
       nil))))

;;; --- User management actions ---

(defun save-bouncer-config ()
  "Save the current bouncer config to disk."
  (let ((b (bouncer-instance)))
    (when b
      (config:save-config (bouncer:bouncer-config b)))))

(defun parse-channels (channels-str)
  "Parse a comma-separated channel string into a list."
  (when (and channels-str (plusp (length channels-str)))
    (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
            (cl-ppcre:split "," channels-str))))

(comp:defaction users-page :add-user (c params)
  (let* ((username (cdr (assoc :username params)))
         (password (cdr (assoc :password params)))
         (admin-str (cdr (assoc :admin params)))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b))))
    (cond
      ((or (null username) (zerop (length username)))
       (setf (users-message c) "Username is required.")
       nil)
      ((or (null password) (< (length password) 6))
       (setf (users-message c) "Password must be at least 6 characters.")
       nil)
      ((and cfg (config:find-user username cfg))
       (setf (users-message c) (format nil "User '~a' already exists." username))
       nil)
      (cfg
       (let ((new-user (make-instance 'config:user-config
                         :name username
                         :password-hash (config:hash-password password)
                         :admin-p (string-equal admin-str "yes"))))
         (push new-user (config:config-users cfg))
         (save-bouncer-config)
         (setf (users-message c) (format nil "User '~a' created." username))
         nil))
      (t
       (setf (users-message c) "Bouncer not running.")
       nil))))

(comp:defaction users-page :delete-user (c params)
  (let* ((username (cdr (assoc :username params)))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b))))
    (when (and cfg username)
      (setf (config:config-users cfg)
            (remove username (config:config-users cfg)
                   :key #'config:user-name :test #'string-equal))
      (save-bouncer-config)
      (setf (users-message c) (format nil "User '~a' deleted." username)))
    nil))

(comp:defaction user-detail :save-user (c params)
  (let* ((username (cdr (assoc :username params)))
         (password (cdr (assoc :password params)))
         (admin-str (cdr (assoc :admin params)))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b)))
         (user-cfg (when cfg (config:find-user username cfg))))
    (when user-cfg
      (when (and password (plusp (length password)))
        (setf (config:user-password-hash user-cfg)
              (config:hash-password password)))
      (setf (config:user-admin-p user-cfg)
            (string-equal admin-str "yes"))
      (save-bouncer-config)
      (setf (detail-message c) "User settings saved."))
    nil))

(comp:defaction user-detail :add-network (c params)
  (let* ((username (cdr (assoc :username params)))
         (name (cdr (assoc :name params)))
         (server (cdr (assoc :server params)))
         (port-str (cdr (assoc :port params)))
         (nick (cdr (assoc :nick params)))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b)))
         (user-cfg (when cfg (config:find-user username cfg))))
    (cond
      ((null user-cfg)
       (setf (detail-message c) "User not found.")
       nil)
      ((or (null name) (zerop (length name)))
       (setf (detail-message c) "Network name is required.")
       nil)
      ((config:find-network username name cfg)
       (setf (detail-message c) (format nil "Network '~a' already exists." name))
       nil)
      (t
       (let ((net (make-instance 'config:network-config
                    :name name
                    :server (or server "irc.libera.chat")
                    :port (or (ignore-errors (parse-integer port-str)) 6697)
                    :tls t
                    :nick (or nick username))))
         (push net (config:user-networks user-cfg))
         (save-bouncer-config)
         (setf (detail-message c) (format nil "Network '~a' added." name))
         nil)))))

(comp:defaction user-detail :delete-network (c params)
  (let* ((username (cdr (assoc :username params)))
         (netname (cdr (assoc :network params)))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b)))
         (user-cfg (when cfg (config:find-user username cfg))))
    (when user-cfg
      (setf (config:user-networks user-cfg)
            (remove netname (config:user-networks user-cfg)
                   :key #'config:network-name :test #'string-equal))
      (save-bouncer-config)
      (setf (detail-message c) (format nil "Network '~a' deleted." netname)))
    nil))

(comp:defaction network-edit :save-network (c params)
  (let* ((username (cdr (assoc :username params)))
         (orig-name (cdr (assoc :orig-name params)))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b)))
         (net-cfg (when cfg (config:find-network username orig-name cfg))))
    (when net-cfg
      (let ((name (cdr (assoc :name params)))
            (server (cdr (assoc :server params)))
            (port-str (cdr (assoc :port params)))
            (tls-str (cdr (assoc :tls params)))
            (nick (cdr (assoc :nick params)))
            (alt-nick (cdr (assoc :alt-nick params)))
            (ident (cdr (assoc :ident params)))
            (realname (cdr (assoc :realname params)))
            (server-pass (cdr (assoc :server-pass params)))
            (sasl-str (cdr (assoc :sasl params)))
            (buf-str (cdr (assoc :buffer-size params)))
            (motd-str (cdr (assoc :block-motd params))))
        (when name (setf (config:network-name net-cfg) name))
        (when server (setf (config:network-server net-cfg) server))
        (when port-str
          (let ((p (ignore-errors (parse-integer port-str))))
            (when p (setf (config:network-port net-cfg) p))))
        (setf (config:network-tls net-cfg) (string-equal tls-str "yes"))
        (when nick (setf (config:network-nick net-cfg) nick))
        (setf (config:network-alt-nick net-cfg)
              (when (and alt-nick (plusp (length alt-nick))) alt-nick))
        (setf (config:network-username net-cfg)
              (when (and ident (plusp (length ident))) ident))
        (when realname (setf (config:network-realname net-cfg) realname))
        (when (and server-pass (plusp (length server-pass)))
          (setf (config:network-password net-cfg) server-pass))
        (setf (config:network-sasl net-cfg)
              (when (and sasl-str (plusp (length sasl-str))) sasl-str))
        (when buf-str
          (let ((bs (ignore-errors (parse-integer buf-str))))
            (when bs (setf (config:network-buffer-size net-cfg) bs))))
        (setf (config:network-block-motd net-cfg) (string-equal motd-str "yes"))
        (save-bouncer-config)
        (setf (netedit-message c) "Network saved.")))
    nil))

;;; --- Channel management actions ---

(comp:defaction network-edit :add-channel (c params)
  (let* ((username (cdr (assoc :username params)))
         (netname (cdr (assoc :network params)))
         (channel (cdr (assoc :channel params)))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b)))
         (net-cfg (when cfg (config:find-network username netname cfg))))
    (cond
      ((null net-cfg)
       (setf (netedit-message c) "Network not found.")
       nil)
      ((or (null channel) (zerop (length channel)))
       (setf (netedit-message c) "Channel name is required.")
       nil)
      ((member channel (config:network-autojoin net-cfg) :test #'string-equal)
       (setf (netedit-message c) (format nil "Channel '~a' already exists." channel))
       nil)
      (t
       ;; Prefix with # if not already
       (unless (char= (char channel 0) #\#)
         (setf channel (concatenate 'string "#" channel)))
       (setf (config:network-autojoin net-cfg)
             (append (config:network-autojoin net-cfg) (list channel)))
       (save-bouncer-config)
       (setf (netedit-message c) (format nil "Channel '~a' added." channel))
       nil))))

(comp:defaction network-edit :delete-channel (c params)
  (let* ((username (cdr (assoc :username params)))
         (netname (cdr (assoc :network params)))
         (channel (cdr (assoc :channel params)))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b)))
         (net-cfg (when cfg (config:find-network username netname cfg))))
    (when net-cfg
      (setf (config:network-autojoin net-cfg)
            (remove channel (config:network-autojoin net-cfg) :test #'string-equal))
      (save-bouncer-config)
      (setf (netedit-message c) (format nil "Channel '~a' removed." channel)))
    nil))

;;; --- Module actions ---

(comp:defaction modules-page :enable (c params)
  (let* ((name (cdr (assoc :name params)))
         (b (bouncer-instance)))
    (when (and b name)
      (let ((mod (modules:load-module name b)))
        (if mod
            (progn
              ;; Persist to config
              (let ((cfg (bouncer:bouncer-config b)))
                (unless (member name (config:config-enabled-modules cfg) :test #'string-equal)
                  (push name (config:config-enabled-modules cfg))
                  (save-bouncer-config)))
              (setf (modules-message c) (format nil "Module '~a' enabled." name)))
            (setf (modules-message c) (format nil "Failed to enable module '~a'." name))))))
  nil)

(comp:defaction modules-page :disable (c params)
  (let* ((name (cdr (assoc :name params)))
         (b (bouncer-instance)))
    (when (and b name)
      (modules:unload-module name b)
      ;; Remove from config
      (let ((cfg (bouncer:bouncer-config b)))
        (setf (config:config-enabled-modules cfg)
              (remove name (config:config-enabled-modules cfg) :test #'string-equal))
        (save-bouncer-config))
      (setf (modules-message c) (format nil "Module '~a' disabled." name))
      ;; Close settings panel if showing this module
      (when (string-equal (modules-detail c) name)
        (setf (modules-detail c) nil))))
  nil)

(comp:defaction modules-page :show-settings (c params)
  (let ((name (cdr (assoc :name params))))
    (setf (modules-detail c) name)
    (setf (modules-message c) nil))
  nil)

(comp:defaction modules-page :hide-settings (c)
  (setf (modules-detail c) nil)
  (setf (modules-message c) nil)
  nil)

(comp:defaction modules-page :save-settings (c params)
  (let* ((name (cdr (assoc :module-name params)))
         (mod (when name (modules:active-module name))))
    (when mod
      (modules:on-save-settings mod params)
      (setf (modules-message c) (format nil "Settings saved for '~a'." name))))
  nil)

;;; --- Buffer actions ---

(comp:defaction buffers-page :expand (c params)
  (let ((key (cdr (assoc :key params))))
    (when key
      (setf (buffers-expanded c) key)))
  nil)

(comp:defaction buffers-page :collapse (c)
  (setf (buffers-expanded c) nil)
  nil)

(comp:defaction buffers-page :clear (c params)
  (let* ((key (cdr (assoc :key params)))
         (b (bouncer-instance)))
    (when (and key b)
      (let ((buf (gethash key (bouncer:bouncer-buffers b))))
        (when buf
          (cloak.buffer:buffer-clear buf)))
      (setf (buffers-expanded c) nil)))
  nil)

;;; --- Network state push ---

(defun session-sse-active-p (session)
  "Return T if SESSION has an active (non-closed) SSE event queue."
  (let ((q (server:session-event-queue session)))
    (and q (not (server:eq-closed-p q)))))

(defun push-network-state-to-all-sessions (&optional skip-component)
  "Push updated app-shell to all sessions with active SSE connections.
SKIP-COMPONENT, if provided, is excluded (it is patched by the action response)."
  (when *web-app*
    (maphash (lambda (sid session)
               (declare (ignore sid))
               (when (session-sse-active-p session)
                 (let ((shell (server:session-component session "app-shell")))
                   (when (and shell (not (eq shell skip-component)))
                     (handler-case
                         (server:push-component-patch session shell :mode "replace")
                       (error () nil))))))
             (server:app-sessions *web-app*))))

;;; --- Network connect/disconnect actions ---

(comp:defaction networks-page :connect (c params)
  c ; component marked dirty by defaction
  (let* ((key (cdr (assoc :key params)))
         (b (bouncer-instance)))
    (when (and b key)
      (let ((upstream (gethash key (bouncer:bouncer-upstreams b))))
        (when (and upstream
                   (eq (upstream:upstream-state upstream) :disconnected))
          (setf (upstream:upstream-state upstream) :registering)
          (setf (upstream:upstream-reconnect-attempts upstream) 0)
          ;; Connect in background; on-state-change callback handles UI push
          (bt:make-thread
           (lambda ()
             (let ((ok (upstream:upstream-connect upstream)))
               (if ok
                   (setf (upstream:upstream-reconnect-p upstream) t)
                   ;; Connection failed — push updated state (now :disconnected)
                   (push-network-state-to-all-sessions))))
           :name (format nil "cloak-reconnect-~a" key))))))
  nil)

(comp:defaction networks-page :disconnect (c params)
  c ; component marked dirty by defaction
  (let* ((key (cdr (assoc :key params)))
         (b (bouncer-instance)))
    (when (and b key)
      (let ((upstream (gethash key (bouncer:bouncer-upstreams b))))
        (when (and upstream
                   (not (eq (upstream:upstream-state upstream) :disconnected)))
          (setf (upstream:upstream-reconnect-p upstream) nil)
          (upstream:upstream-disconnect upstream :quit t)))))
  (push-network-state-to-all-sessions c)
  nil)

;;; -------------------------------------------------------
;;; Router setup
;;; -------------------------------------------------------

(defun setup-router ()
  "Create and configure the Fluxion router for CLoak."
  (let ((router (server:make-router)))

    ;; Login page
    (server:add-route router :get "/login"
      (lambda (app session env &key params)
        (declare (ignore app env params))
        (if (server:authenticated-p session)
            (list 303 '(:location "/") '("Redirecting"))
            (list 200 '(:content-type "text/html")
                  (list (render-login-page session))))))

    ;; Login action auth - need to hook into the session after action
    (server:add-route router :get "/logout"
      (lambda (app session env &key params)
        (declare (ignore app env params))
        (server:logout session)
        (list 303 '(:location "/login") nil)))

    ;; Change password page
    (server:add-route router :get "/change-password"
      (lambda (app session env &key params)
        (declare (ignore app env params))
        (or (server:require-auth session)
            (list 200 '(:content-type "text/html")
                  (list (render-change-password-page session)))))
      :guard (lambda (session)
               (server:require-auth session)))

    ;; Dashboard (auth required)
    (server:add-route router :get "/"
      (lambda (app session env &key params)
        (declare (ignore app env params))
        (or (server:require-auth session)
            (when (must-change-password-p session)
              (list 303 '(:location "/change-password") '("Change password")))
            (progn
              (setup-shell session "dashboard")
              (list 200 '(:content-type "text/html")
                    (list (render-app-page session))))))
      :guard (lambda (session)
               (server:require-auth session)))

    ;; Networks
    (server:add-route router :get "/networks"
      (lambda (app session env &key params)
        (declare (ignore app env params))
        (or (server:require-auth session)
            (progn
              (setup-shell session "networks")
              (list 200 '(:content-type "text/html")
                    (list (render-app-page session))))))
      :guard (lambda (session)
               (server:require-auth session)))

    ;; Users
    (server:add-route router :get "/users"
      (lambda (app session env &key params)
        (declare (ignore app env params))
        (or (server:require-auth session)
            (progn
              (setup-shell session "users")
              (list 200 '(:content-type "text/html")
                    (list (render-app-page session))))))
      :guard (lambda (session)
               (server:require-auth session)))

    ;; User detail
    (server:add-route router :get "/users/:name"
      (lambda (app session env &key params)
        (declare (ignore app env))
        (or (server:require-auth session)
            (let ((username (cdr (assoc :name params))))
              (setup-shell session "user-detail")
              (let ((detail (server:session-component session "user-detail")))
                (setf (detail-target-user detail) username)
                (setf (detail-message detail) nil))
              (list 200 '(:content-type "text/html")
                    (list (render-app-page session))))))
      :guard (lambda (session)
               (server:require-auth session)))

    ;; Network edit
    (server:add-route router :get "/users/:user/networks/:net"
      (lambda (app session env &key params)
        (declare (ignore app env))
        (or (server:require-auth session)
            (let ((username (cdr (assoc :user params)))
                  (netname (cdr (assoc :net params))))
              (setup-shell session "network-edit")
              (let ((editor (server:session-component session "network-edit")))
                (setf (netedit-target-user editor) username)
                (setf (netedit-target-network editor) netname)
                (setf (netedit-message editor) nil))
              (list 200 '(:content-type "text/html")
                    (list (render-app-page session))))))
      :guard (lambda (session)
               (server:require-auth session)))

    ;; Modules
    (server:add-route router :get "/modules"
      (lambda (app session env &key params)
        (declare (ignore app env params))
        (or (server:require-auth session)
            (progn
              (setup-shell session "modules")
              (list 200 '(:content-type "text/html")
                    (list (render-app-page session))))))
      :guard (lambda (session)
               (server:require-auth session)))

    ;; Buffers
    (server:add-route router :get "/buffers"
      (lambda (app session env &key params)
        (declare (ignore app env params))
        (or (server:require-auth session)
            (progn
              (setup-shell session "buffers")
              (list 200 '(:content-type "text/html")
                    (list (render-app-page session))))))
      :guard (lambda (session)
               (server:require-auth session)))

    ;; Config
    (server:add-route router :get "/config"
      (lambda (app session env &key params)
        (declare (ignore app env params))
        (or (server:require-auth session)
            (progn
              (setup-shell session "config")
              (list 200 '(:content-type "text/html")
                    (list (render-app-page session))))))
      :guard (lambda (session)
               (server:require-auth session)))

    router))

;;; -------------------------------------------------------
;;; Application lifecycle
;;; -------------------------------------------------------

(defun start-web-admin (host port &key (server :woo))
  "Start the CLoak web admin interface on HOST:PORT.
SERVER is the Clack backend: :woo (default) or :hunchentoot."
  (when *web-app*
    (stop-web-admin))

  (format t "[CLoak] Starting Fluxion web admin on http://~a:~d (~a)~%"
          host port server)

  ;; Write embedded fluxion.js to a temp static directory
  (let* ((static-dir (uiop:ensure-directory-pathname
                      (merge-pathnames "cloak/static/"
                                       (uiop:temporary-directory))))
         (js-path (merge-pathnames "fluxion.js" static-dir)))
    (ensure-directories-exist js-path)
    (with-open-file (out js-path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string *embedded-fluxion-js* out))
    (format t "Fluxion: client runtime written to ~a (~d bytes)~%"
            js-path (length *embedded-fluxion-js*))
    (setf *web-app* (server:make-fluxion-app
                     :port port
                     :static-dir static-dir
                     :server server))

    ;; Register per-session component factories
    (server:register-component-factory *web-app* "login-form"
      (lambda () (make-instance 'login-form)))
    (server:register-component-factory *web-app* "change-password"
      (lambda () (make-instance 'change-password-form)))
    (server:register-component-factory *web-app* "app-shell"
      (lambda () (make-instance 'app-shell)))
    (server:register-component-factory *web-app* "dashboard"
      (lambda () (make-instance 'dashboard)))
    (server:register-component-factory *web-app* "networks-page"
      (lambda () (make-instance 'networks-page)))
    (server:register-component-factory *web-app* "users-page"
      (lambda () (make-instance 'users-page)))
    (server:register-component-factory *web-app* "user-detail"
      (lambda () (make-instance 'user-detail)))
    (server:register-component-factory *web-app* "network-edit"
      (lambda () (make-instance 'network-edit)))
    (server:register-component-factory *web-app* "modules-page"
      (lambda () (make-instance 'modules-page)))
    (server:register-component-factory *web-app* "buffers-page"
      (lambda () (make-instance 'buffers-page)))
    (server:register-component-factory *web-app* "config-page"
      (lambda () (make-instance 'config-page)))

    ;; Setup router and start
    (let ((router (setup-router)))
      (server:start *web-app*
        (server:router-handler router)
        :port port
        :server server))

    ;; Install state-change hooks so upstream transitions push UI updates
    (install-upstream-state-hooks)

    ;; Start periodic dashboard refresh (uptime, stats)
    (start-dashboard-timer :interval 30)

    (format t "[CLoak] Fluxion web admin running~%")
    *web-app*))

;;; find-session-for-component removed - use (comp:component-session c) instead.

(defun install-upstream-state-hooks ()
  "Install on-state-change callbacks on all bouncer upstreams so that
UI updates are pushed automatically when connection state changes.
Chains with any existing callback (e.g. module disconnect hooks)."
  (let ((b (bouncer-instance)))
    (when b
      (maphash (lambda (key upstream)
                 (declare (ignore key))
                 (let ((prev (upstream:upstream-on-state-change upstream)))
                   (setf (upstream:upstream-on-state-change upstream)
                         (lambda (us new-state)
                           ;; Run previous callback (module hooks) first
                           (when prev
                             (handler-case (funcall prev us new-state)
                               (error (e)
                                 (format t "[CLoak] State hook error: ~a~%" e))))
                           ;; Then push UI updates
                           (push-network-state-to-all-sessions)))))
               (bouncer:bouncer-upstreams b))
      (format t "[CLoak] Installed state-change hooks on ~d upstreams~%"
              (hash-table-count (bouncer:bouncer-upstreams b)))
      (force-output))))

;;; --- Dashboard refresh timer ---

(defvar *dashboard-timer* nil "Thread that periodically refreshes dashboard.")

(defun start-dashboard-timer (&key (interval 30))
  "Start a background thread that refreshes dashboard components every INTERVAL seconds.
Only pushes to sessions with an active SSE connection."
  (when *dashboard-timer*
    (bt:destroy-thread *dashboard-timer*)
    (setf *dashboard-timer* nil))
  (setf *dashboard-timer*
        (bt:make-thread
         (lambda ()
           (loop
             (sleep interval)
             (handler-case
                 (when *web-app*
                   (maphash (lambda (sid session)
                              (declare (ignore sid))
                              (when (session-sse-active-p session)
                                (let ((shell (server:session-component session "app-shell")))
                                  (when shell
                                    (handler-case
                                        (server:push-component-patch session shell :mode "replace")
                                      (error () nil))))))
                            (server:app-sessions *web-app*)))
               (error () nil))))
         :name "cloak-dashboard-timer")))

(defun stop-dashboard-timer ()
  "Stop the dashboard refresh timer."
  (when (and *dashboard-timer* (bt:thread-alive-p *dashboard-timer*))
    (bt:destroy-thread *dashboard-timer*)
    (setf *dashboard-timer* nil)))

(defun stop-web-admin ()
  "Stop the CLoak web admin interface."
  (when *web-app*
    (format t "[CLoak] Stopping web admin~%")
    (stop-dashboard-timer)
    (handler-case
        (server:stop *web-app*)
      (error (e)
        (format t "[CLoak] Web admin stop: ~a~%" e)))
    (setf *web-app* nil)))
