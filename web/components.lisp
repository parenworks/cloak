;;;; web/components.lisp - Fluxion components for CLoak web admin

(in-package #:cloak.web)

;;; -------------------------------------------------------
;;; Helpers
;;; -------------------------------------------------------

(defun bouncer-instance ()
  "Return the active bouncer instance."
  bouncer:*bouncer*)

(defun format-uptime (start-time)
  "Format uptime from START-TIME (universal time) to human string."
  (let* ((elapsed (- (get-universal-time) start-time))
         (days (floor elapsed 86400))
         (hours (floor (mod elapsed 86400) 3600))
         (mins (floor (mod elapsed 3600) 60))
         (secs (mod elapsed 60)))
    (format nil "~dd ~dh ~dm ~ds" days hours mins secs)))

(defun network-status (upstream)
  "Return status keyword for an upstream connection."
  (if upstream
      (let ((state (upstream:upstream-state upstream)))
        (cond
          ((eq state :connected) :connected)
          ((eq state :registering) :connecting)
          (t :disconnected)))
      :disconnected))

(defun status-class (status)
  "Return CSS class for a status keyword."
  (ecase status
    (:connected "connected")
    (:disconnected "disconnected")
    (:connecting "connecting")))

(defun status-label (status)
  "Return display label for a status keyword."
  (ecase status
    (:connected "Connected")
    (:disconnected "Disconnected")
    (:connecting "Connecting")))

(defun badge-class (status)
  "Return badge CSS class for a status keyword."
  (ecase status
    (:connected "badge badge-green")
    (:disconnected "badge badge-red")
    (:connecting "badge badge-amber")))

;;; -------------------------------------------------------
;;; Login Component
;;; -------------------------------------------------------

(comp:defcomponent login-form
  :id "login-form"
  :slots ((error-message :initform nil :accessor login-error-message))
  :render
  (spinneret:with-html-string
    (:div :id (comp:component-id self) :class "login-page"
      (:div :class "login-box"
        (:h1 "CLoak")
        (:p :class "tagline" "IRC Bouncer Admin")
        (when (login-error-message self)
          (:div :class "login-error" (login-error-message self)))
        (:form :data-on-submit "/action/login-form/login"
          (:div :class "form-group"
            (:label :class "form-label" :for "username" "Username")
            (:input :class "form-input" :type "text" :name "username"
                    :id "username" :placeholder "admin" :autofocus t
                    :autocomplete "username"))
          (:div :class "form-group"
            (:label :class "form-label" :for "password" "Password")
            (:input :class "form-input" :type "password" :name "password"
                    :id "password" :placeholder "password"
                    :autocomplete "current-password"))
          (:button :type "submit" :class "login-btn"
                   :data-disable-during-request t
                   "Sign In"))))))

;;; -------------------------------------------------------
;;; Sidebar Component
;;; -------------------------------------------------------

(comp:defcomponent app-shell
  :id "app-shell"
  :slots ((current-page :initform "dashboard" :accessor shell-current-page)
          (user-name :initform "" :accessor shell-user-name)
          (content :initform nil :accessor shell-content))
  :render
  (let ((page (shell-current-page self)))
    (spinneret:with-html-string
      (:div :id (comp:component-id self) :class "app-shell"
        (:nav :class "sidebar"
          (:div :class "sidebar-brand"
            (:h1 "CLoak")
            (:div :class "version"
              (format nil "v~a" cloak:*version*)))
          (:div :class "sidebar-nav"
            (:div :class "nav-section" "Monitor")
            (:a :class (if (string= page "dashboard") "nav-item active" "nav-item")
                :data-on-click "/action/app-shell/navigate"
                :data-param-page "dashboard"
              (:span :class "icon" (:raw "&#9632;"))
              "Dashboard")
            (:a :class (if (string= page "networks") "nav-item active" "nav-item")
                :data-on-click "/action/app-shell/navigate"
                :data-param-page "networks"
              (:span :class "icon" (:raw "&#9741;"))
              "Networks")
            (:a :class (if (string= page "buffers") "nav-item active" "nav-item")
                :data-on-click "/action/app-shell/navigate"
                :data-param-page "buffers"
              (:span :class "icon" (:raw "&#9776;"))
              "Buffers")
            (:div :class "nav-section" "Manage")
            (:a :class (if (string= page "modules") "nav-item active" "nav-item")
                :data-on-click "/action/app-shell/navigate"
                :data-param-page "modules"
              (:span :class "icon" (:raw "&#9830;"))
              "Modules")
            (:a :class (if (string= page "users") "nav-item active" "nav-item")
                :data-on-click "/action/app-shell/navigate"
                :data-param-page "users"
              (:span :class "icon" (:raw "&#9679;"))
              "Users")
            (:a :class (if (string= page "config") "nav-item active" "nav-item")
                :data-on-click "/action/app-shell/navigate"
                :data-param-page "config"
              (:span :class "icon" (:raw "&#9881;"))
              "Config"))
          (:div :class "sidebar-footer"
            (:div :class "user-name" (shell-user-name self))
            (:a :href "/logout" :class "nav-item" :style "padding: 0.3rem 0; margin-top: 0.25rem;"
              "Sign out")))
        (:main :class "main-content"
          (when (shell-content self)
            (:raw (comp:render (shell-content self)))))))))

;;; -------------------------------------------------------
;;; Dashboard Component
;;; -------------------------------------------------------

(comp:defcomponent dashboard
  :id "dashboard"
  :render
  (let* ((b (bouncer-instance))
         (upstreams (when b (bouncer:bouncer-upstreams b)))
         (clients (when b (bouncer:bouncer-clients b)))
         (total-nets 0)
         (connected-nets 0)
         (total-channels 0)
         (total-buffers 0))
    ;; Count stats
    (when upstreams
      (maphash (lambda (key upstream)
                 (declare (ignore key))
                 (incf total-nets)
                 (when (upstream:upstream-connected-p upstream)
                   (incf connected-nets)
                   (incf total-channels
                         (hash-table-count (upstream:upstream-channels upstream)))))
               upstreams))
    (when b
      (setf total-buffers (hash-table-count (bouncer:bouncer-buffers b))))
    (spinneret:with-html-string
      (:div :id (comp:component-id self)
        ;; Stats grid
        (:div :class "stats-grid"
          ;; Uptime
          (:div :class "stat-card"
            (:div :class "stat-label" "Uptime")
            (:div :class "stat-value green"
              (if b
                  (format-uptime (bouncer:bouncer-start-time b))
                  "Offline")))
          ;; Networks
          (:div :class "stat-card"
            (:div :class "stat-label" "Networks")
            (:div :class "stat-value blue"
              (format nil "~d" connected-nets))
            (:div :class "stat-detail"
              (format nil "~d/~d connected" connected-nets total-nets)))
          ;; Clients
          (:div :class "stat-card"
            (:div :class "stat-label" "Clients")
            (:div :class "stat-value amber"
              (format nil "~d" (length clients))))
          ;; Channels
          (:div :class "stat-card"
            (:div :class "stat-label" "Channels")
            (:div :class "stat-value"
              (format nil "~d" total-channels)))
          ;; Buffers
          (:div :class "stat-card"
            (:div :class "stat-label" "Buffers")
            (:div :class "stat-value"
              (format nil "~d" total-buffers))))

        ;; Networks table
        (:div :class "card mt-2"
          (:div :class "card-header"
            (:span :class "card-title" "Networks"))
          (if (and upstreams (plusp (hash-table-count upstreams)))
              (:table :class "data-table"
                (:thead
                  (:tr
                    (:th "Status")
                    (:th "Network")
                    (:th "Server")
                    (:th "Nick")
                    (:th "Channels")))
                (:tbody
                  (maphash
                   (lambda (key upstream)
                     (declare (ignore key))
                     (let ((status (network-status upstream)))
                       (:tr
                         (:td (:span :class (badge-class status) (status-label status)))
                         (:td :class "mono" (upstream:upstream-network-name upstream))
                         (:td :class "mono"
                           (format nil "~a:~d"
                                   (config:network-server (upstream:upstream-config upstream))
                                   (config:network-port (upstream:upstream-config upstream))))
                         (:td :class "mono" (upstream:upstream-nick upstream))
                         (:td (format nil "~d"
                                      (hash-table-count
                                       (upstream:upstream-channels upstream)))))))
                   upstreams)))
              (:p :style "color: var(--text-muted); padding: 1rem 0;"
                "No networks configured.")))

        ;; Connected clients
        (:div :class "card mt-2"
          (:div :class "card-header"
            (:span :class "card-title" "Connected Clients"))
          (if (and clients (plusp (length clients)))
              (:table :class "data-table"
                (:thead
                  (:tr
                    (:th "Nick")
                    (:th "Network")
                    (:th "Authenticated")))
                (:tbody
                  (dolist (client clients)
                    (:tr
                      (:td :class "mono" (or (downstream:client-nick client) "-"))
                      (:td :class "mono" (or (downstream:client-network client) "-"))
                      (:td (if (downstream:client-authenticated-p client)
                               (:span :class "badge badge-green" "Yes")
                               (:span :class "badge badge-red" "No")))))))
              (:p :style "color: var(--text-muted); padding: 1rem 0;"
                "No clients connected.")))))))

;;; -------------------------------------------------------
;;; Networks Component
;;; -------------------------------------------------------

(comp:defcomponent networks-page
  :id "networks-page"
  :render
  (let* ((b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b))))
    (spinneret:with-html-string
      (:div :id (comp:component-id self)
        (:div :class "page-header"
          (:h2 "Networks")
          (:p :class "subtitle" "Manage IRC network connections"))
        (when cfg
          (dolist (user (config:config-users cfg))
            (:div :class "card mb-2"
              (:div :class "card-header"
                (:span :class "card-title"
                  (format nil "~a's networks" (config:user-name user))))
              (:div :class "network-list"
                (if (config:user-networks user)
                    (dolist (net (config:user-networks user))
                      (let* ((key (format nil "~a/~a"
                                          (config:user-name user)
                                          (config:network-name net)))
                             (upstream (when b
                                         (gethash key (bouncer:bouncer-upstreams b))))
                             (status (network-status upstream)))
                        (:div :class "network-row"
                          (:div :class "network-info"
                            (:span :class (format nil "status-dot ~a" (status-class status)))
                            (:div
                              (:div :class "network-name" (config:network-name net))
                              (:div :class "network-server"
                                (format nil "~a:~d~a"
                                        (config:network-server net)
                                        (config:network-port net)
                                        (if (config:network-tls net) " (TLS)" "")))))
                          (:div :class "network-actions"
                            (ecase status
                              (:connected
                               (:button :class "btn btn-sm btn-danger"
                                        :data-on-click "/action/networks-page/disconnect"
                                        :data-param-key key
                                        :data-disable-during-request t
                                        "Disconnect"))
                              (:connecting
                               (:button :class "btn btn-sm" :disabled "disabled"
                                        "Connecting..."))
                              (:disconnected
                               (:button :class "btn btn-sm btn-primary"
                                        :data-on-click "/action/networks-page/connect"
                                        :data-param-key key
                                        :data-disable-during-request t
                                        "Connect")))))))))
                    (:p :style "color: var(--text-muted);"
                      "No networks configured."))))))))

;;; -------------------------------------------------------
;;; Users Component
;;; -------------------------------------------------------

(comp:defcomponent users-page
  :id "users-page"
  :slots ((message :initform nil :accessor users-message))
  :render
  (let* ((b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b))))
    (spinneret:with-html-string
      (:div :id (comp:component-id self)
        (:div :class "page-header"
          (:h2 "Users")
          (:p :class "subtitle" "Manage bouncer users"))
        (when (users-message self)
          (:div :class "alert" (users-message self)))
        ;; User list
        (:div :class "card"
          (:table :class "data-table"
            (:thead
              (:tr
                (:th "Username")
                (:th "Admin")
                (:th "Networks")
                (:th "Actions")))
            (:tbody
              (when cfg
                (dolist (user (config:config-users cfg))
                  (:tr
                    (:td (:a :class "mono link"
                             :data-on-click "/action/app-shell/navigate-user"
                             :data-param-username (config:user-name user)
                           (config:user-name user)))
                    (:td (if (config:user-admin-p user)
                             (:span :class "badge badge-amber" "Admin")
                             (:span :class "badge badge-green" "User")))
                    (:td (format nil "~d" (length (config:user-networks user))))
                    (:td :class "flex gap-1"
                      (:button :class "btn btn-sm"
                               :data-on-click "/action/app-shell/navigate-user"
                               :data-param-username (config:user-name user)
                               "Edit")
                      (:button :class "btn btn-sm btn-danger"
                               :data-on-click "/action/users-page/delete-user"
                               :data-param-username (config:user-name user)
                               :data-disable-during-request t
                               "Delete"))))))))
        ;; Add user form
        (:div :class "card mt-2"
          (:div :class "card-header"
            (:span :class "card-title" "Add User"))
          (:form :class "form-inline" :data-on-submit "/action/users-page/add-user"
            (:div :class "form-row"
              (:div :class "form-group"
                (:label :class "form-label" "Username")
                (:input :class "form-input" :type "text" :name "username"
                        :placeholder "newuser" :required t))
              (:div :class "form-group"
                (:label :class "form-label" "Password")
                (:input :class "form-input" :type "password" :name "password"
                        :placeholder "password" :required t :minlength "6"))
              (:div :class "form-group"
                (:label :class "form-label" "Admin?")
                (:select :class "form-input" :name "admin"
                  (:option :value "no" "No")
                  (:option :value "yes" "Yes")))
              (:div :class "form-group" :style "align-self: flex-end;"
                (:button :type "submit" :class "btn btn-primary" "Add User")))))))))

;;; -------------------------------------------------------
;;; User Detail Component
;;; -------------------------------------------------------

(comp:defcomponent user-detail
  :id "user-detail"
  :slots ((target-user :initarg :target-user :initform nil :accessor detail-target-user)
          (message :initform nil :accessor detail-message))
  :render
  (let* ((username (detail-target-user self))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b)))
         (user-cfg (when (and cfg username)
                     (config:find-user username cfg))))
    (spinneret:with-html-string
      (:div :id (comp:component-id self)
        (:div :class "page-header"
          (:div :class "flex" :style "align-items: center; gap: 1rem;"
            (:button :class "btn btn-sm"
                     :data-on-click "/action/app-shell/navigate"
                     :data-param-page "users"
                     "Back")
            (:div
              (:h2 (or username "User"))
              (:p :class "subtitle" "User settings and networks"))))
        (when (detail-message self)
          (:div :class "alert" (detail-message self)))
        (if (null user-cfg)
            (:div :class "card"
              (:p :style "padding: 2rem; text-align: center; color: var(--surface2);"
                "User not found."))
            (progn
              ;; User settings card
              (:div :class "card"
                (:div :class "card-header"
                  (:span :class "card-title" "User Settings"))
                (:form :class "form-inline" :data-on-submit "/action/user-detail/save-user"
                  (:input :type "hidden" :name "username" :value username)
                  (:div :class "form-row"
                    (:div :class "form-group"
                      (:label :class "form-label" "New Password (leave blank to keep)")
                      (:input :class "form-input" :type "password" :name "password"
                              :placeholder "unchanged"))
                    (:div :class "form-group"
                      (:label :class "form-label" "Admin?")
                      (:select :class "form-input" :name "admin"
                        (:option :value "no"
                          :selected (unless (config:user-admin-p user-cfg) "selected")
                          "No")
                        (:option :value "yes"
                          :selected (when (config:user-admin-p user-cfg) "selected")
                          "Yes")))
                    (:div :class "form-group" :style "align-self: flex-end;"
                      (:button :type "submit" :class "btn btn-primary" "Save")))))

              ;; Networks card
              (:div :class "card mt-2"
                (:div :class "card-header"
                  (:span :class "card-title" "Networks"))
                (if (config:user-networks user-cfg)
                    (:table :class "data-table"
                      (:thead
                        (:tr
                          (:th "Network")
                          (:th "Server")
                          (:th "Nick")
                          (:th "TLS")
                          (:th "Channels")
                          (:th "Actions")))
                      (:tbody
                        (dolist (net (config:user-networks user-cfg))
                          (:tr
                            (:td (:a :class "mono link"
                                     :data-on-click "/action/app-shell/navigate-network"
                                     :data-param-username username
                                     :data-param-network (config:network-name net)
                                   (config:network-name net)))
                            (:td :class "mono"
                              (format nil "~a:~d" (config:network-server net)
                                      (config:network-port net)))
                            (:td :class "mono" (or (config:network-nick net) "-"))
                            (:td (if (config:network-tls net)
                                     (:span :class "badge badge-green" "Yes")
                                     (:span :class "badge badge-red" "No")))
                            (:td (format nil "~d" (length (config:network-autojoin net))))
                            (:td :class "flex gap-1"
                              (:button :class "btn btn-sm"
                                       :data-on-click "/action/app-shell/navigate-network"
                                       :data-param-username username
                                       :data-param-network (config:network-name net)
                                       "Edit")
                              (:button :class "btn btn-sm btn-danger"
                                       :data-on-click "/action/user-detail/delete-network"
                                       :data-param-username username
                                       :data-param-network (config:network-name net)
                                       "Delete"))))))
                    (:p :style "padding: 1rem; color: var(--surface2);"
                      "No networks configured.")))

              ;; Add network form
              (:div :class "card mt-2"
                (:div :class "card-header"
                  (:span :class "card-title" "Add Network"))
                (:form :class "form-inline" :data-on-submit "/action/user-detail/add-network"
                  (:input :type "hidden" :name "username" :value username)
                  (:div :class "form-row"
                    (:div :class "form-group"
                      (:label :class "form-label" "Name")
                      (:input :class "form-input" :type "text" :name "name"
                              :placeholder "libera" :required t))
                    (:div :class "form-group"
                      (:label :class "form-label" "Server")
                      (:input :class "form-input" :type "text" :name "server"
                              :placeholder "irc.libera.chat" :required t))
                    (:div :class "form-group"
                      (:label :class "form-label" "Port")
                      (:input :class "form-input" :type "number" :name "port"
                              :value "6697"))
                    (:div :class "form-group"
                      (:label :class "form-label" "Nick")
                      (:input :class "form-input" :type "text" :name "nick"
                              :placeholder "mynick" :required t))
                    (:div :class "form-group" :style "align-self: flex-end;"
                      (:button :type "submit" :class "btn btn-primary" "Add Network")))))))))))

;;; -------------------------------------------------------
;;; Network Edit Component
;;; -------------------------------------------------------

(comp:defcomponent network-edit
  :id "network-edit"
  :slots ((target-user :initarg :target-user :initform nil :accessor netedit-target-user)
          (target-network :initarg :target-network :initform nil :accessor netedit-target-network)
          (message :initform nil :accessor netedit-message))
  :render
  (let* ((username (netedit-target-user self))
         (netname (netedit-target-network self))
         (b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b)))
         (net-cfg (when (and cfg username netname)
                    (config:find-network username netname cfg))))
    (spinneret:with-html-string
      (:div :id (comp:component-id self)
        (:div :class "page-header"
          (:div :class "flex" :style "align-items: center; gap: 1rem;"
            (:button :class "btn btn-sm"
                     :data-on-click "/action/app-shell/navigate-user"
                     :data-param-username username
                     "Back")
            (:div
              (:h2 (format nil "~a / ~a" (or username "") (or netname "")))
              (:p :class "subtitle" "Network configuration"))))
        (when (netedit-message self)
          (:div :class "alert" (netedit-message self)))
        (if (null net-cfg)
            (:div :class "card"
              (:p :style "padding: 2rem; text-align: center; color: var(--surface2);"
                "Network not found."))
            (:div :class "card"
              (:div :class "card-header"
                (:span :class "card-title" "Connection"))
              (:form :data-on-submit "/action/network-edit/save-network"
                (:input :type "hidden" :name "username" :value username)
                (:input :type "hidden" :name "orig-name" :value netname)
                (:div :class "form-grid"
                  (:div :class "form-group"
                    (:label :class "form-label" "Network Name")
                    (:input :class "form-input" :type "text" :name "name"
                            :value (config:network-name net-cfg) :required t))
                  (:div :class "form-group"
                    (:label :class "form-label" "Server")
                    (:input :class "form-input" :type "text" :name "server"
                            :value (config:network-server net-cfg) :required t))
                  (:div :class "form-group"
                    (:label :class "form-label" "Port")
                    (:input :class "form-input" :type "number" :name "port"
                            :value (format nil "~d" (config:network-port net-cfg))))
                  (:div :class "form-group"
                    (:label :class "form-label" "TLS")
                    (:select :class "form-input" :name "tls"
                      (:option :value "yes"
                        :selected (when (config:network-tls net-cfg) "selected")
                        "Yes")
                      (:option :value "no"
                        :selected (unless (config:network-tls net-cfg) "selected")
                        "No")))
                  (:div :class "form-group"
                    (:label :class "form-label" "Nick")
                    (:input :class "form-input" :type "text" :name "nick"
                            :value (or (config:network-nick net-cfg) "")))
                  (:div :class "form-group"
                    (:label :class "form-label" "Alt Nick")
                    (:input :class "form-input" :type "text" :name "alt-nick"
                            :value (or (config:network-alt-nick net-cfg) "")
                            :placeholder "optional"))
                  (:div :class "form-group"
                    (:label :class "form-label" "Username (ident)")
                    (:input :class "form-input" :type "text" :name "ident"
                            :value (or (config:network-username net-cfg) "")
                            :placeholder "optional"))
                  (:div :class "form-group"
                    (:label :class "form-label" "Real Name")
                    (:input :class "form-input" :type "text" :name "realname"
                            :value (or (config:network-realname net-cfg) "CLoak User")))
                  (:div :class "form-group"
                    (:label :class "form-label" "Server Password")
                    (:input :class "form-input" :type "password" :name "server-pass"
                            :placeholder "optional"))
                  (:div :class "form-group"
                    (:label :class "form-label" "SASL")
                    (:select :class "form-input" :name "sasl"
                      (:option :value "" :selected (unless (config:network-sasl net-cfg) "selected") "None")
                      (:option :value "plain" :selected (when (string-equal "plain" (config:network-sasl net-cfg)) "selected") "PLAIN")))
                  (:div :class "form-group"
                    (:label :class "form-label" "Buffer Size")
                    (:input :class "form-input" :type "number" :name "buffer-size"
                            :value (format nil "~d" (config:network-buffer-size net-cfg))))
                  (:div :class "form-group"
                    (:label :class "form-label" "Block MOTD")
                    (:select :class "form-input" :name "block-motd"
                      (:option :value "no"
                        :selected (unless (config:network-block-motd net-cfg) "selected")
                        "No")
                      (:option :value "yes"
                        :selected (when (config:network-block-motd net-cfg) "selected")
                        "Yes")))
                (:div :class "flex gap-1 mt-2"
                  (:button :type "submit" :class "btn btn-primary" "Save Network")
                  (:button :class "btn"
                           :data-on-click "/action/app-shell/navigate-user"
                           :data-param-username username
                           "Cancel"))))

            ;; Channels section (ZNC-style, separate from connection settings)
            (:div :class "card mt-2"
              (:div :class "card-header"
                (:span :class "card-title" "Channels"))
              (if (config:network-autojoin net-cfg)
                  (:table :class "data-table"
                    (:thead
                      (:tr
                        (:th "Channel")
                        (:th "Actions")))
                    (:tbody
                      (dolist (chan (config:network-autojoin net-cfg))
                        (:tr
                          (:td :class "mono" chan)
                          (:td
                            (:button :class "btn btn-sm btn-danger"
                                     :data-on-click "/action/network-edit/delete-channel"
                                     :data-param-username username
                                     :data-param-network netname
                                     :data-param-channel chan
                                     "Delete"))))))
                  (:p :style "padding: 1rem; color: var(--surface2);"
                    "No channels configured. Add channels below.")))

            ;; Add channel form
            (:div :class "card mt-2"
              (:div :class "card-header"
                (:span :class "card-title" "Add Channel"))
              (:form :class "form-inline" :data-on-submit "/action/network-edit/add-channel"
                (:input :type "hidden" :name "username" :value username)
                (:input :type "hidden" :name "network" :value netname)
                (:div :class "form-row"
                  (:div :class "form-group"
                    (:label :class "form-label" "Channel Name")
                    (:input :class "form-input" :type "text" :name "channel"
                            :placeholder "#channel" :required t))
                  (:div :class "form-group" :style "align-self: flex-end;"
                    (:button :type "submit" :class "btn btn-primary" "Add Channel")))))))))))

;;; -------------------------------------------------------
;;; Config Component
;;; -------------------------------------------------------

(comp:defcomponent config-page
  :id "config-page"
  :slots ((message :initform nil :accessor config-message))
  :render
  (let* ((b (bouncer-instance))
         (cfg (when b (bouncer:bouncer-config b))))
    (spinneret:with-html-string
      (:div :id (comp:component-id self)
        (:div :class "page-header"
          (:h2 "Configuration")
          (:p :class "subtitle" "View and manage bouncer settings"))

        (when (config-message self)
          (:div :class "alert" (config-message self)))

        ;; Listener Settings
        (:div :class "card"
          (:div :class "card-header"
            (:span :class "card-title" "IRC Listener"))
          (:form :data-on-submit "/action/config-page/save-listener"
            (:div :class "form-grid"
              (:div :class "form-group"
                (:label :class "form-label" "Listen Host")
                (:input :class "form-input" :type "text" :name "listen-host"
                        :value (if cfg (config:config-listen-host cfg) "0.0.0.0")))
              (:div :class "form-group"
                (:label :class "form-label" "Listen Port")
                (:input :class "form-input" :type "number" :name "listen-port"
                        :value (format nil "~d" (if cfg (config:config-listen-port cfg) 6697))))
              (:div :class "form-group"
                (:label :class "form-label" "TLS")
                (:select :class "form-input" :name "listen-tls"
                  (:option :value "yes"
                    :selected (when (and cfg (config:config-listen-tls cfg)) "selected")
                    "Enabled")
                  (:option :value "no"
                    :selected (when (or (null cfg) (not (config:config-listen-tls cfg))) "selected")
                    "Disabled")))
              (:div :class "form-group"
                (:label :class "form-label" "TLS Certificate Path")
                (:input :class "form-input" :type "text" :name "tls-cert"
                        :value (or (and cfg (config:config-tls-cert cfg)) "")
                        :placeholder "/etc/letsencrypt/live/example.com/fullchain.pem"))
              (:div :class "form-group"
                (:label :class "form-label" "TLS Key Path")
                (:input :class "form-input" :type "text" :name "tls-key"
                        :value (or (and cfg (config:config-tls-key cfg)) "")
                        :placeholder "/etc/letsencrypt/live/example.com/privkey.pem")))
            (:div :style "padding: 0 1rem 1rem;"
              (:div :class "flex gap-1"
                (:button :type "submit" :class "btn btn-primary"
                         :data-disable-during-request t
                         "Save Listener Settings")
                (:span :style "font-size: 0.75rem; color: var(--subtext0); align-self: center;"
                  "Requires restart to take effect")))))

        ;; Web Admin Settings
        (:div :class "card mt-2"
          (:div :class "card-header"
            (:span :class "card-title" "Web Admin"))
          (:form :data-on-submit "/action/config-page/save-web"
            (:div :class "form-grid"
              (:div :class "form-group"
                (:label :class "form-label" "Web Host")
                (:input :class "form-input" :type "text" :name "web-host"
                        :value (if cfg (config:config-web-host cfg) "127.0.0.1")))
              (:div :class "form-group"
                (:label :class "form-label" "Web Port")
                (:input :class "form-input" :type "number" :name "web-port"
                        :value (format nil "~d" (if cfg (config:config-web-port cfg) 8076))))
              (:div :class "form-group"
                (:label :class "form-label" "Log Level")
                (:select :class "form-input" :name "log-level"
                  (:option :value "debug"
                    :selected (when (and cfg (eq (config:config-log-level cfg) :debug)) "selected")
                    "Debug")
                  (:option :value "info"
                    :selected (when (or (null cfg) (eq (config:config-log-level cfg) :info)) "selected")
                    "Info")
                  (:option :value "warn"
                    :selected (when (and cfg (eq (config:config-log-level cfg) :warn)) "selected")
                    "Warn")
                  (:option :value "error"
                    :selected (when (and cfg (eq (config:config-log-level cfg) :error)) "selected")
                    "Error"))))
            (:div :style "padding: 0 1rem 1rem;"
              (:div :class "flex gap-1"
                (:button :type "submit" :class "btn btn-primary"
                         :data-disable-during-request t
                         "Save Web Settings")
                (:span :style "font-size: 0.75rem; color: var(--subtext0); align-self: center;"
                  "Requires restart to take effect")))))

        ;; Raw Config
        (:div :class "card mt-2"
          (:div :class "card-header"
            (:span :class "card-title" "Raw Config")
            (:div :class "flex gap-1"
              (:button :class "btn btn-sm"
                       :data-on-click "/action/config-page/reload"
                       :data-disable-during-request t
                       "Reload from Disk")
              (:button :class "btn btn-sm btn-primary"
                       :data-on-click "/action/config-page/save"
                       :data-disable-during-request t
                       "Save to Disk")))
          (:pre :style "font-family: var(--mono); font-size: 0.8rem; color: var(--subtext1); padding: 1rem; background: var(--mantle); border-radius: var(--radius); overflow-x: auto; white-space: pre-wrap;"
            (if cfg
                (with-output-to-string (s)
                  (let ((*print-pretty* t)
                        (*print-case* :downcase))
                    (write (config:config-to-plist cfg) :stream s)))
                "No configuration loaded."))))))
)

;;; -------------------------------------------------------
;;; Change Password Component
;;; -------------------------------------------------------

(comp:defcomponent change-password-form
  :id "change-password"
  :slots ((error-message :initform nil :accessor change-pw-error)
          (success-message :initform nil :accessor change-pw-success))
  :render
  (spinneret:with-html-string
    (:div :id (comp:component-id self) :class "login-page"
      (:div :class "login-box"
        (:h1 "CLoak")
        (:div :class "tagline" "Change your password")
        (when (change-pw-error self)
          (:div :class "login-error" (change-pw-error self)))
        (when (change-pw-success self)
          (:div :class "badge badge-green" :style "display: block; text-align: center; margin-bottom: 1rem; padding: 0.5rem;"
            (change-pw-success self)))
        (:form :data-on-submit "/action/change-password/change"
          (:div :class "form-group"
            (:label :class "form-label" "New Password")
            (:input :type "password" :name "new-password"
                    :class "form-input"
                    :placeholder "Enter new password"
                    :required t
                    :minlength "6"))
          (:div :class "form-group"
            (:label :class "form-label" "Confirm Password")
            (:input :type "password" :name "confirm-password"
                    :class "form-input"
                    :placeholder "Confirm new password"
                    :required t))
          (:button :type "submit" :class "login-btn"
                   :data-disable-during-request t
                   "Set Password"))))))

;;; -------------------------------------------------------
;;; Buffers Component
;;; -------------------------------------------------------

(defun format-message-time (universal-time)
  "Format a universal time as HH:MM:SS for display."
  (multiple-value-bind (sec min hour)
      (decode-universal-time universal-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour min sec)))

(comp:defcomponent buffers-page
  :id "buffers-page"
  :slots ((expanded-buffer :initform nil :accessor buffers-expanded))
  :render
  (let* ((b (bouncer-instance))
         (buffers (when b (bouncer:bouncer-buffers b)))
         (buffer-keys nil)
         (expanded (buffers-expanded self)))
    ;; Collect buffer keys
    (when buffers
      (maphash (lambda (key val)
                 (declare (ignore val))
                 (push key buffer-keys))
               buffers)
      (setf buffer-keys (sort buffer-keys #'string<)))
    (spinneret:with-html-string
      (:div :id (comp:component-id self)
        (:div :class "page-header"
          (:h2 "Buffers")
          (:p :class "subtitle"
            (format nil "~d active buffers" (length buffer-keys))))
        (:div :class "card"
          (:table :class "data-table"
            (:thead
              (:tr
                (:th "Buffer")
                (:th "Messages")
                (:th "Latest")
                (:th "")))
            (:tbody
              (if buffer-keys
                  (dolist (key buffer-keys)
                    (let* ((buf (gethash key buffers))
                           (count (cloak.buffer:buffer-count buf))
                           (msgs (when (> count 0)
                                   (cloak.buffer:buffer-messages-all buf)))
                           (latest (when msgs
                                     (cloak.buffer:stored-message-time
                                      (car (last msgs))))))
                      (:tr
                        (:td :class "mono" key)
                        (:td (format nil "~d" count))
                        (:td :class "mono"
                          (if latest
                              (format-message-time latest)
                              "-"))
                        (:td
                          (if (string= key expanded)
                              (:button :class "btn btn-sm"
                                       :data-on-click "/action/buffers-page/collapse"
                                       "Hide")
                              (:button :class "btn btn-sm"
                                       :data-on-click "/action/buffers-page/expand"
                                       :data-param-key key
                                       "View"))))))
                  (:tr
                    (:td :colspan "4"
                         :style "text-align: center; color: var(--surface2); padding: 2rem;"
                      "No buffers yet. Connect to a network to start buffering."))))))

        ;; Expanded buffer messages
        (when (and expanded buffers)
          (let ((buf (gethash expanded buffers)))
            (when buf
              (:div :class "card mt-2"
                (:div :class "card-header"
                  (:span :class "card-title" (format nil "Messages: ~a" expanded))
                  (:button :class "btn btn-sm btn-danger"
                           :data-on-click "/action/buffers-page/clear"
                           :data-param-key expanded
                           "Clear Buffer"))
                (:div :style "max-height: 400px; overflow-y: auto; padding: 0.5rem 1rem; background: var(--mantle); border-radius: 0 0 var(--radius) var(--radius);"
                  (let ((msgs (cloak.buffer:buffer-messages-all buf)))
                    (if msgs
                        (dolist (msg msgs)
                          (:div :style "font-family: var(--mono); font-size: 0.75rem; padding: 0.2rem 0; border-bottom: 1px solid var(--surface0);"
                            (:span :style "color: var(--subtext0); margin-right: 0.5rem;"
                              (format-message-time (cloak.buffer:stored-message-time msg)))
                            (:span :style "color: var(--text);"
                              (cloak.buffer:stored-message-raw msg))))
                        (:p :style "color: var(--surface2); text-align: center; padding: 1rem;"
                          "Buffer is empty."))))))))))))

;;; -------------------------------------------------------
;;; Modules Component
;;; -------------------------------------------------------

(comp:defcomponent modules-page
  :id "modules-page"
  :slots ((message :initform nil :accessor modules-message)
          (detail-module :initform nil :accessor modules-detail))
  :render
  (let* ((registered (modules:list-registered-modules))
         (detail-name (modules-detail self))
         (detail-mod (when detail-name (modules:active-module detail-name)))
         (detail-reg (when detail-name (modules:find-module-registration detail-name))))
    (spinneret:with-html-string
      (:div :id (comp:component-id self)
        (:div :class "page-header"
          (:h2 "Modules")
          (:p :class "subtitle"
            (format nil "~d available, ~d loaded — plugins: ~a"
                    (length registered)
                    (hash-table-count modules:*active-modules*)
                    (namestring (modules:plugin-directory)))))
        (when (modules-message self)
          (:div :class "alert" (modules-message self)))

        ;; Module list as cards
        (:div :class "module-grid"
          (dolist (entry registered)
            (let* ((name (car entry))
                   (plist (cdr entry))
                   (desc (getf plist :description))
                   (scope (getf plist :scope))
                   (ver (getf plist :version))
                   (author (getf plist :author))
                   (source (getf plist :source))
                   (active (modules:module-active-p name)))
              (:div :class (if active "module-card module-active" "module-card")
                (:div :class "module-card-header"
                  (:div
                    (:div :class "module-name"
                      name
                      (when ver
                        (:span :style "font-size: 0.7rem; color: var(--subtext0); margin-left: 0.5rem;"
                          (format nil "v~a" ver))))
                    (:div :class "module-desc" (or desc ""))
                    (when author
                      (:div :style "font-size: 0.7rem; color: var(--subtext0); margin-top: 0.25rem;"
                        (format nil "by ~a" author))))
                  (:div :class "module-card-actions"
                    (when (eq source :plugin)
                      (:span :class "badge" :style "background: var(--mauve); color: var(--base);"
                        "plugin"))
                    (:span :class (if (eq scope :network)
                                      "badge badge-amber"
                                      "badge badge-green")
                      (if (eq scope :network) "network" "global"))
                    (if active
                        (:button :class "btn btn-sm btn-danger"
                                 :data-on-click "/action/modules-page/disable"
                                 :data-param-name name
                                 "Disable")
                        (:button :class "btn btn-sm btn-primary"
                                 :data-on-click "/action/modules-page/enable"
                                 :data-param-name name
                                 "Enable"))))
                ;; Settings link for active modules that have settings
                (when active
                  (let ((mod-instance (modules:active-module name)))
                    (when (and mod-instance (modules:module-settings-html mod-instance))
                      (if (string-equal detail-name name)
                          ;; Settings panel expanded inline
                          (:div :class "module-card-settings"
                            :style "border-top: 1px solid var(--surface1); padding: 0.75rem; background: var(--mantle);"
                            (:div :style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0.5rem;"
                              (:span :style "font-weight: 600; font-size: 0.85rem;"
                                (format nil "~a Settings" name))
                              (:button :class "btn btn-sm"
                                       :data-on-click "/action/modules-page/hide-settings"
                                       "Close"))
                            (:form :data-on-submit "/action/modules-page/save-settings"
                              (:input :type "hidden" :name "module-name" :value name)
                              (:raw (modules:module-settings-html mod-instance))
                              (:div :style "margin-top: 0.5rem;"
                                (:button :type "submit" :class "btn btn-sm btn-primary"
                                  "Save"))))
                          ;; Collapsed — show settings button
                          (:div :class "module-card-footer"
                            (:button :class "btn btn-sm"
                                     :data-on-click "/action/modules-page/show-settings"
                                     :data-param-name name
                                     "Settings"))))))))))))))
