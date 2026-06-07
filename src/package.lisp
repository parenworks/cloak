;;;; package.lisp - Package definitions for CLoak IRC bouncer

(defpackage #:cloak.config
  (:use #:cl)
  (:local-nicknames (#:alex #:alexandria))
  (:export
   ;; Config structure
   #:*config*
   #:*config-path*
   #:bouncer-config
   #:config-listen-host
   #:config-listen-port
   #:config-listen-tls
   #:config-tls-cert
   #:config-tls-key
   #:config-web-host
   #:config-web-port
   #:config-users
   #:config-log-level
   #:config-enabled-modules
   #:config-playback-lines
   ;; User config
   #:user-config
   #:user-name
   #:user-password-hash
   #:user-networks
   #:user-admin-p
   ;; Network config
   #:network-config
   #:network-name
   #:network-server
   #:network-port
   #:network-tls
   #:network-nick
   #:network-username
   #:network-realname
   #:network-password
   #:network-sasl
   #:network-alt-nick
   #:network-autojoin
   #:network-buffer-size
   #:network-block-motd
   ;; Paths
   #:xdg-config-home
   ;; Operations
   #:load-config
   #:save-config
   #:config-to-plist
   #:generate-default-config
   #:find-user
   #:find-network
   ;; Password hashing
   #:hash-password
   #:verify-password
   #:default-password-p))

(defpackage #:cloak.protocol
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export
   ;; Thread-safe logging
   #:*log-lock*
   #:cloak-log
   ;; IRC message structure
   #:irc-message
   #:make-irc-message
   #:irc-message-tags
   #:irc-message-source
   #:irc-message-command
   #:irc-message-params
   ;; Parsing and formatting
   #:parse-message
   #:format-message
   ;; Tag handling
   #:parse-tags
   #:format-tags
   ;; Source parsing
   #:irc-source
   #:make-irc-source
   #:irc-source-nick
   #:irc-source-user
   #:irc-source-host
   #:parse-source
   #:source-nick
   ;; Common message constructors
   #:irc-pass
   #:irc-nick
   #:irc-user
   #:irc-join
   #:irc-part
   #:irc-quit
   #:irc-privmsg
   #:irc-notice
   #:irc-ping
   #:irc-pong
   #:irc-cap
   #:irc-authenticate))

(defpackage #:cloak.buffer
  (:use #:cl)
  (:local-nicknames (#:lt #:local-time))
  (:export
   ;; Buffer management
   #:message-buffer
   #:make-message-buffer
   #:buffer-push
   #:buffer-messages-since
   #:buffer-messages-all
   #:buffer-clear
   #:buffer-count
   ;; Stored message
   #:stored-message
   #:make-stored-message
   #:stored-message-time
   #:stored-message-raw
   #:stored-message-msgid))

(defpackage #:cloak.upstream
  (:use #:cl #:cloak.protocol)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export
   ;; Upstream (bouncer -> IRC server) connection
   #:upstream-connection
   #:make-upstream
   #:upstream-connect
   #:upstream-disconnect
   #:upstream-send
   #:upstream-connected-p
   #:upstream-state
   #:upstream-nick
   #:upstream-network-name
   #:upstream-channels
   #:upstream-channel-nicks
   #:upstream-cap-enabled
   #:upstream-server-name
   #:upstream-isupport
   #:upstream-motd
   #:upstream-config
   #:upstream-reconnect-p
   #:upstream-reconnect-attempts
   #:upstream-on-state-change
   #:calculate-backoff))

(defpackage #:cloak.downstream
  (:use #:cl #:cloak.protocol)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export
   ;; Downstream (IRC client -> bouncer) connection
   #:downstream-client
   #:make-downstream-client
   #:client-send
   #:client-disconnect
   #:client-nick
   #:client-user
   #:client-ident
   #:client-authenticated-p
   #:client-network
   #:client-last-playback
   #:client-message-handler
   #:client-disconnect-handler
   ;; Listener
   #:start-listener
   #:stop-listener))

(defpackage #:cloak.bouncer
  (:use #:cl #:cloak.config #:cloak.protocol
        #:cloak.buffer #:cloak.upstream #:cloak.downstream)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:alex #:alexandria))
  (:export
   ;; Core bouncer
   #:*bouncer*
   #:bouncer
   #:make-bouncer
   #:start-bouncer
   #:stop-bouncer
   ;; Runtime operations
   #:bouncer-config
   #:bouncer-upstreams
   #:bouncer-clients
   #:bouncer-buffers
   #:bouncer-lock
   #:bouncer-running-p
   #:bouncer-start-time
   #:attach-client
   #:detach-client
   #:relay-to-upstream
   #:relay-to-clients
   #:playback-buffer))

(defpackage #:cloak.modules
  (:use #:cl #:cloak.protocol)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export
   ;; Module protocol
   #:module
   #:module-name
   #:module-description
   #:module-version
   #:module-author
   #:module-scope
   #:module-network
   #:module-timers
   ;; Lifecycle hooks
   #:on-load
   #:on-unload
   ;; Message hooks
   #:on-upstream-message
   #:on-downstream-message
   ;; Connection hooks
   #:on-client-attach
   #:on-client-detach
   #:on-upstream-connect
   #:on-upstream-disconnect
   ;; Auth hooks
   #:on-new-connection
   #:on-auth-failure
   ;; Channel hooks
   #:on-channel-join
   #:on-channel-part
   #:on-channel-kick
   ;; Settings
   #:module-settings-html
   #:on-save-settings
   ;; Timer support
   #:start-module-timer
   ;; Persistent storage
   #:load-module-data
   #:save-module-data
   ;; Registry
   #:register-module
   #:define-module
   #:find-module-registration
   #:list-registered-modules
   ;; Active modules
   #:*active-modules*
   #:module-active-p
   #:active-module
   #:list-active-modules
   #:load-module
   #:unload-module
   ;; Hook dispatch
   #:run-upstream-hooks
   #:run-downstream-hooks
   #:run-client-attach-hooks
   #:run-client-detach-hooks
   #:run-upstream-connect-hooks
   #:run-upstream-disconnect-hooks
   #:run-channel-join-hooks
   #:run-channel-part-hooks
   #:run-channel-kick-hooks
   #:run-new-connection-hooks
   #:run-auth-failure-hooks
   ;; Plugin system
   #:plugin-directory
   #:scan-plugins))

(defpackage #:cloak
  (:use #:cl #:cloak.protocol)
  (:export
   ;; Top-level entry points
   #:start
   #:stop
   #:main
   #:version
   #:*version*))
