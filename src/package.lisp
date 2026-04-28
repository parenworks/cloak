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
   #:network-autojoin
   #:network-buffer-size
   ;; Operations
   #:load-config
   #:save-config
   #:generate-default-config
   #:find-user
   #:find-network))

(defpackage #:cloak.protocol
  (:use #:cl)
  (:export
   ;; IRC message structure
   #:irc-message
   #:make-irc-message
   #:message-tags
   #:message-source
   #:message-command
   #:message-params
   ;; Parsing and formatting
   #:parse-message
   #:format-message
   ;; Tag handling
   #:parse-tags
   #:format-tags
   ;; Source parsing
   #:parse-source
   #:source-nick
   #:source-user
   #:source-host
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
   #:stored-time
   #:stored-raw
   #:stored-msgid))

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
   #:upstream-nick
   #:upstream-network-name
   #:upstream-channels
   #:upstream-cap-enabled))

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
   #:client-authenticated-p
   #:client-network
   #:client-last-playback
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
   #:bouncer
   #:make-bouncer
   #:start-bouncer
   #:stop-bouncer
   ;; Runtime operations
   #:bouncer-upstreams
   #:bouncer-clients
   #:attach-client
   #:detach-client
   #:relay-to-upstream
   #:relay-to-clients
   #:playback-buffer))

(defpackage #:cloak.modules
  (:use #:cl #:cloak.bouncer)
  (:export
   #:module
   #:module-name
   #:module-description
   #:on-load
   #:on-unload
   #:on-upstream-message
   #:on-downstream-message
   #:register-module
   #:find-module))

(defpackage #:cloak
  (:use #:cl)
  (:export
   ;; Top-level entry points
   #:start
   #:stop
   #:main
   #:version))
