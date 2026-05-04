;;;; test/test-modules.lisp - Functional tests for the module system and built-in modules

(in-package #:cloak.test)

(in-suite :cloak-tests)

;;; --- Test Helpers ---

(defun make-test-config ()
  "Create a minimal network-config for testing."
  (make-instance 'cloak.config:network-config
    :name "testnet" :server "irc.test" :port 6667
    :tls nil :nick "testuser" :autojoin '("#test")))

(defun make-test-bouncer ()
  "Create a bouncer with minimal config for testing."
  (let* ((net (make-test-config))
         (user (make-instance 'cloak.config:user-config
                 :name "tester" :password-hash "x"
                 :networks (list net)))
         (cfg (make-instance 'cloak.config:bouncer-config
                 :users (list user)
                 :enabled-modules nil)))
    (cloak.bouncer:make-bouncer cfg)))

(defun make-test-upstream (&key (network-name "testnet") (nick "testuser") config)
  "Create a minimal upstream-connection for testing (not connected)."
  (make-instance 'cloak.upstream::upstream-connection
    :network-name network-name
    :nick nick
    :config (or config (make-test-config))))

(defmacro with-clean-modules (&body body)
  "Run BODY with a clean active-modules table (restores after)."
  `(let ((saved-active (alexandria:copy-hash-table cloak.modules:*active-modules*)))
     (unwind-protect (progn ,@body)
       ;; Unload anything we loaded (stops timers)
       (maphash (lambda (name mod)
                  (declare (ignore mod))
                  (unless (gethash name saved-active)
                    (ignore-errors
                      (cloak.modules:on-unload
                       (cloak.modules:active-module name) nil))))
                cloak.modules:*active-modules*)
       (setf cloak.modules:*active-modules* saved-active))))

;;; --- Module Registry Tests ---

(test module-register-and-find
  "Register a module and find it."
  (cloak.modules:register-module "test-dummy" 'cloak.modules:module
    :description "A test module" :scope :global :version "0.1")
  (let ((entry (cloak.modules:find-module-registration "test-dummy")))
    (is (not (null entry)))
    (is (string= "A test module" (getf entry :description)))
    (is (eq :global (getf entry :scope)))
    (is (string= "0.1" (getf entry :version)))))

(test module-load-unload
  "Load and unload a module."
  (with-clean-modules
    (let ((bouncer (make-test-bouncer)))
      (cloak.modules:register-module "test-lu" 'cloak.modules:module
        :description "load-unload test")
      (let ((mod (cloak.modules:load-module "test-lu" bouncer)))
        (is (not (null mod)))
        (is (cloak.modules:module-active-p "test-lu"))
        ;; Loading again returns existing instance
        (is (eq mod (cloak.modules:load-module "test-lu" bouncer)))
        ;; Unload
        (is (cloak.modules:unload-module "test-lu" bouncer))
        (is (null (cloak.modules:module-active-p "test-lu")))))))

(test module-load-unknown
  "Loading an unknown module returns NIL."
  (with-clean-modules
    (let ((bouncer (make-test-bouncer)))
      (is (null (cloak.modules:load-module "nonexistent-xyz" bouncer))))))

(test module-list-registered
  "list-registered-modules returns all registered modules sorted."
  (let ((mods (cloak.modules:list-registered-modules)))
    ;; Should include our built-ins
    (is (find "block-motd" mods :key #'car :test #'string=))
    (is (find "keepnick" mods :key #'car :test #'string=))
    (is (find "stickychan" mods :key #'car :test #'string=))
    ;; Should be sorted by name
    (let ((names (mapcar #'car mods)))
      (is (equal names (sort (copy-list names) #'string<))))))

;;; --- Module Persistent Storage Tests ---

(test module-data-roundtrip
  "Save and load module data."
  (let ((data '(:foo "bar" :count 42)))
    (cloak.modules:save-module-data "test-roundtrip" data)
    (let ((loaded (cloak.modules:load-module-data "test-roundtrip")))
      (is (equal "bar" (getf loaded :foo)))
      (is (= 42 (getf loaded :count))))
    ;; Cleanup
    (let ((path (cloak.modules::module-data-path "test-roundtrip")))
      (when (probe-file path) (delete-file path)))))

(test module-data-missing-returns-nil
  "Loading data for non-existent module returns NIL."
  (is (null (cloak.modules:load-module-data "definitely-not-a-real-module"))))

;;; --- Block-MOTD Functional Test ---

(test block-motd-drops-motd
  "Block-MOTD module returns :drop for MOTD numerics."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (mod (cloak.modules:load-module "block-motd" bouncer)))
      (is (not (null mod)))
      ;; 375 = MOTD start — should drop
      (let ((msg (parse-message ":server 375 testuser :- MOTD -")))
        (is (eq :drop (cloak.modules:on-upstream-message mod bouncer nil
                         ":server 375 testuser :- MOTD -" msg))))
      ;; 372 = MOTD body — should drop
      (let ((msg (parse-message ":server 372 testuser :Welcome")))
        (is (eq :drop (cloak.modules:on-upstream-message mod bouncer nil
                         ":server 372 testuser :Welcome" msg))))
      ;; 376 = MOTD end — should drop
      (let ((msg (parse-message ":server 376 testuser :End of MOTD")))
        (is (eq :drop (cloak.modules:on-upstream-message mod bouncer nil
                         ":server 376 testuser :End of MOTD" msg))))
      ;; Normal PRIVMSG should NOT be dropped
      (let ((msg (parse-message ":nick!u@h PRIVMSG #test :hello")))
        (is (null (cloak.modules:on-upstream-message mod bouncer nil
                    ":nick!u@h PRIVMSG #test :hello" msg)))))))

(test block-motd-passes-other-numerics
  "Block-MOTD does not drop non-MOTD numerics."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (mod (cloak.modules:load-module "block-motd" bouncer)))
      ;; 001 RPL_WELCOME should pass through
      (let ((msg (parse-message ":server 001 testuser :Welcome to IRC")))
        (is (null (cloak.modules:on-upstream-message mod bouncer nil
                    ":server 001 testuser :Welcome to IRC" msg))))
      ;; 332 RPL_TOPIC should pass through
      (let ((msg (parse-message ":server 332 testuser #test :Channel topic")))
        (is (null (cloak.modules:on-upstream-message mod bouncer nil
                    ":server 332 testuser #test :Channel topic" msg)))))))

;;; --- Hook Dispatch Error Isolation ---

(test hook-dispatch-error-isolation
  "A broken module hook doesn't crash the dispatch."
  (with-clean-modules
    (let ((bouncer (make-test-bouncer)))
      ;; Create a module that always errors
      (defclass error-test-module (cloak.modules:module) ())
      (defmethod cloak.modules:on-upstream-message
          ((mod error-test-module) bouncer upstream raw-line msg)
        (declare (ignore bouncer upstream raw-line msg))
        (error "Intentional test error"))
      (cloak.modules:register-module "error-test" 'error-test-module)
      (cloak.modules:load-module "error-test" bouncer)
      ;; Dispatch should not signal — error is caught
      (let ((msg (parse-message ":n!u@h PRIVMSG #ch :hi")))
        (finishes
          (cloak.modules:run-upstream-hooks bouncer nil ":n!u@h PRIVMSG #ch :hi" msg))))))

;;; --- Buffextras Functional Test ---

(test buffextras-buffers-join-event
  "Buffextras module buffers JOIN events into existing channel buffers."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (upstream (make-test-upstream)))
      ;; Register upstream so buffextras can resolve user/network
      (setf (gethash "tester/testnet" (cloak.bouncer:bouncer-upstreams bouncer)) upstream)
      ;; Create a buffer for #test
      (let ((buf (cloak.buffer:make-message-buffer :capacity 100)))
        (setf (gethash "tester/testnet/#test" (cloak.bouncer:bouncer-buffers bouncer))
              buf)
        (let ((mod (cloak.modules:load-module "buffextras" bouncer)))
          ;; Simulate a JOIN event — call the module directly
          (let* ((raw ":other!u@h JOIN #test")
                 (msg (parse-message raw)))
            ;; Call module directly with real upstream
            (cloak.modules:on-upstream-message mod bouncer upstream raw msg))
          ;; Buffer should now have the JOIN line
          (let ((msgs (buffer-messages-all buf)))
            (is (= 1 (length msgs)))
            (is (search "JOIN" (stored-message-raw (first msgs))))))))))

(test buffextras-ignores-privmsg
  "Buffextras does not buffer regular PRIVMSG (bouncer handles that)."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (upstream (make-test-upstream)))
      (setf (gethash "tester/testnet" (cloak.bouncer:bouncer-upstreams bouncer)) upstream)
      (let ((buf (cloak.buffer:make-message-buffer :capacity 100)))
        (setf (gethash "tester/testnet/#test" (cloak.bouncer:bouncer-buffers bouncer))
              buf)
        (let ((mod (cloak.modules:load-module "buffextras" bouncer)))
          (let* ((raw ":nick!u@h PRIVMSG #test :hello")
                 (msg (parse-message raw)))
            (cloak.modules:on-upstream-message mod bouncer upstream raw msg))
          ;; Buffer should be empty (PRIVMSG not handled by buffextras)
          (is (= 0 (buffer-count buf))))))))

;;; --- Perform Module Settings Test ---

(test perform-saves-and-restores-commands
  "Perform module persists and restores command list."
  (with-clean-modules
    (let ((bouncer (make-test-bouncer)))
      (let ((mod (cloak.modules:load-module "perform" bouncer)))
        (cloak.modules:on-save-settings mod
          '((:commands . "MODE $me +x
JOIN #secret")))
        (is (= 2 (length (cloak.modules::perform-commands mod))))
        (is (string= "MODE $me +x" (first (cloak.modules::perform-commands mod))))
        ;; Verify persistence
        (let ((data (cloak.modules:load-module-data "perform")))
          (is (not (null data)))
          (is (= 2 (length (getf data :commands))))))
      ;; Cleanup data file
      (let ((path (cloak.modules::module-data-path "perform")))
        (when (probe-file path) (delete-file path))))))

(test perform-empty-lines-stripped
  "Perform strips blank lines from command list."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (mod (cloak.modules:load-module "perform" bouncer)))
      (cloak.modules:on-save-settings mod
        '((:commands . "MODE $me +x

JOIN #channel
   
")))
      ;; Should only have 2 real commands
      (is (= 2 (length (cloak.modules::perform-commands mod))))
      ;; Cleanup
      (let ((path (cloak.modules::module-data-path "perform")))
        (when (probe-file path) (delete-file path))))))

;;; --- Auto-away settings test ---

(test auto-away-settings
  "Auto-away module accepts and stores custom away message."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (mod (cloak.modules:load-module "auto-away" bouncer)))
      ;; Default message
      (is (string= "Detached from CLoak" (cloak.modules::auto-away-message mod)))
      ;; Update via settings
      (cloak.modules:on-save-settings mod '((:away-message . "Gone fishing")))
      (is (string= "Gone fishing" (cloak.modules::auto-away-message mod))))))

;;; --- ClearBufferOnMsg Functional Test ---

(test clearbufferonmsg-clears-buffer
  "ClearBufferOnMsg clears the buffer when user sends PRIVMSG to a target."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream nil))
           (mod (cloak.modules:load-module "clearbufferonmsg" bouncer)))
      ;; Set up client identity
      (setf (cloak.downstream:client-user client) "tester")
      (setf (cloak.downstream:client-network client) "testnet")
      ;; Create a buffer with some messages
      (let ((buf (cloak.buffer:make-message-buffer :capacity 100)))
        (cloak.buffer:buffer-push buf ":nick!u@h PRIVMSG #test :old message 1")
        (cloak.buffer:buffer-push buf ":nick!u@h PRIVMSG #test :old message 2")
        (is (= 2 (cloak.buffer:buffer-count buf)))
        (setf (gethash "tester/testnet/#test" (cloak.bouncer:bouncer-buffers bouncer))
              buf)
        ;; Simulate user sending a message to #test
        (let* ((raw "PRIVMSG #test :I'm here now")
               (msg (parse-message raw)))
          (cloak.modules:on-downstream-message mod bouncer client raw msg))
        ;; Buffer should be cleared
        (is (= 0 (cloak.buffer:buffer-count buf)))))))

(test clearbufferonmsg-ignores-other-commands
  "ClearBufferOnMsg does nothing for non-PRIVMSG/NOTICE commands."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream nil))
           (mod (cloak.modules:load-module "clearbufferonmsg" bouncer)))
      (setf (cloak.downstream:client-user client) "tester")
      (setf (cloak.downstream:client-network client) "testnet")
      (let ((buf (cloak.buffer:make-message-buffer :capacity 100)))
        (cloak.buffer:buffer-push buf ":nick!u@h PRIVMSG #test :msg")
        (setf (gethash "tester/testnet/#test" (cloak.bouncer:bouncer-buffers bouncer))
              buf)
        ;; JOIN should not clear buffer
        (let* ((raw "JOIN #test")
               (msg (parse-message raw)))
          (cloak.modules:on-downstream-message mod bouncer client raw msg))
        (is (= 1 (cloak.buffer:buffer-count buf)))))))

;;; --- Route-Replies Functional Test ---

(test route-replies-tracks-whois-query
  "Route-replies records the requesting client when WHOIS is sent."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream nil))
           (mod (cloak.modules:load-module "route-replies" bouncer)))
      ;; Client sends WHOIS
      (let* ((raw "WHOIS someuser")
             (msg (parse-message raw)))
        (cloak.modules:on-downstream-message mod bouncer client raw msg))
      ;; Pending table should have WHOIS -> client
      (is (eq client (gethash "WHOIS" (cloak.modules::rr-pending mod)))))))

(test route-replies-drops-reply-numerics
  "Route-replies returns :drop for WHOIS reply numerics."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (upstream (make-test-upstream))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream nil))
           (mod (cloak.modules:load-module "route-replies" bouncer)))
      ;; Register client as WHOIS requester
      (setf (gethash "WHOIS" (cloak.modules::rr-pending mod)) client)
      ;; 311 RPL_WHOISUSER should be dropped from normal relay
      (let* ((raw ":server 311 me someuser user host * :Real Name")
             (msg (parse-message raw)))
        (is (eq :drop (cloak.modules:on-upstream-message mod bouncer upstream raw msg))))
      ;; 318 End of WHOIS should also drop and clear pending
      (let* ((raw ":server 318 me someuser :End of /WHOIS list")
             (msg (parse-message raw)))
        (is (eq :drop (cloak.modules:on-upstream-message mod bouncer upstream raw msg))))
      ;; Pending should be cleared after end numeric
      (is (null (gethash "WHOIS" (cloak.modules::rr-pending mod)))))))

(test route-replies-passes-unrelated-numerics
  "Route-replies does not interfere with non-reply numerics."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (upstream (make-test-upstream))
           (mod (cloak.modules:load-module "route-replies" bouncer)))
      ;; 001 RPL_WELCOME should pass through
      (let* ((raw ":server 001 me :Welcome")
             (msg (parse-message raw)))
        (is (null (cloak.modules:on-upstream-message mod bouncer upstream raw msg))))
      ;; Regular PRIVMSG should pass through
      (let* ((raw ":nick!u@h PRIVMSG #test :hello")
             (msg (parse-message raw)))
        (is (null (cloak.modules:on-upstream-message mod bouncer upstream raw msg)))))))

;;; --- Log Module Test ---

(test log-module-loads-and-creates-dir
  "Log module creates log directory on load."
  (with-clean-modules
    (let ((bouncer (make-test-bouncer)))
      (let ((mod (cloak.modules:load-module "log" bouncer)))
        (is (not (null mod)))
        (is (not (null (cloak.modules::log-log-dir mod))))
        (is (uiop:directory-exists-p (cloak.modules::log-log-dir mod)))))))

;;; --- Clientnotify Functional Test ---

(test clientnotify-loads
  "Clientnotify module loads successfully."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (mod (cloak.modules:load-module "clientnotify" bouncer)))
      (is (not (null mod)))
      (is (cloak.modules:module-active-p "clientnotify")))))

;;; --- Fail2ban Functional Test ---

(test fail2ban-tracks-failures
  "Fail2ban tracks auth failures and bans after threshold."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (mod (cloak.modules:load-module "fail2ban" bouncer)))
      ;; Lower threshold for testing
      (setf (cloak.modules::f2b-max-attempts mod) 3)
      (setf (cloak.modules::f2b-attempt-window mod) 60)
      (setf (cloak.modules::f2b-ban-duration mod) 120)
      ;; First failure — should be tracked
      (cloak.modules:on-auth-failure mod bouncer "1.2.3.4")
      (is (= 1 (car (gethash "1.2.3.4" (cloak.modules::f2b-attempts mod)))))
      ;; Second failure
      (cloak.modules:on-auth-failure mod bouncer "1.2.3.4")
      (is (= 2 (car (gethash "1.2.3.4" (cloak.modules::f2b-attempts mod)))))
      ;; Third failure — should trigger ban
      (cloak.modules:on-auth-failure mod bouncer "1.2.3.4")
      ;; Attempts should be cleared, ban should exist
      (is (null (gethash "1.2.3.4" (cloak.modules::f2b-attempts mod))))
      (is (not (null (gethash "1.2.3.4" (cloak.modules::f2b-bans mod))))))))

(test fail2ban-rejects-banned-ip
  "Fail2ban rejects connections from banned IPs."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (mod (cloak.modules:load-module "fail2ban" bouncer)))
      ;; Manually ban an IP
      (setf (gethash "5.6.7.8" (cloak.modules::f2b-bans mod))
            (+ (get-universal-time) 300))
      ;; Connection should be rejected
      (is (eq :drop (cloak.modules:on-new-connection mod bouncer "5.6.7.8")))
      ;; Non-banned IP should pass
      (is (null (cloak.modules:on-new-connection mod bouncer "9.10.11.12"))))))

;;; --- Controlpanel Functional Test ---

(test controlpanel-requires-admin
  "Controlpanel rejects non-admin users."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream nil))
           (mod (cloak.modules:load-module "controlpanel" bouncer)))
      ;; Set up client as non-admin user "tester"
      (setf (cloak.downstream:client-user client) "tester")
      (setf (cloak.downstream:client-network client) "testnet")
      (setf (cloak.downstream:client-nick client) "tester")
      ;; tester is NOT admin by default in our test config
      (let* ((raw "PRIVMSG *controlpanel :help")
             (msg (parse-message raw)))
        (is (eq :halt (cloak.modules:on-downstream-message mod bouncer client raw msg)))))))

(test controlpanel-help-for-admin
  "Controlpanel shows help to admin users."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream nil))
           (mod (cloak.modules:load-module "controlpanel" bouncer)))
      ;; Make tester an admin
      (let ((user-cfg (cloak.config:find-user "tester" (cloak.bouncer:bouncer-config bouncer))))
        (setf (cloak.config:user-admin-p user-cfg) t))
      (setf (cloak.downstream:client-user client) "tester")
      (setf (cloak.downstream:client-network client) "testnet")
      (setf (cloak.downstream:client-nick client) "tester")
      ;; Help command should return :halt (handled)
      (let* ((raw "PRIVMSG *controlpanel :help")
             (msg (parse-message raw)))
        (is (eq :halt (cloak.modules:on-downstream-message mod bouncer client raw msg)))))))

(test controlpanel-ignores-non-cp-messages
  "Controlpanel ignores messages not directed to *controlpanel."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream nil))
           (mod (cloak.modules:load-module "controlpanel" bouncer)))
      (setf (cloak.downstream:client-user client) "tester")
      (setf (cloak.downstream:client-network client) "testnet")
      ;; Message to a regular channel — should return nil (not handled)
      (let* ((raw "PRIVMSG #test :hello")
             (msg (parse-message raw)))
        (is (null (cloak.modules:on-downstream-message mod bouncer client raw msg)))))))

;;; --- Watch Module Test ---

(test watch-loads-and-persists
  "Watch module loads watchlist from persistent storage."
  (with-clean-modules
    (let ((bouncer (make-test-bouncer)))
      ;; Save some watch data
      (cloak.modules:save-module-data "watch" '(:nicks ("alice" "bob")))
      (let ((mod (cloak.modules:load-module "watch" bouncer)))
        (is (= 2 (length (cloak.modules::watch-list mod))))
        (is (member "alice" (cloak.modules::watch-list mod) :test #'string=)))
      ;; Cleanup
      (let ((path (cloak.modules::module-data-path "watch")))
        (when (probe-file path) (delete-file path))))))

(test watch-ignores-unwatched-nicks
  "Watch module ignores events from unwatched users."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (upstream (make-test-upstream))
           (mod (cloak.modules:load-module "watch" bouncer)))
      ;; Empty watchlist — QUIT from random user should do nothing
      (let* ((raw ":random!u@h QUIT :bye")
             (msg (parse-message raw)))
        (is (null (cloak.modules:on-upstream-message mod bouncer upstream raw msg)))))))

;;; --- Flooddetach Functional Test ---

(test flooddetach-counts-messages
  "Flooddetach tracks message counts per channel."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (upstream (make-test-upstream))
           (mod (cloak.modules:load-module "flooddetach" bouncer)))
      ;; Set low threshold
      (setf (cloak.modules::flood-threshold mod) 100)
      (setf (cloak.modules::flood-window mod) 60)
      ;; Send a few messages — should not trigger
      (dotimes (i 5)
        (let* ((raw (format nil ":nick~d!u@h PRIVMSG #test :msg ~d" i i))
               (msg (parse-message raw)))
          (cloak.modules:on-upstream-message mod bouncer upstream raw msg)))
      ;; Counter should exist
      (let ((record (gethash "#test" (cloak.modules::flood-counters mod))))
        (is (not (null record)))
        (is (= 5 (car record)))))))

(test flooddetach-ignores-non-channel
  "Flooddetach does not track private messages."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (upstream (make-test-upstream))
           (mod (cloak.modules:load-module "flooddetach" bouncer)))
      ;; Private message — should not create counter
      (let* ((raw ":nick!u@h PRIVMSG testuser :private msg")
             (msg (parse-message raw)))
        (cloak.modules:on-upstream-message mod bouncer upstream raw msg))
      (is (= 0 (hash-table-count (cloak.modules::flood-counters mod)))))))

;;; --- Auth hooks dispatch test ---

(test auth-hooks-dispatch
  "Auth failure and new-connection hooks dispatch correctly."
  (with-clean-modules
    (let ((bouncer (make-test-bouncer)))
      ;; Should not error even with no modules loaded
      (finishes (cloak.modules:run-auth-failure-hooks bouncer "1.2.3.4"))
      (is (null (cloak.modules:run-new-connection-hooks bouncer "1.2.3.4"))))))

;;; --- Clientbuffer Functional Test ---

(test clientbuffer-saves-position-on-detach
  "Clientbuffer saves playback position when client detaches."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream nil))
           (mod (cloak.modules:load-module "clientbuffer" bouncer)))
      (setf (cloak.downstream:client-user client) "tester")
      (setf (cloak.downstream:client-network client) "testnet")
      (setf (cloak.downstream:client-nick client) "tester")
      ;; Detach should save position
      (cloak.modules:on-client-detach mod bouncer client)
      (let ((key "tester/testnet/tester"))
        (is (not (null (gethash key (cloak.modules::cb-positions mod)))))
        (is (numberp (gethash key (cloak.modules::cb-positions mod)))))
      ;; Cleanup
      (let ((path (cloak.modules::module-data-path "clientbuffer")))
        (when (probe-file path) (delete-file path))))))

(test clientbuffer-restores-position-on-attach
  "Clientbuffer restores playback position when known client attaches."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream nil))
           (mod (cloak.modules:load-module "clientbuffer" bouncer)))
      (setf (cloak.downstream:client-user client) "tester")
      (setf (cloak.downstream:client-network client) "testnet")
      (setf (cloak.downstream:client-nick client) "tester")
      ;; Store a position
      (let ((saved-time (- (get-universal-time) 300)))
        (setf (gethash "tester/testnet/tester" (cloak.modules::cb-positions mod))
              saved-time)
        ;; Attach should restore
        (cloak.modules:on-client-attach mod bouncer client "tester" "testnet")
        (is (= saved-time (cloak.downstream:client-last-playback client))))
      ;; Cleanup
      (let ((path (cloak.modules::module-data-path "clientbuffer")))
        (when (probe-file path) (delete-file path))))))

;;; --- Playback Module Tests ---

(test playback-parse-duration
  "Playback duration parser handles various formats."
  (is (= 7200 (cloak.modules::pb--parse-duration "2h")))
  (is (= 1800 (cloak.modules::pb--parse-duration "30m")))
  (is (= 90 (cloak.modules::pb--parse-duration "90s")))
  (is (= 5400 (cloak.modules::pb--parse-duration "1h30m")))
  (is (= 3661 (cloak.modules::pb--parse-duration "1h1m1s")))
  (is (= 60 (cloak.modules::pb--parse-duration "60")))
  (is (null (cloak.modules::pb--parse-duration "")))
  (is (null (cloak.modules::pb--parse-duration nil)))
  (is (null (cloak.modules::pb--parse-duration "abc"))))

(test playback-ignores-non-playback-messages
  "Playback module ignores messages not to *playback."
  (with-clean-modules
    (let* ((bouncer (make-test-bouncer))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream nil))
           (mod (cloak.modules:load-module "playback" bouncer)))
      (setf (cloak.downstream:client-user client) "tester")
      (setf (cloak.downstream:client-network client) "testnet")
      (let* ((raw "PRIVMSG #test :hello")
             (msg (parse-message raw)))
        (is (null (cloak.modules:on-downstream-message mod bouncer client raw msg)))))))

;;; --- Module count verification ---

(test all-builtin-modules-registered
  "All expected built-in modules are registered."
  (let ((expected '("ctcp-version" "block-motd" "auto-away" "nickserv"
                    "perform" "keepnick" "stickychan" "buffextras" "savebuff"
                    "clearbufferonmsg" "log" "route-replies"
                    "clientnotify" "fail2ban" "controlpanel" "watch" "flooddetach"
                    "clientbuffer" "playback"))
        (registered (mapcar #'car (cloak.modules:list-registered-modules))))
    (dolist (name expected)
      (is (member name registered :test #'string=)
          "Module ~a should be registered" name))))
