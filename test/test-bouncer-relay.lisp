;;;; test/test-bouncer-relay.lisp - Bouncer relay and integration tests
;;;; Covers message relay, buffering, echo dedup, *status commands,
;;;; client auth, attach/detach, playback, and server noise filtering.

(in-package #:cloak.test)

(in-suite :cloak-tests)

;;; --- Helpers ---

(defun make-relay-bouncer ()
  "Create a bouncer with one user/network for relay testing."
  (let* ((net (make-instance 'cloak.config:network-config
                :name "testnet" :server "irc.test" :port 6667
                :tls nil :nick "botnick" :autojoin '("#test")))
         (user (make-instance 'cloak.config:user-config
                 :name "tester"
                 :password-hash (cloak.config:hash-password "secret")
                 :admin-p t :networks (list net)))
         (cfg (make-instance 'cloak.config:bouncer-config
                 :users (list user)
                 :enabled-modules nil))
         (bouncer (cloak.bouncer:make-bouncer cfg)))
    ;; Create an upstream (not connected, just state tracking)
    (let ((upstream (cloak.upstream:make-upstream net)))
      (setf (cloak.upstream:upstream-state upstream) :connected)
      (setf (cloak.upstream:upstream-nick upstream) "botnick")
      ;; Simulate being in #test
      (setf (gethash "#test" (cloak.upstream:upstream-channels upstream)) t)
      (setf (gethash "#test" (cloak.upstream:upstream-channel-nicks upstream))
            (let ((ht (make-hash-table :test 'equal)))
              (setf (gethash "botnick" ht) t)
              ht))
      (setf (gethash "tester/testnet" (cloak.bouncer:bouncer-upstreams bouncer))
            upstream))
    bouncer))

(defun make-attached-client (bouncer &key (user "tester") (network "testnet"))
  "Create a mock client and attach it to BOUNCER."
  (let* ((output (make-string-output-stream))
         (client (make-instance 'cloak.downstream:downstream-client
                   :socket nil :stream output)))
    (setf (cloak.downstream:client-nick client) "clientnick")
    (setf (cloak.downstream:client-user client) user)
    (setf (cloak.downstream:client-network client) network)
    (setf (cloak.downstream:client-authenticated-p client) t)
    (setf (cloak.downstream:client-last-playback client) (get-universal-time))
    ;; Add to bouncer's client list
    (bordeaux-threads:with-lock-held ((cloak.bouncer:bouncer-lock bouncer))
      (push client (cloak.bouncer:bouncer-clients bouncer)))
    ;; Set up message handler
    (setf (cloak.downstream:client-message-handler client)
          (lambda (client line msg)
            (cloak.bouncer::bouncer--on-client-message
             bouncer user client line msg)))
    (values client output)))

(defun get-upstream (bouncer &key (user "tester") (network "testnet"))
  "Get the test upstream from BOUNCER."
  (gethash (format nil "~a/~a" user network)
           (cloak.bouncer:bouncer-upstreams bouncer)))

;;; --- Upstream -> Client Relay ---

(test relay-privmsg-to-client
  "PRIVMSG from upstream is relayed to attached client."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (declare (ignore client))
      ;; Simulate upstream message
      (let* ((raw ":alice!a@b PRIVMSG #test :hello")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-upstream-message
         bouncer "tester" (get-upstream bouncer) raw msg))
      ;; Client should have received it
      (let ((written (get-output-stream-string output)))
        (is (search "PRIVMSG #test :hello" written))))))

(test relay-privmsg-buffered
  "PRIVMSG from upstream is buffered for later playback."
  (let ((bouncer (make-relay-bouncer)))
    ;; No clients attached - just buffer
    (let* ((raw ":alice!a@b PRIVMSG #test :hello")
           (msg (cloak.protocol:parse-message raw)))
      (cloak.bouncer::bouncer--on-upstream-message
       bouncer "tester" (get-upstream bouncer) raw msg))
    ;; Check buffer
    (let ((buf (gethash "tester/testnet/#test" (cloak.bouncer:bouncer-buffers bouncer))))
      (is (not (null buf)))
      (is (= 1 (cloak.buffer:buffer-count buf))))))

(test relay-notice-buffered
  "NOTICE from upstream is buffered."
  (let ((bouncer (make-relay-bouncer)))
    (let* ((raw ":alice!a@b NOTICE #test :important notice")
           (msg (cloak.protocol:parse-message raw)))
      (cloak.bouncer::bouncer--on-upstream-message
       bouncer "tester" (get-upstream bouncer) raw msg))
    (let ((buf (gethash "tester/testnet/#test" (cloak.bouncer:bouncer-buffers bouncer))))
      (is (= 1 (cloak.buffer:buffer-count buf))))))

(test relay-dm-buffered-under-sender
  "DM to us is buffered under sender's nick, not our nick."
  (let ((bouncer (make-relay-bouncer)))
    (let* ((raw ":alice!a@b PRIVMSG botnick :private msg")
           (msg (cloak.protocol:parse-message raw)))
      (cloak.bouncer::bouncer--on-upstream-message
       bouncer "tester" (get-upstream bouncer) raw msg))
    ;; Should be buffered under "alice" (the sender), not "botnick"
    (let ((buf (gethash "tester/testnet/alice" (cloak.bouncer:bouncer-buffers bouncer))))
      (is (not (null buf)))
      (is (= 1 (cloak.buffer:buffer-count buf))))))

;;; --- Server Noise Filtering ---

(test relay-server-notice-not-buffered
  "Server NOTICE (no ! in source) is not buffered or relayed."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (declare (ignore client))
      (let* ((raw ":irc.server.com NOTICE * :Looking up your hostname")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-upstream-message
         bouncer "tester" (get-upstream bouncer) raw msg))
      ;; Not buffered
      (is (zerop (hash-table-count (cloak.bouncer:bouncer-buffers bouncer))))
      ;; Not relayed
      (is (string= "" (get-output-stream-string output))))))

(test relay-nickserv-notice-not-buffered
  "NickServ NOTICE is not buffered."
  (let ((bouncer (make-relay-bouncer)))
    (let* ((raw ":NickServ!NickServ@services NOTICE botnick :You are identified")
           (msg (cloak.protocol:parse-message raw)))
      (cloak.bouncer::bouncer--on-upstream-message
       bouncer "tester" (get-upstream bouncer) raw msg))
    (is (zerop (hash-table-count (cloak.bouncer:bouncer-buffers bouncer))))))

;;; --- Echo Dedup ---

(test relay-echo-not-buffered
  "Echo-message (our own PRIVMSG echoed back) is not buffered."
  (let ((bouncer (make-relay-bouncer)))
    ;; Message appears to come from our own nick
    (let* ((raw ":botnick!tester@CLoak PRIVMSG #test :my own message")
           (msg (cloak.protocol:parse-message raw)))
      (cloak.bouncer::bouncer--on-upstream-message
       bouncer "tester" (get-upstream bouncer) raw msg))
    ;; Echo should NOT be buffered (we buffer locally when sending)
    (is (zerop (hash-table-count (cloak.bouncer:bouncer-buffers bouncer))))))

(test relay-echo-not-relayed
  "Echo-message is not relayed to clients."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (declare (ignore client))
      (let* ((raw ":botnick!tester@CLoak PRIVMSG #test :my own message")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-upstream-message
         bouncer "tester" (get-upstream bouncer) raw msg))
      (is (string= "" (get-output-stream-string output))))))

;;; --- Client -> Upstream Relay ---

(test relay-client-quit-detaches
  "QUIT from client detaches rather than forwarding."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client)
        (make-attached-client bouncer)
      ;; Set up disconnect handler like attach-client does
      (setf (cloak.downstream:client-disconnect-handler client)
            (lambda (c) (cloak.bouncer:detach-client bouncer c)))
      ;; Simulate client sending QUIT
      (let* ((raw "QUIT :bye")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-client-message
         bouncer "tester" client raw msg))
      ;; Client should be removed from bouncer
      (is (not (member client (cloak.bouncer:bouncer-clients bouncer)))))))

(test relay-client-ping-answered-locally
  "PING from client is answered locally, not forwarded."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (let* ((raw "PING :mytoken")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-client-message
         bouncer "tester" client raw msg))
      (let ((written (get-output-stream-string output)))
        (is (search "PONG" written))))))

;;; --- *status Commands ---

(test status-help-responds
  "*status help sends help text."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (let* ((raw "PRIVMSG *status :help")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-client-message
         bouncer "tester" client raw msg))
      (let ((written (get-output-stream-string output)))
        (is (search "help" written))
        (is (search "NOTICE" written))))))

(test status-version-responds
  "*status version responds with version."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (let* ((raw "PRIVMSG *status :version")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-client-message
         bouncer "tester" client raw msg))
      (let ((written (get-output-stream-string output)))
        (is (search "CLoak" written))))))

(test status-listchans-responds
  "*status listchans responds with channels."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (let* ((raw "PRIVMSG *status :listchans")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-client-message
         bouncer "tester" client raw msg))
      (let ((written (get-output-stream-string output)))
        (is (search "#test" written))))))

(test status-listclients-responds
  "*status listclients shows attached client count."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (let* ((raw "PRIVMSG *status :listclients")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-client-message
         bouncer "tester" client raw msg))
      (let ((written (get-output-stream-string output)))
        (is (search "1 client" written))))))

(test status-listnets-responds
  "*status listnets shows configured networks."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (let* ((raw "PRIVMSG *status :listnets")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-client-message
         bouncer "tester" client raw msg))
      (let ((written (get-output-stream-string output)))
        (is (search "testnet" written))
        (is (search "connected" written))))))

(test status-unknown-command
  "*status unknown command gives error."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (let* ((raw "PRIVMSG *status :xyzzy")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-client-message
         bouncer "tester" client raw msg))
      (let ((written (get-output-stream-string output)))
        (is (search "Unknown command" written))))))

(test status-uptime-responds
  "*status uptime responds with time info."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      (let* ((raw "PRIVMSG *status :uptime")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-client-message
         bouncer "tester" client raw msg))
      (let ((written (get-output-stream-string output)))
        (is (search "running since" written))))))

;;; --- Client Auth Flow ---

(test auth-valid-credentials
  "Valid user/network:password authenticates and attaches."
  (let* ((bouncer (make-relay-bouncer))
         (output (make-string-output-stream))
         (client (make-instance 'cloak.downstream:downstream-client
                   :socket nil :stream output)))
    ;; Set up registration handler
    (setf (cloak.downstream:client-message-handler client)
          (lambda (client line msg)
            (declare (ignore line))
            (cloak.bouncer::bouncer--handle-client-auth bouncer client msg)))
    ;; Send PASS
    (cloak.downstream::client--handle-line client "PASS tester/testnet:secret")
    ;; Send NICK
    (cloak.downstream::client--handle-line client "NICK mynick")
    ;; Send USER - triggers auth
    (cloak.downstream::client--handle-line client "USER myident 0 * :Real Name")
    ;; Should be authenticated
    (is (eq t (cloak.downstream:client-authenticated-p client)))
    ;; Should have received welcome
    (let ((written (get-output-stream-string output)))
      (is (search "001" written))
      (is (search "Welcome" written)))))

(test auth-bad-password
  "Wrong password sends 464 and disconnects."
  (let* ((bouncer (make-relay-bouncer))
         (output (make-string-output-stream))
         (client (make-instance 'cloak.downstream:downstream-client
                   :socket nil :stream output)))
    (setf (cloak.downstream:client-message-handler client)
          (lambda (client line msg)
            (declare (ignore line))
            (cloak.bouncer::bouncer--handle-client-auth bouncer client msg)))
    (cloak.downstream::client--handle-line client "PASS tester/testnet:wrongpass")
    (cloak.downstream::client--handle-line client "NICK mynick")
    (cloak.downstream::client--handle-line client "USER myident 0 * :Name")
    ;; Should not be authenticated
    (is (not (cloak.downstream:client-authenticated-p client)))
    ;; Should have received error
    (let ((written (get-output-stream-string output)))
      (is (search "464" written)))))

(test auth-bad-format
  "Malformed PASS (no slash) sends error."
  (let* ((bouncer (make-relay-bouncer))
         (output (make-string-output-stream))
         (client (make-instance 'cloak.downstream:downstream-client
                   :socket nil :stream output)))
    (setf (cloak.downstream:client-message-handler client)
          (lambda (client line msg)
            (declare (ignore line))
            (cloak.bouncer::bouncer--handle-client-auth bouncer client msg)))
    (cloak.downstream::client--handle-line client "PASS badformat")
    (cloak.downstream::client--handle-line client "NICK mynick")
    (cloak.downstream::client--handle-line client "USER myident 0 * :Name")
    (is (not (cloak.downstream:client-authenticated-p client)))
    (let ((written (get-output-stream-string output)))
      (is (search "PASS user/network:password" written)))))

(test auth-cap-ls-responds
  "CAP LS during auth responds with empty cap list."
  (let* ((bouncer (make-relay-bouncer))
         (output (make-string-output-stream))
         (client (make-instance 'cloak.downstream:downstream-client
                   :socket nil :stream output)))
    (setf (cloak.downstream:client-message-handler client)
          (lambda (client line msg)
            (declare (ignore line))
            (cloak.bouncer::bouncer--handle-client-auth bouncer client msg)))
    (cloak.downstream::client--handle-line client "CAP LS 302")
    (let ((written (get-output-stream-string output)))
      (is (search "CAP * LS :" written)))))

;;; --- Playback ---

(test playback-sends-buffered-messages
  "Playback sends messages buffered since last playback time."
  (let ((bouncer (make-relay-bouncer)))
    ;; Buffer a message
    (let ((buf (cloak.buffer:make-message-buffer :capacity 100)))
      (cloak.buffer:buffer-push buf ":alice!a@b PRIVMSG #test :old message")
      (setf (gethash "tester/testnet/#test" (cloak.bouncer:bouncer-buffers bouncer))
            buf))
    ;; Create client with old playback time
    (let* ((output (make-string-output-stream))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream output)))
      (setf (cloak.downstream:client-user client) "tester")
      (setf (cloak.downstream:client-network client) "testnet")
      (setf (cloak.downstream:client-last-playback client) 0)
      (cloak.bouncer:playback-buffer bouncer client "tester" "testnet")
      (let ((written (get-output-stream-string output)))
        (is (search "old message" written))))))

(test playback-updates-timestamp
  "Playback updates client's last-playback time."
  (let ((bouncer (make-relay-bouncer)))
    (let* ((output (make-string-output-stream))
           (client (make-instance 'cloak.downstream:downstream-client
                     :socket nil :stream output)))
      (setf (cloak.downstream:client-user client) "tester")
      (setf (cloak.downstream:client-network client) "testnet")
      (setf (cloak.downstream:client-last-playback client) 0)
      (cloak.bouncer:playback-buffer bouncer client "tester" "testnet")
      ;; Timestamp should be updated to roughly now
      (is (> (cloak.downstream:client-last-playback client) 0)))))

;;; --- Detach ---

(test detach-removes-client
  "detach-client removes client from bouncer's client list."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client)
        (make-attached-client bouncer)
      (is (member client (cloak.bouncer:bouncer-clients bouncer)))
      (cloak.bouncer:detach-client bouncer client)
      (is (not (member client (cloak.bouncer:bouncer-clients bouncer)))))))

;;; --- Message Target Resolution ---

(test message-target-channel
  "Channel message targets the channel."
  (let ((msg (cloak.protocol:parse-message ":alice!a@b PRIVMSG #test :hello")))
    (is (string= "#test" (cloak.bouncer::bouncer--message-target msg "me")))))

(test message-target-dm-to-us
  "DM to our nick targets the sender."
  (let ((msg (cloak.protocol:parse-message ":alice!a@b PRIVMSG botnick :hello")))
    (is (string= "alice" (cloak.bouncer::bouncer--message-target msg "botnick")))))

(test message-target-dm-from-us
  "Our own PRIVMSG targets the recipient."
  (let ((msg (cloak.protocol:parse-message ":botnick!u@h PRIVMSG alice :hello")))
    (is (string= "alice" (cloak.bouncer::bouncer--message-target msg "botnick")))))

(test message-target-non-privmsg
  "Non-PRIVMSG/NOTICE returns NIL target."
  (let ((msg (cloak.protocol:parse-message ":alice!a@b JOIN #test")))
    (is (null (cloak.bouncer::bouncer--message-target msg "botnick")))))

;;; --- Server Noise Detection ---

(test server-noise-server-notice
  "NOTICE from server (no !) is server noise."
  (let ((msg (cloak.protocol:parse-message ":irc.server.com NOTICE * :Looking up host")))
    (is (cloak.bouncer::bouncer--server-noise-p "NOTICE" msg))))

(test server-noise-nickserv
  "NOTICE from NickServ is server noise."
  (let ((msg (cloak.protocol:parse-message ":NickServ!services@host NOTICE me :Identified")))
    (is (cloak.bouncer::bouncer--server-noise-p "NOTICE" msg))))

(test server-noise-chanserv
  "NOTICE from ChanServ is server noise."
  (let ((msg (cloak.protocol:parse-message ":ChanServ!services@host NOTICE me :Info")))
    (is (cloak.bouncer::bouncer--server-noise-p "NOTICE" msg))))

(test server-noise-user-notice-not-noise
  "NOTICE from a regular user is not server noise."
  (let ((msg (cloak.protocol:parse-message ":alice!a@b NOTICE #test :hello")))
    (is (not (cloak.bouncer::bouncer--server-noise-p "NOTICE" msg)))))

(test server-noise-privmsg-not-noise
  "PRIVMSG is never server noise."
  (let ((msg (cloak.protocol:parse-message ":irc.server.com PRIVMSG * :test")))
    (is (not (cloak.bouncer::bouncer--server-noise-p "PRIVMSG" msg)))))

;;; --- Relay to Only Matching Clients ---

(test relay-only-to-matching-network
  "Messages relay only to clients on the same user/network."
  (let ((bouncer (make-relay-bouncer)))
    ;; Attach two clients to different networks
    (multiple-value-bind (client1 output1)
        (make-attached-client bouncer :user "tester" :network "testnet")
      (declare (ignore client1))
      ;; Create a second "fake" client on a different network
      (let* ((output2 (make-string-output-stream))
             (client2 (make-instance 'cloak.downstream:downstream-client
                        :socket nil :stream output2)))
        (setf (cloak.downstream:client-nick client2) "other")
        (setf (cloak.downstream:client-user client2) "tester")
        (setf (cloak.downstream:client-network client2) "othernet")
        (setf (cloak.downstream:client-authenticated-p client2) t)
        (bordeaux-threads:with-lock-held ((cloak.bouncer:bouncer-lock bouncer))
          (push client2 (cloak.bouncer:bouncer-clients bouncer)))
        ;; Send message on testnet
        (let* ((raw ":alice!a@b PRIVMSG #test :hello")
               (msg (cloak.protocol:parse-message raw)))
          (cloak.bouncer::bouncer--on-upstream-message
           bouncer "tester" (get-upstream bouncer) raw msg))
        ;; client1 (testnet) should get it
        (is (search "hello" (get-output-stream-string output1)))
        ;; client2 (othernet) should NOT
        (is (string= "" (get-output-stream-string output2)))))))

;;; --- NAMES Response ---

(test names-response-local
  "NAMES request is answered locally from tracked nick data."
  (let ((bouncer (make-relay-bouncer)))
    (multiple-value-bind (client output)
        (make-attached-client bouncer)
      ;; Add more nicks to the channel
      (let* ((upstream (get-upstream bouncer))
             (nicks (gethash "#test" (cloak.upstream:upstream-channel-nicks upstream))))
        (setf (gethash "alice" nicks) t)
        (setf (gethash "bob" nicks) t))
      ;; Send NAMES request
      (let* ((raw "NAMES #test")
             (msg (cloak.protocol:parse-message raw)))
        (cloak.bouncer::bouncer--on-client-message
         bouncer "tester" client raw msg))
      (let ((written (get-output-stream-string output)))
        ;; Should have 353 (NAMREPLY) and 366 (ENDOFNAMES)
        (is (search "353" written))
        (is (search "366" written))
        (is (search "End of /NAMES" written))))))
