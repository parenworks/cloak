;;;; bouncer.lisp - Core bouncer logic for CLoak
;;;; Orchestrates upstream/downstream connections, message relay, and buffering.

(in-package #:cloak.bouncer)

;;; --- Bouncer ---

(defclass bouncer ()
  ((config :initarg :config :accessor bouncer-config)
   (upstreams :initform (make-hash-table :test 'equal) :accessor bouncer-upstreams
              :documentation "Hash: \"user/network\" -> upstream-connection")
   (clients :initform nil :accessor bouncer-clients
            :documentation "List of connected downstream-client instances")
   (buffers :initform (make-hash-table :test 'equal) :accessor bouncer-buffers
            :documentation "Hash: \"user/network/target\" -> message-buffer")
   (lock :initform (bt:make-lock "bouncer-lock") :accessor bouncer-lock)
   (running-p :initform nil :accessor bouncer-running-p)
   (start-time :initform (get-universal-time) :accessor bouncer-start-time)
   (last-sender :initform nil :accessor bouncer-last-sender
                :documentation "The client that sent the last PRIVMSG/NOTICE, for echo dedup."))
  (:documentation "The core CLoak bouncer instance."))

(defvar *bouncer* nil "The active bouncer instance.")

(defun make-bouncer (config)
  "Create a bouncer from CONFIG."
  (make-instance 'bouncer :config config))

;;; --- Upstream Management ---

(defun bouncer--upstream-key (user-name network-name)
  "Generate hash key for an upstream connection."
  (format nil "~a/~a" user-name network-name))

(defun bouncer--get-upstream (bouncer user-name network-name)
  "Get the upstream connection for USER-NAME on NETWORK-NAME."
  (gethash (bouncer--upstream-key user-name network-name)
           (bouncer-upstreams bouncer)))

(defun bouncer--connect-upstreams (bouncer)
  "Connect all configured upstream networks for all users.
Connections are made in a background thread so startup isn't blocked."
  (bt:make-thread
   (lambda ()
     (let ((first-p t))
       (dolist (user (config-users (bouncer-config bouncer)))
         (dolist (net (user-networks user))
           ;; Stagger connections to avoid IRC server throttling
           (unless first-p (sleep 2))
           (setf first-p nil)
           (let* ((key (bouncer--upstream-key (user-name user) (network-name net)))
                  (upstream (make-upstream net
                              :message-handler
                              (lambda (upstream raw-line msg)
                                (bouncer--on-upstream-message bouncer
                                                              (user-name user)
                                                              upstream raw-line msg)))))
             ;; Fire disconnect hooks when upstream drops
             (setf (upstream-on-state-change upstream)
                   (lambda (us new-state)
                     (when (eq new-state :disconnected)
                       (cloak.modules:run-upstream-disconnect-hooks bouncer us))))
             (setf (gethash key (bouncer-upstreams bouncer)) upstream)
             (upstream-connect upstream))))))
   :name "cloak-connect-upstreams"))

;;; --- Buffer Management ---

(defun bouncer--buffer-key (user-name network-name target)
  "Generate hash key for a message buffer."
  (format nil "~a/~a/~a" user-name network-name (string-downcase target)))

(defun bouncer--get-buffer (bouncer user-name network-name target)
  "Get or create the message buffer for a target."
  (let ((key (bouncer--buffer-key user-name network-name target)))
    (or (gethash key (bouncer-buffers bouncer))
        (let ((net-cfg (find-network user-name network-name
                                     (bouncer-config bouncer))))
          (setf (gethash key (bouncer-buffers bouncer))
                (make-message-buffer
                 :capacity (if net-cfg
                               (network-buffer-size net-cfg)
                               500)))))))

(defun bouncer--server-noise-p (command msg)
  "Return T if MSG is server connection noise that should not be buffered.
  Suppresses NickServ/ChanServ notices and harmless server NOTICEs (ident, looking up host).
  Important server NOTICEs (flood warnings, kill notices) are passed through."
  (when (string= command "NOTICE")
    (let ((source (cloak.protocol:irc-message-source msg))
          (text (second (cloak.protocol:irc-message-params msg))))
      (or
       ;; NickServ / ChanServ notices (SASL handles auth)
       (and source
            (member (cloak.protocol:source-nick source)
                    '("NickServ" "ChanServ") :test #'string-equal))
       ;; Server NOTICE (source has no ! = it's a server, not a user)
       ;; But only suppress routine connection noise, not warnings
       (and source (not (position #\! source))
            text
            (or (search "Looking up your hostname" text)
                (search "Checking Ident" text)
                (search "Found your hostname" text)
                (search "No Ident response" text)
                (search "*** You are connected" text)))))))

;;; --- Message Relay ---

(defun bouncer--on-upstream-message (bouncer user-name upstream raw-line msg)
  "Handle a message from an upstream IRC server.
Buffer it and relay to any attached clients."
  ;; Run module upstream hooks first
  (let ((hook-result (cloak.modules:run-upstream-hooks bouncer upstream raw-line msg)))
    (when (member hook-result '(:halt :drop))
      (return-from bouncer--on-upstream-message)))
  (let* ((network-name (upstream-network-name upstream))
         (command (cloak.protocol:irc-message-command msg))
         (target (bouncer--message-target msg (upstream-nick upstream))))
    ;; Fire module hooks for connection/channel events
    (cond
      ((string= command "001")
       (cloak.modules:run-upstream-connect-hooks bouncer upstream))
      ((and (string= command "JOIN")
            (string-equal (cloak.protocol:source-nick
                           (cloak.protocol:irc-message-source msg))
                          (upstream-nick upstream)))
       (cloak.modules:run-channel-join-hooks bouncer upstream
         (first (cloak.protocol:irc-message-params msg))))
      ((and (string= command "PART")
            (string-equal (cloak.protocol:source-nick
                           (cloak.protocol:irc-message-source msg))
                          (upstream-nick upstream)))
       (cloak.modules:run-channel-part-hooks bouncer upstream
         (first (cloak.protocol:irc-message-params msg))))
      ((and (string= command "KICK")
            (string-equal (second (cloak.protocol:irc-message-params msg))
                          (upstream-nick upstream)))
       (cloak.modules:run-channel-kick-hooks bouncer upstream
         (first (cloak.protocol:irc-message-params msg))
         (cloak.protocol:source-nick (cloak.protocol:irc-message-source msg))
         (or (third (cloak.protocol:irc-message-params msg)) ""))))
    ;; Detect echo-message (server echoing our own PRIVMSG/NOTICE back)
    (let ((echo-p (and (member command '("PRIVMSG" "NOTICE") :test #'string=)
                       (string-equal (cloak.protocol:source-nick
                                      (cloak.protocol:irc-message-source msg))
                                     (upstream-nick upstream)))))
      ;; Buffer conversation messages (state events like JOIN/PART are
      ;; replayed via attach-client's channel state replay instead)
      ;; Skip: server noise, and echo-messages (already buffered locally by catch-all)
      (when (and (member command '("PRIVMSG" "NOTICE" "TOPIC") :test #'string=)
                (not echo-p)
                (not (bouncer--server-noise-p command msg)))
        (let ((buffer (bouncer--get-buffer bouncer user-name network-name
                                            (or target network-name)))
              (msgid (cdr (assoc "msgid"
                                 (cloak.protocol:irc-message-tags msg)
                                 :test #'string=))))
          (buffer-push buffer raw-line msgid)))
      ;; Relay to attached clients
      ;; Suppress: echo-messages (client displays locally) and server noise
      ;; (Ident checks, NickServ/ChanServ notices - no value to clients)
      (unless (or echo-p (bouncer--server-noise-p command msg))
        (bt:with-lock-held ((bouncer-lock bouncer))
          (dolist (client (bouncer-clients bouncer))
            (when (and (client-authenticated-p client)
                       (string-equal (client-user client) user-name)
                       (string-equal (client-network client) network-name))
              (client-send client raw-line))))))))

(defun bouncer--message-target (msg our-nick)
  "Determine the buffer target for MSG. DMs use the other party's nick."
  (let ((command (cloak.protocol:irc-message-command msg))
        (params (cloak.protocol:irc-message-params msg)))
    (when (member command '("PRIVMSG" "NOTICE") :test #'string=)
      (let ((target (first params)))
        (if (string-equal target our-nick)
            ;; DM to us - buffer under sender's nick
            (cloak.protocol:source-nick (cloak.protocol:irc-message-source msg))
            target)))))

;;; --- Client Attachment ---

(defun attach-client (bouncer client user-name network-name)
  "Attach CLIENT to BOUNCER for USER-NAME's NETWORK-NAME."
  (bt:with-lock-held ((bouncer-lock bouncer))
    (setf (client-authenticated-p client) t)
    (setf (client-network client) network-name)
    (setf (client-user client) user-name)
    (push client (bouncer-clients bouncer)))
  ;; Set up client message handler
  (setf (client-message-handler client)
        (lambda (client line msg)
          (bouncer--on-client-message bouncer user-name client line msg)))
  (setf (client-disconnect-handler client)
        (lambda (client)
          (detach-client bouncer client)))
  ;; Send welcome and state on a BACKGROUND THREAD so the client read loop
  ;; can return to reading immediately. If we sent this burst synchronously on
  ;; the read thread, the bouncer would not read from the client while writing
  ;; the (potentially large) burst; a client that writes during that window
  ;; (e.g. clatter's CHATHISTORY requests) could block on its socket send,
  ;; freezing single-threaded Emacs until the user hits C-g.
  (bt:make-thread
   (lambda ()
     (handler-case
         (bouncer--send-attach-burst bouncer client user-name network-name)
       (error (e)
         (cloak-log "[CLoak] Attach burst error for ~a: ~a~%" user-name e))))
   :name "cloak-attach-burst"))

(defun bouncer--send-attach-burst (bouncer client user-name network-name)
  "Send the registration burst, channel state, and backlog playback to CLIENT.
Run on a dedicated thread so it does not block the client read loop."
  (let ((upstream (bouncer--get-upstream bouncer user-name network-name)))
    (when upstream
      ;; Send full registration burst so strict clients (e.g. Revolution IRC)
      ;; consider registration complete. Many clients wait for 004 + end-of-MOTD.
      (let ((nick (upstream-nick upstream))
            (srv (or (cloak.upstream:upstream-server-name upstream) "CLoak")))
        (client-send client
                     (format nil ":CLoak 001 ~a :Welcome to CLoak bouncer ~a" nick nick))
        (client-send client
                     (format nil ":CLoak 002 ~a :Your host is CLoak, running via ~a" nick srv))
        (client-send client
                     (format nil ":CLoak 003 ~a :This server was created by CLoak" nick))
        (client-send client
                     (format nil ":CLoak 004 ~a CLoak cloak-1.0 iow ov" nick))
        ;; Relay captured ISUPPORT (005) tokens so the client parses
        ;; PREFIX/CHANTYPES/etc. correctly. Chunk into groups of ~13 tokens.
        (let ((tokens (cloak.upstream:upstream-isupport upstream)))
          (loop while tokens
                do (let ((chunk (subseq tokens 0 (min 13 (length tokens)))))
                     (setf tokens (nthcdr 13 tokens))
                     (client-send client
                                  (format nil ":CLoak 005 ~a ~{~a~^ ~} :are supported by this server"
                                          nick chunk)))))
        ;; Replay MOTD (375/372/376) or send 422 if none
        (let ((motd (cloak.upstream:upstream-motd upstream)))
          (if motd
              (progn
                (client-send client
                             (format nil ":CLoak 375 ~a :- Message of the day -" nick))
                (dolist (line motd)
                  (client-send client
                               (format nil ":CLoak 372 ~a :~a" nick line)))
                (client-send client
                             (format nil ":CLoak 376 ~a :End of /MOTD command" nick)))
              (client-send client
                           (format nil ":CLoak 422 ~a :MOTD File is missing" nick)))))
      ;; Replay channel state: JOIN + NAMES for each channel (as a real server does)
      ;; Stagger delivery to avoid overwhelming clients like Emacs
      (let ((channels nil))
        (maphash (lambda (chan _v)
                   (declare (ignore _v))
                   (push chan channels))
                 (upstream-channels upstream))
        (dolist (chan channels)
          ;; Send JOIN
          (client-send client
                       (format nil ":~a!~a@CLoak JOIN ~a"
                               (upstream-nick upstream)
                               user-name chan))
          ;; Send NAMES (353) for this channel
          (let ((nicks-ht (gethash chan (upstream-channel-nicks upstream))))
            (when nicks-ht
              (let ((nick-list nil))
                (maphash (lambda (nick prefix)
                           (push (format nil "~a~a" prefix nick) nick-list))
                         nicks-ht)
                ;; Send in chunks to stay under IRC line limit
                (let ((chunk nil) (chunk-len 0))
                  (dolist (entry nick-list)
                    (let ((entry-len (+ (length entry) 1)))
                      (when (> (+ chunk-len entry-len) 400)
                        (client-send client
                                     (format nil ":CLoak 353 ~a = ~a :~{~a~^ ~}"
                                             (upstream-nick upstream) chan (nreverse chunk)))
                        (setf chunk nil chunk-len 0))
                      (push entry chunk)
                      (incf chunk-len entry-len)))
                  (when chunk
                    (client-send client
                                 (format nil ":CLoak 353 ~a = ~a :~{~a~^ ~}"
                                         (upstream-nick upstream) chan (nreverse chunk))))))))
          ;; End of NAMES
          (client-send client
                       (format nil ":CLoak 366 ~a ~a :End of /NAMES list"
                               (upstream-nick upstream) chan))
          ;; Small delay between channels
          (sleep 0.02)))
      ;; Clear AWAY now that a client is attached
      (bouncer--set-away bouncer client network-name nil)
      ;; Fire attach hooks BEFORE playback so modules (e.g. clientbuffer)
      ;; can set client-last-playback to their stored position
      (cloak.modules:run-client-attach-hooks bouncer client user-name network-name)
      ;; Playback buffered messages
      (playback-buffer bouncer client user-name network-name))))

(defun detach-client (bouncer client)
  "Detach CLIENT from BOUNCER."
  ;; Fire module hooks before removing
  (cloak.modules:run-client-detach-hooks bouncer client)
  (let ((network (client-network client)))
    (bt:with-lock-held ((bouncer-lock bouncer))
      (setf (bouncer-clients bouncer)
            (remove client (bouncer-clients bouncer)))
      ;; Set AWAY if no clients remain for this network
      (unless (find network (bouncer-clients bouncer)
                    :key #'client-network :test #'string-equal)
        (bouncer--set-away bouncer client network t))))
  (cloak-log "[CLoak] Client detached~%"))

;;; --- Client Message Handling ---

(defun bouncer--on-client-message (bouncer user-name client line msg)
  "Handle a message from a downstream IRC client."
  ;; Run module downstream hooks first
  (let ((hook-result (cloak.modules:run-downstream-hooks bouncer client line msg)))
    (when (member hook-result '(:halt :drop))
      (return-from bouncer--on-client-message)))
  (let* ((command (cloak.protocol:irc-message-command msg))
         (network (client-network client))
         (upstream (bouncer--get-upstream bouncer user-name network)))
    (cond
      ;; QUIT - detach client, don't forward
      ((string= command "QUIT")
       (client-disconnect client))
      ;; PING from client - respond directly
      ((string= command "PING")
       (client-send client
                    (cloak.protocol:irc-pong
                     (or (first (cloak.protocol:irc-message-params msg)) "CLoak"))))
      ;; *status commands
      ((and (string= command "PRIVMSG")
            (let ((target (first (cloak.protocol:irc-message-params msg))))
              (string-equal target "*status")))
       (bouncer--handle-status bouncer user-name client msg))
      ;; JOIN - handle duplicate detection and config persistence
      ((and (string= command "JOIN") upstream)
       (let* ((channels-str (first (cloak.protocol:irc-message-params msg)))
              (channels (split-sequence:split-sequence #\, channels-str)))
         (dolist (chan channels)
           (if (gethash chan (upstream-channels upstream))
               ;; Already in channel - replay state to client instead of forwarding
               (client-send client
                            (format nil ":~a!~a@CLoak JOIN ~a"
                                    (upstream-nick upstream) user-name chan))
               ;; New channel - forward to upstream and persist to autojoin
               (progn
                 (upstream-send upstream (format nil "JOIN ~a" chan))
                 (bouncer--add-autojoin bouncer user-name network chan))))))
      ;; PART - forward and update config
      ((and (string= command "PART") upstream)
       (let ((chan (first (cloak.protocol:irc-message-params msg))))
         (upstream-send upstream line)
         (bouncer--remove-autojoin bouncer user-name network chan)))
      ;; NAMES - respond locally from tracked nick data, throttled
      ((and (string= command "NAMES") upstream)
       (let* ((chan (or (first (cloak.protocol:irc-message-params msg)) "*"))
              (nicks-ht (gethash chan (upstream-channel-nicks upstream)))
              (nick-list nil))
         (when nicks-ht
           (maphash (lambda (nick prefix)
                      (push (format nil "~a~a" prefix nick) nick-list))
                    nicks-ht))
         ;; Send RPL_NAMREPLY (353) in chunks of ~400 chars to stay under line limit
         ;; Throttle between chunks so clients like Emacs can process
         (when nick-list
           (let ((chunk nil)
                 (chunk-len 0)
                 (chunks-sent 0))
             (dolist (entry nick-list)
               (let ((entry-len (+ (length entry) 1))) ; +1 for space
                 (when (> (+ chunk-len entry-len) 400)
                   ;; Flush current chunk
                   (client-send client
                                (format nil ":CLoak 353 ~a = ~a :~{~a~^ ~}"
                                        (upstream-nick upstream) chan (nreverse chunk)))
                   (incf chunks-sent)
                   ;; Yield every 3 chunks to let client process
                   (when (zerop (mod chunks-sent 3))
                     (sleep 0.02))
                   (setf chunk nil chunk-len 0))
                 (push entry chunk)
                 (incf chunk-len entry-len)))
             ;; Flush remaining
             (when chunk
               (client-send client
                            (format nil ":CLoak 353 ~a = ~a :~{~a~^ ~}"
                                    (upstream-nick upstream) chan (nreverse chunk))))))
         ;; End of names
         (client-send client
                      (format nil ":CLoak 366 ~a ~a :End of /NAMES list"
                              (upstream-nick upstream) chan))))
      ;; WHO/WHOX - respond locally from tracked nick data (never forward to avoid flooding)
      ((and (string= command "WHO") upstream)
       (let* ((params (cloak.protocol:irc-message-params msg))
              (mask (or (first params) "*"))
              (flags (second params))
              (whox-p (and flags (plusp (length flags))
                           (char= (char flags 0) #\%)))
              ;; Extract WHOX token if present (after comma in flags)
              (token (when whox-p
                       (let ((comma (position #\, flags)))
                         (when comma (subseq flags (1+ comma))))))
              (nicks-ht (gethash mask (upstream-channel-nicks upstream))))
         (when nicks-ht
           (if whox-p
               ;; WHOX: send 354 replies with token and account field
               (maphash (lambda (nick prefix)
                          (declare (ignore prefix))
                          (client-send client
                                       (format nil ":CLoak 354 ~a ~@[~a ~]~a ~a CLoak CLoak ~a H :0 ~a"
                                               (upstream-nick upstream)
                                               token mask nick nick nick)))
                        nicks-ht)
               ;; Standard WHO: send 352 replies
               (maphash (lambda (nick prefix)
                          (declare (ignore prefix))
                          (client-send client
                                       (format nil ":CLoak 352 ~a ~a ~a CLoak CLoak ~a H :0 ~a"
                                               (upstream-nick upstream) mask
                                               nick nick nick)))
                        nicks-ht)))
         (client-send client
                      (format nil ":CLoak 315 ~a ~a :End of /WHO list"
                              (upstream-nick upstream) mask))))
      ;; PRIVMSG/NOTICE to NickServ/ChanServ - drop if it looks like
      ;; a re-identify attempt (client sending bouncer password to services)
      ((and (member command '("PRIVMSG" "NOTICE") :test #'string=)
            upstream
            (let ((target (first (cloak.protocol:irc-message-params msg))))
              (and (member target '("NickServ" "nickserv") :test #'string-equal)
                   (let ((text (second (cloak.protocol:irc-message-params msg))))
                     (and text (search "/" text))))))
       ;; Drop - client is trying to auth with NickServ using bouncer password
       (cloak-log "[CLoak] Dropped NickServ identify from client (CLoak handles SASL)~%"))
      ;; Everything else - forward to upstream
      (upstream
       ;; Track sender for echo-message dedup
       (when (member command '("PRIVMSG" "NOTICE") :test #'string=)
         (setf (bouncer-last-sender bouncer) client)
         ;; Buffer our own outgoing message (server won't echo it back)
         (let* ((target (first (cloak.protocol:irc-message-params msg)))
                (nick (upstream-nick upstream))
                (mirror-line (format nil ":~a!~a@CLoak ~a ~a :~a"
                                     nick user-name command target
                                     (second (cloak.protocol:irc-message-params msg))))
                (buffer (bouncer--get-buffer bouncer user-name network target)))
           (buffer-push buffer mirror-line)
           ;; Mirror to all OTHER attached clients in real-time so every
           ;; device sees messages sent from any client (true multi-client).
           (bt:with-lock-held ((bouncer-lock bouncer))
             (dolist (other (bouncer-clients bouncer))
               (when (and (not (eq other client))
                          (client-authenticated-p other)
                          (string-equal (client-user other) user-name)
                          (string-equal (client-network other) network))
                 (client-send other mirror-line))))))
       (upstream-send upstream line)))))

;;; --- Away Management ---

(defun bouncer--set-away (bouncer client network-name away-p)
  "Set or clear AWAY on the upstream for NETWORK-NAME.
Finds the upstream using any user who owns that network."
  (declare (ignore client))
  (maphash (lambda (key upstream)
             (declare (ignore key))
             (when (and (string-equal (upstream-network-name upstream) network-name)
                        (upstream-connected-p upstream))
               (if away-p
                   (upstream-send upstream "AWAY :Detached from CLoak")
                   (upstream-send upstream "AWAY"))))
           (bouncer-upstreams bouncer)))

;;; --- Playback ---

(defvar *playback-limit* 100
  "Fallback maximum messages replayed per channel on attach when no
config is available. The live value comes from config-playback-lines.")

(defun bouncer--playback-limit (bouncer)
  "Resolve the per-channel playback line limit from config (live), with fallback."
  (let ((cfg (bouncer-config bouncer)))
    (or (and cfg (cloak.config:config-playback-lines cfg))
        *playback-limit*)))

(defvar *playback-batch-counter* 0
  "Monotonic counter used to mint unique BATCH reference tags for playback.")

(defun bouncer--server-time-tag (universal-time)
  "Format UNIVERSAL-TIME as an IRCv3 server-time value (UTC, ISO-8601)."
  (multiple-value-bind (sec min hr day mon yr)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.000Z"
            yr mon day hr min sec)))

(defun bouncer--augment-tags (raw-line extra-tags)
  "Return RAW-LINE with EXTRA-TAGS (alist of (key . value)) prepended to its
IRCv3 message tags, preserving the rest of the line verbatim. Keys already
present on the line are left untouched."
  (let* ((has-tags (and (plusp (length raw-line)) (char= (char raw-line 0) #\@)))
         (space (and has-tags (position #\Space raw-line)))
         (existing (if (and has-tags space) (subseq raw-line 1 space) ""))
         (rest (if (and has-tags space) (subseq raw-line (1+ space)) raw-line))
         (present (mapcar #'car (cloak.protocol:parse-tags existing)))
         (new-parts (loop for (k . v) in extra-tags
                          unless (member k present :test #'string-equal)
                            collect (if v (format nil "~a=~a" k v) k)))
         (all (append (when (plusp (length existing)) (list existing))
                      new-parts)))
    (if all
        (format nil "@~{~a~^;~} ~a" all rest)
        rest)))

(defun bouncer--send-backlog-message (client msg server-time-p batch-ref)
  "Send a single stored MSG to CLIENT, adding a server-time tag (when the
client negotiated server-time) and a batch tag (when BATCH-REF is non-nil)."
  (let ((tags (append
               (when server-time-p
                 (list (cons "time" (bouncer--server-time-tag
                                     (stored-message-time msg)))))
               (when batch-ref
                 (list (cons "batch" batch-ref))))))
    (client-send client
                 (if tags
                     (bouncer--augment-tags (stored-message-raw msg) tags)
                     (stored-message-raw msg)))))

(defun playback-buffer (bouncer client user-name network-name)
  "Replay backlog to CLIENT on attach.
Caps each channel to the last N messages (N = config-playback-lines) for
context. Messages the client already saw (timestamp <= its stored playback
position) are sent inside a znc.in/playback BATCH so capable clients treat them
as read history; messages newer than that position are sent normally so they
surface as unread. server-time tags are added (for clients that negotiated the
capability) so backlog renders at its original time instead of all-new.
Throttled to avoid overwhelming single-threaded clients (e.g. Emacs)."
  (let* ((prefix (format nil "~a/~a/" user-name network-name))
         (limit (bouncer--playback-limit bouncer))
         (since (client-last-playback client))
         (server-time-p (and (member "server-time" (client-caps client)
                                     :test #'string-equal)
                             t))
         (batch-p (and (member "batch" (client-caps client) :test #'string-equal)
                       t))
         (total 0))
    ;; Replay all buffers for this network, throttled to avoid overwhelming clients
    (maphash
     (lambda (key buffer)
       (when (alex:starts-with-subseq prefix key)
         (let* ((target (subseq key (length prefix)))
                (msgs (buffer-messages-all buffer))
                ;; Always keep only the most recent LIMIT messages
                (len (length msgs))
                (capped (if (> len limit) (nthcdr (- len limit) msgs) msgs))
                (context nil)
                (unread nil)
                (sent 0))
           ;; Split the capped window into already-seen context vs. unread.
           (dolist (msg capped)
             (if (and (plusp since) (> (stored-message-time msg) since))
                 (push msg unread)
                 (push msg context)))
           (setf context (nreverse context)
                 unread (nreverse unread))
           ;; Send seen context, wrapped in a playback BATCH when supported.
           (let ((ref (when (and batch-p context)
                        (format nil "playback~d" (incf *playback-batch-counter*)))))
             (when ref
               (client-send client
                            (format nil ":CLoak BATCH +~a znc.in/playback ~a"
                                    ref target)))
             (dolist (msg context)
               (incf total) (incf sent)
               (bouncer--send-backlog-message client msg server-time-p ref)
               (when (zerop (mod sent 20)) (sleep 0.02)))
             (when ref
               (client-send client (format nil ":CLoak BATCH -~a" ref))))
           ;; Send unread messages outside any batch so they count as new.
           (dolist (msg unread)
             (incf total) (incf sent)
             (bouncer--send-backlog-message client msg server-time-p nil)
             (when (zerop (mod sent 20)) (sleep 0.02)))
           ;; Small delay between buffers to let client process
           (when capped (sleep 0.05)))))
     (bouncer-buffers bouncer))
    (when (plusp total)
      (cloak-log "[CLoak] Played back ~d messages for ~a~%" total user-name))
    ;; Update playback timestamp
    (setf (client-last-playback client) (get-universal-time))))

;;; --- Start / Stop ---

(defun start-bouncer (bouncer)
  "Start the CLoak bouncer."
  (cloak-log "~&[CLoak] Starting bouncer v~a~%"
          cloak:*version*)
  (setf (bouncer-running-p bouncer) t)
  (setf *bouncer* bouncer)
  ;; Scan for third-party plugins
  (cloak.modules:scan-plugins)
  ;; Load enabled modules
  (let ((modules (config-enabled-modules (bouncer-config bouncer))))
    (when modules
      (dolist (name modules)
        (cloak.modules:load-module name bouncer))
      (cloak-log "[CLoak] Loaded ~d module~:p~%" (length modules))))
  ;; Connect all upstreams
  (bouncer--connect-upstreams bouncer)
  ;; Start client listener
  (let ((cfg (bouncer-config bouncer)))
    (start-listener (config-listen-host cfg)
                    (config-listen-port cfg)
                    :tls-cert (config-tls-cert cfg)
                    :tls-key (config-tls-key cfg)
                    :on-connect (lambda (client)
                                  (bouncer--on-new-client bouncer client))))
  (cloak-log "[CLoak] Bouncer running~%")
  bouncer)

(defun stop-bouncer (bouncer)
  "Stop the CLoak bouncer."
  (cloak-log "[CLoak] Stopping bouncer...~%")
  (setf (bouncer-running-p bouncer) nil)
  ;; Disconnect all clients
  (bt:with-lock-held ((bouncer-lock bouncer))
    (dolist (client (bouncer-clients bouncer))
      (client-send client ":CLoak NOTICE * :Bouncer shutting down")
      (client-disconnect client))
    (setf (bouncer-clients bouncer) nil))
  ;; Disconnect all upstreams (disable reconnect first)
  (maphash (lambda (key upstream)
             (declare (ignore key))
             (setf (upstream-reconnect-p upstream) nil)
             (ignore-errors
               (upstream-send upstream
                              (cloak.protocol:irc-quit "CLoak shutting down"))
               (upstream-disconnect upstream)))
           (bouncer-upstreams bouncer))
  ;; Stop listener
  (stop-listener)
  (setf *bouncer* nil)
  (cloak-log "[CLoak] Bouncer stopped~%"))

;;; --- Autojoin Persistence ---

(defun bouncer--add-autojoin (bouncer user-name network-name channel)
  "Add CHANNEL to the autojoin list for USER-NAME's NETWORK-NAME and save."
  (let ((net-cfg (find-network user-name network-name (bouncer-config bouncer))))
    (when net-cfg
      (unless (member channel (network-autojoin net-cfg) :test #'string-equal)
        (setf (network-autojoin net-cfg)
              (append (network-autojoin net-cfg) (list channel)))
        (save-config (bouncer-config bouncer))
        (cloak-log "[CLoak] Added ~a to autojoin for ~a/~a~%"
                channel user-name network-name)))))

(defun bouncer--remove-autojoin (bouncer user-name network-name channel)
  "Remove CHANNEL from the autojoin list for USER-NAME's NETWORK-NAME and save."
  (let ((net-cfg (find-network user-name network-name (bouncer-config bouncer))))
    (when net-cfg
      (when (member channel (network-autojoin net-cfg) :test #'string-equal)
        (setf (network-autojoin net-cfg)
              (remove channel (network-autojoin net-cfg) :test #'string-equal))
        (save-config (bouncer-config bouncer))
        (cloak-log "[CLoak] Removed ~a from autojoin for ~a/~a~%"
                channel user-name network-name)))))

;;; --- *status Commands ---

(defun bouncer--status-reply (client text)
  "Send a NOTICE from *status to CLIENT."
  (client-send client (format nil ":*status!status@CLoak NOTICE ~a :~a"
                               (or (client-nick client) "*") text)))

(defun bouncer--handle-status (bouncer user-name client msg)
  "Handle a /msg *status command from CLIENT."
  (let* ((text (second (cloak.protocol:irc-message-params msg)))
         (parts (and text (split-sequence:split-sequence #\Space text
                            :remove-empty-subseqs t)))
         (cmd (string-downcase (or (first parts) "")))
         (args (rest parts)))
    (cond
      ((string= cmd "help")
       (bouncer--status-reply client "CLoak bouncer commands:")
       (bouncer--status-reply client "  help           - Show this help")
       (bouncer--status-reply client "  version        - Show CLoak version")
       (bouncer--status-reply client "  listnets       - List configured networks")
       (bouncer--status-reply client "  listchans      - List joined channels")
       (bouncer--status-reply client "  listclients    - List connected clients")
       (bouncer--status-reply client "  connect <net>  - Connect to a network")
       (bouncer--status-reply client "  disconnect <net> - Disconnect from a network")
       (bouncer--status-reply client "  jump <net>     - Reconnect to a network")
       (bouncer--status-reply client "  uptime         - Show bouncer uptime")
       (bouncer--status-reply client "  saveconfig     - Save current configuration")
       (bouncer--status-reply client "  reloadconfig   - Reload configuration from disk"))

      ((string= cmd "version")
       (bouncer--status-reply client
        (format nil "CLoak v~a" cloak:*version*)))

      ((string= cmd "listnets")
       (let ((user-cfg (find-user user-name (bouncer-config bouncer))))
         (if user-cfg
             (dolist (net (user-networks user-cfg))
               (let* ((key (bouncer--upstream-key user-name (network-name net)))
                      (up (gethash key (bouncer-upstreams bouncer)))
                      (status (if (and up (upstream-connected-p up)) "connected" "disconnected")))
                 (bouncer--status-reply client
                  (format nil "  ~a (~a:~d) [~a]"
                          (network-name net) (network-server net)
                          (network-port net) status))))
             (bouncer--status-reply client "No networks configured."))))

      ((string= cmd "listchans")
       (let ((upstream (bouncer--get-upstream bouncer user-name
                         (client-network client))))
         (if (and upstream (> (hash-table-count (upstream-channels upstream)) 0))
             (maphash (lambda (chan _v)
                        (declare (ignore _v))
                        (bouncer--status-reply client (format nil "  ~a" chan)))
                      (upstream-channels upstream))
             (bouncer--status-reply client "No channels joined."))))

      ((string= cmd "listclients")
       (bt:with-lock-held ((bouncer-lock bouncer))
         (let ((count 0))
           (dolist (c (bouncer-clients bouncer))
             (when (string-equal (client-network c) (client-network client))
               (incf count)
               (bouncer--status-reply client
                (format nil "  ~a [~a]" (or (client-nick c) "?")
                        (client-network c)))))
           (bouncer--status-reply client (format nil "~d client(s) attached." count)))))

      ((string= cmd "connect")
       (let ((net-name (first args)))
         (if net-name
             (let ((upstream (bouncer--get-upstream bouncer user-name net-name)))
               (if upstream
                   (if (upstream-connected-p upstream)
                       (bouncer--status-reply client
                        (format nil "Already connected to ~a." net-name))
                       (progn
                         (bouncer--status-reply client
                          (format nil "Connecting to ~a..." net-name))
                         (bt:make-thread
                          (lambda () (upstream-connect upstream))
                          :name "cloak-reconnect")))
                   (bouncer--status-reply client
                    (format nil "Unknown network: ~a" net-name))))
             (bouncer--status-reply client "Usage: connect <network>"))))

      ((string= cmd "disconnect")
       (let ((net-name (first args)))
         (if net-name
             (let ((upstream (bouncer--get-upstream bouncer user-name net-name)))
               (if upstream
                   (progn
                     (setf (upstream-reconnect-p upstream) nil)
                     (upstream-disconnect upstream)
                     (bouncer--status-reply client
                      (format nil "Disconnected from ~a." net-name)))
                   (bouncer--status-reply client
                    (format nil "Unknown network: ~a" net-name))))
             (bouncer--status-reply client "Usage: disconnect <network>"))))

      ((string= cmd "jump")
       (let ((net-name (first args)))
         (if net-name
             (let ((upstream (bouncer--get-upstream bouncer user-name net-name)))
               (if upstream
                   (progn
                     (bouncer--status-reply client
                      (format nil "Reconnecting to ~a..." net-name))
                     (setf (upstream-reconnect-p upstream) t)
                     (when (upstream-connected-p upstream)
                       (upstream-disconnect upstream))
                     (bt:make-thread
                      (lambda () (upstream-connect upstream))
                      :name "cloak-jump"))
                   (bouncer--status-reply client
                    (format nil "Unknown network: ~a" net-name))))
             (bouncer--status-reply client "Usage: jump <network>"))))

      ((string= cmd "uptime")
       (bouncer--status-reply client
        (format nil "Bouncer running since ~a"
                (local-time:format-timestring nil
                 (local-time:universal-to-timestamp
                  (bouncer-start-time bouncer))))))

      ((string= cmd "saveconfig")
       (handler-case
           (progn
             (save-config (bouncer-config bouncer))
             (bouncer--status-reply client "Configuration saved."))
         (error (e)
           (bouncer--status-reply client (format nil "Save failed: ~a" e)))))

      ((string= cmd "reloadconfig")
       (handler-case
           (let ((new-config (load-config)))
             (setf (bouncer-config bouncer) new-config)
             (bouncer--status-reply client "Configuration reloaded."))
         (error (e)
           (bouncer--status-reply client (format nil "Reload failed: ~a" e)))))

      (t
       (bouncer--status-reply client
        (format nil "Unknown command: ~a (try 'help')" cmd))))))

;;; --- New Client Handling ---

(defun bouncer--client-ip (client)
  "Extract the remote IP address string from CLIENT's socket, or \"unknown\"."
  (handler-case
      (let ((sock (slot-value client 'cloak.downstream::socket)))
        (if sock
            (format nil "~a" (iolib:remote-host sock))
            "unknown"))
    (error () "unknown")))

(defun bouncer--on-new-client (bouncer client)
  "Handle a newly connected client. Wait for auth then attach."
  ;; Run new-connection hooks (fail2ban can reject here)
  (let ((ip (bouncer--client-ip client)))
    (when (eq :drop (cloak.modules:run-new-connection-hooks bouncer ip))
      (client-send client ":CLoak NOTICE * :Connection refused")
      (client-disconnect client)
      (return-from bouncer--on-new-client)))
  ;; Set a temporary handler that processes registration
  (setf (client-message-handler client)
        (lambda (client line msg)
          (declare (ignore line))
          (bouncer--handle-client-auth bouncer client msg))))

(defparameter *downstream-caps* '("server-time" "message-tags" "batch")
  "IRCv3 capabilities CLoak offers to downstream clients. server-time lets
clients render replayed backlog at its original time (so it is not all marked
new); batch lets them treat the backlog block as history.")

(defun bouncer--handle-client-cap (client msg)
  "Handle a CAP subcommand from a client during registration.
Returns T if the client just finished negotiation (CAP END) and auth should
proceed, NIL otherwise."
  (let* ((params (cloak.protocol:irc-message-params msg))
         (subcmd (first params)))
    (cond
      ((string-equal subcmd "LS")
       (setf (client-cap-negotiating-p client) t)
       (client-send client
                    (format nil ":CLoak CAP * LS :~{~a~^ ~}" *downstream-caps*))
       nil)
      ((string-equal subcmd "REQ")
       (setf (client-cap-negotiating-p client) t)
       (let* ((req-str (or (second params) ""))
              (requested (remove "" (split-sequence:split-sequence #\Space req-str)
                                 :test #'string=)))
         (if (and requested
                  (every (lambda (c) (member c *downstream-caps* :test #'string-equal))
                         requested))
             (progn
               (dolist (c requested)
                 (pushnew c (client-caps client) :test #'string-equal))
               (client-send client (format nil ":CLoak CAP * ACK :~a" req-str)))
             (client-send client (format nil ":CLoak CAP * NAK :~a" req-str))))
       nil)
      ((string-equal subcmd "LIST")
       (client-send client
                    (format nil ":CLoak CAP * LIST :~{~a~^ ~}" (client-caps client)))
       nil)
      ((string-equal subcmd "END")
       (setf (client-cap-negotiating-p client) nil)
       t)
      (t nil))))

(defun bouncer--handle-client-auth (bouncer client msg)
  "Process authentication from a new CLIENT."
  (let ((command (cloak.protocol:irc-message-command msg)))
    ;; Handle CAP negotiation during registration. Auth is deferred until the
    ;; client sends CAP END so that negotiated caps (server-time, batch) are
    ;; known before the attach burst and backlog playback are sent.
    (when (string= command "CAP")
      (when (and (bouncer--handle-client-cap client msg)
                 (client-user-received-p client))
        (bouncer--try-client-auth bouncer client))
      (return-from bouncer--handle-client-auth))
    (when (string= command "USER")
      (setf (client-user-received-p client) t)
      ;; If the client is still negotiating caps, wait for CAP END.
      (unless (client-cap-negotiating-p client)
        (bouncer--try-client-auth bouncer client)))))

(defun bouncer--try-client-auth (bouncer client)
  "Validate credentials for CLIENT and attach, or reject."
  (block nil
      ;; Try to authenticate - PASS should contain user/network:password
      (let* ((pass-data (or (client-user client) ""))
             (slash-pos (position #\/ pass-data))
             (colon-pos (position #\: pass-data :from-end t)))
        (if (and slash-pos colon-pos (< slash-pos colon-pos))
            (let* ((user-name (subseq pass-data 0 slash-pos))
                   (network (subseq pass-data (1+ slash-pos) colon-pos))
                   (password (subseq pass-data (1+ colon-pos)))
                   (user-cfg (find-user user-name (bouncer-config bouncer))))
              (if (and user-cfg
                       (verify-password password (user-password-hash user-cfg)))
                  ;; Auth success
                  (progn
                    (cloak-log "[CLoak] Authenticated: ~a -> ~a~%" user-name network)
                    (attach-client bouncer client user-name network))
                  ;; Auth failure
                  (progn
                    (cloak.modules:run-auth-failure-hooks bouncer
                      (bouncer--client-ip client))
                    (client-send client ":CLoak 464 * :Password incorrect")
                    (client-disconnect client))))
            ;; Bad format
            (progn
              (cloak.modules:run-auth-failure-hooks bouncer
                (bouncer--client-ip client))
              (client-send client
                           ":CLoak NOTICE * :Use PASS user/network:password to authenticate")
              (client-disconnect client))))))
