;;;; modules-builtin.lisp - Built-in modules for CLoak
;;;; These modules extract functionality that was previously hardcoded in the bouncer.

(in-package #:cloak.modules)

;;; -------------------------------------------------------
;;; ctcp-version - Respond to CTCP VERSION requests
;;; -------------------------------------------------------

(defclass ctcp-version-module (module) ()
  (:documentation "Responds to CTCP VERSION requests with CLoak version info."))

(defmethod on-upstream-message ((mod ctcp-version-module) bouncer upstream raw-line msg)
  (declare (ignore bouncer raw-line))
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (string= command "PRIVMSG")
      (let ((text (second (cloak.protocol:irc-message-params msg))))
        (when (and text
                   (> (length text) 2)
                   (char= (char text 0) #\Soh)
                   (search "VERSION" text))
          (let ((sender (cloak.protocol:source-nick
                         (cloak.protocol:irc-message-source msg))))
            (when sender
              (cloak.upstream:upstream-send upstream
                (format nil "NOTICE ~a :~aCLoak v~a - Common Lisp IRC Bouncer~a"
                        sender (string #\Soh)
                        cloak:*version*
                        (string #\Soh)))))
          ;; Don't halt - let the PRIVMSG still be relayed/buffered
          nil)))))

(register-module "ctcp-version" 'ctcp-version-module
  :description "Responds to CTCP VERSION requests with CLoak version info"
  :scope :global)

;;; -------------------------------------------------------
;;; block-motd - Suppress MOTD from being relayed to clients
;;; -------------------------------------------------------

(defclass block-motd-module (module) ()
  (:documentation "Blocks MOTD messages (375/372/376) from being relayed to clients."))

(defmethod on-upstream-message ((mod block-motd-module) bouncer upstream raw-line msg)
  (declare (ignore bouncer upstream raw-line))
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (member command '("375" "372" "376") :test #'string=)
      :drop)))

(register-module "block-motd" 'block-motd-module
  :description "Suppresses MOTD (Message of the Day) from being relayed to clients"
  :scope :global)

;;; -------------------------------------------------------
;;; auto-away - Set AWAY when no clients are attached
;;; -------------------------------------------------------

(defclass auto-away-module (module)
  ((away-message :initarg :away-message :accessor auto-away-message
                 :initform "Detached from CLoak"))
  (:documentation "Automatically sets AWAY status when no clients are attached."))

(defmethod module-settings-html ((mod auto-away-module))
  (spinneret:with-html-string
    (:div :class "form-group"
      (:label :class "form-label" "Away Message")
      (:input :class "form-input" :type "text" :name "away-message"
              :value (auto-away-message mod)
              :placeholder "Detached from CLoak"))))

(defmethod on-save-settings ((mod auto-away-module) params)
  (let ((msg (cdr (assoc :away-message params))))
    (when (and msg (plusp (length msg)))
      (setf (auto-away-message mod) msg))))

(register-module "auto-away" 'auto-away-module
  :description "Sets AWAY when no IRC clients are connected, clears it when one attaches"
  :scope :global)

;;; -------------------------------------------------------
;;; nickserv - Auto-identify with NickServ on connect
;;; -------------------------------------------------------

(defclass nickserv-module (module)
  ((password :initarg :password :accessor nickserv-password
             :initform nil))
  (:documentation "Automatically identifies with NickServ after connecting to a network."))

(defmethod on-upstream-message ((mod nickserv-module) bouncer upstream raw-line msg)
  (declare (ignore bouncer raw-line))
  ;; On 001 (RPL_WELCOME), send IDENTIFY to NickServ
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (and (string= command "001")
               (nickserv-password mod))
      (cloak.upstream:upstream-send upstream
        (format nil "PRIVMSG NickServ :IDENTIFY ~a"
                (nickserv-password mod)))
      (format t "[CLoak] NickServ: sent IDENTIFY for ~a~%"
              (cloak.upstream:upstream-network-name upstream))))
  nil)

(defmethod module-settings-html ((mod nickserv-module))
  (spinneret:with-html-string
    (:div :class "form-group"
      (:label :class "form-label" "NickServ Password")
      (:input :class "form-input" :type "password" :name "password"
              :placeholder "NickServ password"
              :value (or (nickserv-password mod) "")))))

(defmethod on-save-settings ((mod nickserv-module) params)
  (let ((pw (cdr (assoc :password params))))
    (when (and pw (plusp (length pw)))
      (setf (nickserv-password mod) pw))))

(register-module "nickserv" 'nickserv-module
  :description "Automatically identifies with NickServ after connecting"
  :scope :network)

;;; -------------------------------------------------------
;;; perform - Run IRC commands on connect
;;; -------------------------------------------------------

(defclass perform-module (module)
  ((commands :initform nil :accessor perform-commands
             :documentation "List of raw IRC commands to send after connecting."))
  (:documentation "Sends configured IRC commands after connecting to a network."))

(defmethod on-load ((mod perform-module) bouncer)
  (declare (ignore bouncer))
  (let ((data (load-module-data "perform")))
    (when data
      (setf (perform-commands mod) (getf data :commands))))
  (format t "[CLoak] Module loaded: perform (~d commands)~%"
          (length (perform-commands mod))))

(defmethod on-upstream-connect ((mod perform-module) bouncer upstream)
  (declare (ignore bouncer))
  (when (perform-commands mod)
    (dolist (cmd (perform-commands mod))
      (when (and cmd (plusp (length cmd)))
        (cloak.upstream:upstream-send upstream cmd)))
    (format t "[CLoak] Perform: sent ~d commands to ~a~%"
            (length (perform-commands mod))
            (cloak.upstream:upstream-network-name upstream))))

(defmethod module-settings-html ((mod perform-module))
  (spinneret:with-html-string
    (:div :class "form-group"
      (:label :class "form-label" "Commands (one per line)")
      (:textarea :class "form-input" :name "commands"
                 :rows "6" :placeholder "MODE $me +x
JOIN #channel
PRIVMSG NickServ :GHOST myname mypass"
        (format nil "~{~a~^~%~}" (or (perform-commands mod) '()))))))

(defmethod on-save-settings ((mod perform-module) params)
  (let ((raw (cdr (assoc :commands params))))
    (when raw
      (setf (perform-commands mod)
            (remove-if (lambda (s) (zerop (length (string-trim " " s))))
                       (uiop:split-string raw :separator '(#\Newline #\Return))))
      (save-module-data "perform"
                        (list :commands (perform-commands mod))))))

(register-module "perform" 'perform-module
  :description "Runs IRC commands after connecting to a network (MODE, JOIN, etc.)"
  :scope :global)

;;; -------------------------------------------------------
;;; keepnick - Reclaim desired nick when available
;;; -------------------------------------------------------

(defclass keepnick-module (module)
  ((interval :initform 30 :accessor keepnick-interval
             :documentation "Seconds between nick reclaim attempts."))
  (:documentation "Periodically attempts to reclaim the configured nick if it was taken."))

(defmethod on-load ((mod keepnick-module) bouncer)
  (format t "[CLoak] Module loaded: keepnick~%")
  ;; Start a timer that checks all upstreams
  (start-module-timer mod "keepnick" (keepnick-interval mod)
    (lambda ()
      (maphash (lambda (key upstream)
                 (declare (ignore key))
                 (when (cloak.upstream:upstream-connected-p upstream)
                   (let* ((config (cloak.upstream:upstream-config upstream))
                          (desired (cloak.config:network-nick config))
                          (current (cloak.upstream:upstream-nick upstream)))
                     (when (and desired (not (string-equal current desired)))
                       (cloak.upstream:upstream-send upstream
                         (cloak.protocol:irc-nick desired))))))
               (cloak.bouncer:bouncer-upstreams bouncer)))))

(defmethod on-upstream-message ((mod keepnick-module) bouncer upstream raw-line msg)
  (declare (ignore bouncer raw-line))
  ;; If someone with our desired nick quits or changes nick, try to reclaim
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (or (string= command "QUIT") (string= command "NICK"))
      (let* ((config (cloak.upstream:upstream-config upstream))
             (desired (cloak.config:network-nick config))
             (who (cloak.protocol:source-nick (cloak.protocol:irc-message-source msg))))
        (when (and desired
                   (not (string-equal (cloak.upstream:upstream-nick upstream) desired))
                   (string-equal who desired))
          (cloak.upstream:upstream-send upstream
            (cloak.protocol:irc-nick desired))))))
  nil)

(defmethod module-settings-html ((mod keepnick-module))
  (spinneret:with-html-string
    (:div :class "form-group"
      (:label :class "form-label" "Check Interval (seconds)")
      (:input :class "form-input" :type "number" :name "interval"
              :value (format nil "~d" (keepnick-interval mod))))))

(defmethod on-save-settings ((mod keepnick-module) params)
  (let ((val (cdr (assoc :interval params))))
    (when (and val (plusp (length val)))
      (let ((n (parse-integer val :junk-allowed t)))
        (when (and n (> n 5))
          (setf (keepnick-interval mod) n))))))

(register-module "keepnick" 'keepnick-module
  :description "Periodically reclaims your configured nick if it was taken"
  :scope :global)

;;; -------------------------------------------------------
;;; stickychan - Auto-rejoin channels on kick
;;; -------------------------------------------------------

(defclass stickychan-module (module)
  ((rejoin-delay :initform 2 :accessor stickychan-delay
                 :documentation "Seconds to wait before rejoining."))
  (:documentation "Automatically rejoins channels after being kicked."))

(defmethod on-channel-kick ((mod stickychan-module) bouncer upstream channel kicker reason)
  (declare (ignore bouncer kicker reason))
  (let ((delay (stickychan-delay mod)))
    (bt:make-thread
     (lambda ()
       (sleep delay)
       (when (cloak.upstream:upstream-connected-p upstream)
         (cloak.upstream:upstream-send upstream
           (cloak.protocol:irc-join channel))
         (format t "[CLoak] StickyChan: rejoined ~a on ~a~%"
                 channel (cloak.upstream:upstream-network-name upstream))))
     :name (format nil "stickychan-rejoin-~a" channel))))

(defmethod module-settings-html ((mod stickychan-module))
  (spinneret:with-html-string
    (:div :class "form-group"
      (:label :class "form-label" "Rejoin Delay (seconds)")
      (:input :class "form-input" :type "number" :name "delay"
              :value (format nil "~d" (stickychan-delay mod))))))

(defmethod on-save-settings ((mod stickychan-module) params)
  (let ((val (cdr (assoc :delay params))))
    (when (and val (plusp (length val)))
      (let ((n (parse-integer val :junk-allowed t)))
        (when (and n (>= n 0))
          (setf (stickychan-delay mod) n))))))

(register-module "stickychan" 'stickychan-module
  :description "Automatically rejoins channels after being kicked"
  :scope :global)

;;; -------------------------------------------------------
;;; buffextras - Buffer JOIN/PART/NICK/QUIT/TOPIC/MODE
;;; -------------------------------------------------------

(defclass buffextras-module (module) ()
  (:documentation "Buffers non-message events (JOIN/PART/NICK/QUIT/TOPIC/MODE) so they appear in playback."))

(defmethod on-upstream-message ((mod buffextras-module) bouncer upstream raw-line msg)
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (member command '("JOIN" "PART" "QUIT" "NICK" "TOPIC" "MODE")
                  :test #'string=)
      (let* ((network (cloak.upstream:upstream-network-name upstream))
             ;; Find the user that owns this upstream
             (user-name nil))
        (maphash (lambda (key _up)
                   (declare (ignore _up))
                   (let ((slash (position #\/ key)))
                     (when (and slash
                                (string-equal (subseq key (1+ slash)) network))
                       (setf user-name (subseq key 0 slash)))))
                 (cloak.bouncer:bouncer-upstreams bouncer))
        (when user-name
          (let* ((target (cond
                           ;; JOIN/PART/TOPIC/MODE have channel as first param
                           ((member command '("JOIN" "PART" "TOPIC" "MODE")
                                    :test #'string=)
                            (first (cloak.protocol:irc-message-params msg)))
                           ;; NICK/QUIT affect all channels - skip buffering
                           ;; (would need to buffer per-channel, too complex)
                           (t nil)))
                 (buf-key (when target
                            (format nil "~a/~a/~a"
                                    user-name network (string-downcase target))))
                 (buffer (when buf-key
                           (gethash buf-key
                                    (cloak.bouncer:bouncer-buffers bouncer)))))
            (when buffer
              (cloak.buffer:buffer-push buffer raw-line)))))))
  nil)

(register-module "buffextras" 'buffextras-module
  :description "Buffers JOIN/PART/TOPIC/MODE events for playback alongside messages"
  :scope :global)

;;; -------------------------------------------------------
;;; savebuff - Persist message buffers to disk
;;; -------------------------------------------------------

(defclass savebuff-module (module)
  ((save-interval :initform 300 :accessor savebuff-interval
                  :documentation "Seconds between auto-saves."))
  (:documentation "Periodically saves message buffers to disk and restores them on startup."))

(defmethod on-load ((mod savebuff-module) bouncer)
  (format t "[CLoak] Module loaded: savebuff~%")
  ;; Restore saved buffers
  (let ((data (load-module-data "savebuff")))
    (when data
      (let ((count 0))
        (loop for (key . messages) in data
              do (let ((buffer (gethash key (cloak.bouncer:bouncer-buffers bouncer))))
                   (unless buffer
                     ;; Create buffer if it doesn't exist yet
                     (setf buffer (cloak.buffer:make-message-buffer :capacity 500))
                     (setf (gethash key (cloak.bouncer:bouncer-buffers bouncer)) buffer))
                   (dolist (entry messages)
                     (let ((raw (getf entry :raw))
                           (msgid (getf entry :msgid)))
                       (when raw
                         (cloak.buffer:buffer-push buffer raw msgid)
                         (incf count))))))
        (when (plusp count)
          (format t "[CLoak] Savebuff: restored ~d messages across ~d buffers~%"
                  count (length data))))))
  ;; Start periodic save timer
  (start-module-timer mod "save" (savebuff-interval mod)
    (lambda ()
      (savebuff--save bouncer))))

(defun savebuff--save (bouncer)
  "Save all non-empty buffers to disk."
  (let ((data nil))
    (maphash (lambda (key buffer)
               (let ((messages (cloak.buffer:buffer-messages-all buffer)))
                 (when messages
                   (push (cons key
                               (mapcar (lambda (m)
                                         (list :raw (cloak.buffer:stored-message-raw m)
                                               :time (cloak.buffer:stored-message-time m)
                                               :msgid (cloak.buffer:stored-message-msgid m)))
                                       messages))
                         data))))
             (cloak.bouncer:bouncer-buffers bouncer))
    (when data
      (save-module-data "savebuff" data))))

(defmethod on-unload ((mod savebuff-module) bouncer)
  ;; Save before unloading
  (savebuff--save bouncer)
  ;; Call parent to stop timers
  (call-next-method))

(defmethod module-settings-html ((mod savebuff-module))
  (spinneret:with-html-string
    (:div :class "form-group"
      (:label :class "form-label" "Auto-save Interval (seconds)")
      (:input :class "form-input" :type "number" :name "interval"
              :value (format nil "~d" (savebuff-interval mod))))))

(defmethod on-save-settings ((mod savebuff-module) params)
  (let ((val (cdr (assoc :interval params))))
    (when (and val (plusp (length val)))
      (let ((n (parse-integer val :junk-allowed t)))
        (when (and n (> n 30))
          (setf (savebuff-interval mod) n))))))

(register-module "savebuff" 'savebuff-module
  :description "Saves message buffers to disk periodically and restores on restart"
  :scope :global)

;;; -------------------------------------------------------
;;; clearbufferonmsg - Clear buffer when user sends a message
;;; -------------------------------------------------------

(defclass clearbufferonmsg-module (module) ()
  (:documentation "Clears the playback buffer for a target when the user sends a message there,
preventing old messages from being replayed after the user has already read them."))

(defmethod on-downstream-message ((mod clearbufferonmsg-module) bouncer client raw-line msg)
  (declare (ignore raw-line))
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (member command '("PRIVMSG" "NOTICE") :test #'string=)
      (let* ((target (first (cloak.protocol:irc-message-params msg)))
             (user-name (cloak.downstream:client-user client))
             (network (cloak.downstream:client-network client)))
        (when (and target user-name network)
          (let* ((buf-key (format nil "~a/~a/~a"
                                  user-name network (string-downcase target)))
                 (buffer (gethash buf-key (cloak.bouncer:bouncer-buffers bouncer))))
            (when buffer
              (cloak.buffer:buffer-clear buffer)))))))
  nil)

(register-module "clearbufferonmsg" 'clearbufferonmsg-module
  :description "Clears playback buffer for a target when you send a message there"
  :scope :global)

;;; -------------------------------------------------------
;;; log - Log messages to disk
;;; -------------------------------------------------------

(defclass log-module (module)
  ((log-dir :initform nil :accessor log-log-dir)
   (log-joins :initform nil :accessor log-log-joins
              :documentation "If T, also log JOIN/PART/QUIT events."))
  (:documentation "Logs channel and query messages to timestamped files on disk."))

(defmethod on-load ((mod log-module) bouncer)
  (declare (ignore bouncer))
  (let ((dir (merge-pathnames "cloak/logs/"
                              (cloak.config:xdg-config-home))))
    (ensure-directories-exist dir)
    (setf (log-log-dir mod) dir))
  (let ((data (load-module-data "log")))
    (when data
      (setf (log-log-joins mod) (getf data :log-joins))))
  (format t "[CLoak] Module loaded: log (dir: ~a)~%"
          (namestring (log-log-dir mod))))

(defun log--write-entry (mod network target line)
  "Append LINE to the log file for NETWORK/TARGET."
  (let* ((today (multiple-value-bind (sec min hour day month year)
                    (decode-universal-time (get-universal-time))
                  (declare (ignore sec min hour))
                  (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))
         (net-dir (merge-pathnames (format nil "~a/" (string-downcase network))
                                   (log-log-dir mod)))
         (path (merge-pathnames (format nil "~a_~a.log"
                                        (string-downcase target) today)
                                net-dir)))
    (ensure-directories-exist path)
    (handler-case
        (with-open-file (out path :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
          (multiple-value-bind (sec min hour)
              (decode-universal-time (get-universal-time))
            (format out "[~2,'0d:~2,'0d:~2,'0d] ~a~%" hour min sec line)))
      (error (e)
        (format t "[CLoak] Log write error: ~a~%" e)))))

(defmethod on-upstream-message ((mod log-module) bouncer upstream raw-line msg)
  (declare (ignore bouncer raw-line))
  (let ((command (cloak.protocol:irc-message-command msg))
        (network (cloak.upstream:upstream-network-name upstream)))
    (cond
      ;; Log PRIVMSG and NOTICE
      ((member command '("PRIVMSG" "NOTICE") :test #'string=)
       (let* ((source (cloak.protocol:source-nick
                       (cloak.protocol:irc-message-source msg)))
              (target (first (cloak.protocol:irc-message-params msg)))
              (text (second (cloak.protocol:irc-message-params msg)))
              ;; If target is our nick, log under sender's name (query)
              (log-target (if (string-equal target
                               (cloak.upstream:upstream-nick upstream))
                              source
                              target)))
         (log--write-entry mod network log-target
           (format nil "<~a> ~a" source text))))
      ;; Optionally log channel events
      ((and (log-log-joins mod)
            (member command '("JOIN" "PART" "QUIT") :test #'string=))
       (let ((source (cloak.protocol:source-nick
                      (cloak.protocol:irc-message-source msg)))
             (target (first (cloak.protocol:irc-message-params msg))))
         (when target
           (log--write-entry mod network target
             (format nil "*** ~a ~a" source (string-downcase command))))))))
  nil)

(defmethod module-settings-html ((mod log-module))
  (spinneret:with-html-string
    (:div :class "form-group"
      (:label :class "form-label"
        (:input :type "checkbox" :name "log-joins"
                :checked (when (log-log-joins mod) "checked"))
        " Log JOIN/PART/QUIT events"))))

(defmethod on-save-settings ((mod log-module) params)
  (setf (log-log-joins mod) (not (null (cdr (assoc :log-joins params)))))
  (save-module-data "log" (list :log-joins (log-log-joins mod))))

(register-module "log" 'log-module
  :description "Logs channel and query messages to timestamped files on disk"
  :scope :global)

;;; -------------------------------------------------------
;;; route-replies - Route server replies to the requesting client
;;; -------------------------------------------------------

(defclass route-replies-module (module)
  ((pending :initform (make-hash-table :test 'equal) :accessor rr-pending
            :documentation "Maps command-key -> client for pending queries.")
   (lock :initform (bt:make-lock "route-replies") :accessor rr-lock))
  (:documentation "Routes server query replies (WHOIS, WHO, LIST, etc.) to the
client that sent the request, instead of broadcasting to all clients."))

;; Map of query command -> list of (start-numeric ... end-numeric)
(defparameter *reply-ranges*
  '(("WHOIS"  . ((311 . 313) (317 . 319) (301 . 301) (330 . 330)
                 (338 . 338) (378 . 378) (671 . 671)))
    ("WHOWAS" . ((312 . 312) (314 . 314) (369 . 369)))
    ("WHO"    . ((352 . 352) (315 . 315) (354 . 354)))
    ("LIST"   . ((321 . 323)))
    ("LINKS"  . ((364 . 365)))
    ("MODE"   . ((324 . 324) (329 . 329) (367 . 368)))
    ("NAMES"  . ((353 . 353) (366 . 366)))
    ("TOPIC"  . ((332 . 333))))
  "IRC commands and the numeric reply ranges they generate.")

;; Build reverse lookup: numeric -> list of command keys
(defvar *numeric-to-commands* (make-hash-table :test 'eql))

(dolist (entry *reply-ranges*)
  (dolist (range (cdr entry))
    (loop for n from (car range) to (cdr range)
          do (pushnew (car entry) (gethash n *numeric-to-commands*) :test #'string=))))

;; End-of-reply numerics (signal that the reply block is finished)
(defparameter *end-numerics*
  '(318 ; end of WHOIS
    369 ; end of WHOWAS
    315 ; end of WHO
    323 ; end of LIST
    365 ; end of LINKS
    368 ; end of BANLIST
    366 ; end of NAMES
    )
  "Numerics that mark end of a reply block.")

(defmethod on-downstream-message ((mod route-replies-module) bouncer client raw-line msg)
  (declare (ignore bouncer raw-line))
  (let* ((command (cloak.protocol:irc-message-command msg))
         (ranges (cdr (assoc command *reply-ranges* :test #'string=))))
    (when ranges
      (bt:with-lock-held ((rr-lock mod))
        (setf (gethash command (rr-pending mod)) client))))
  nil)

(defmethod on-upstream-message ((mod route-replies-module) bouncer upstream raw-line msg)
  (declare (ignore upstream raw-line))
  (let* ((command (cloak.protocol:irc-message-command msg))
         (numeric (parse-integer command :junk-allowed t)))
    (when numeric
      (let ((query-cmds (gethash numeric *numeric-to-commands*)))
        (when query-cmds
          ;; Find which client is waiting for this
          (let ((client nil)
                (end-p (member numeric *end-numerics*)))
            (bt:with-lock-held ((rr-lock mod))
              (dolist (cmd query-cmds)
                (let ((c (gethash cmd (rr-pending mod))))
                  (when c
                    (setf client c)
                    ;; Clear pending on end-of-reply
                    (when end-p
                      (remhash cmd (rr-pending mod)))
                    (return)))))
            (when client
              ;; Send directly to the requesting client
              (cloak.downstream:client-send client
                (cloak.protocol:format-message msg))
              ;; Drop from normal relay so other clients don't see it
              :drop))))))
  ;; Return nil for non-numeric or non-reply messages
  )

(defmethod on-client-detach ((mod route-replies-module) bouncer client)
  (declare (ignore bouncer))
  ;; Remove any pending queries for this client
  (bt:with-lock-held ((rr-lock mod))
    (maphash (lambda (key val)
               (when (eq val client)
                 (remhash key (rr-pending mod))))
             (rr-pending mod))))

(register-module "route-replies" 'route-replies-module
  :description "Routes WHOIS/WHO/LIST/etc. replies only to the requesting client"
  :scope :global)

;;; -------------------------------------------------------
;;; clientnotify - Notify when another client connects/disconnects
;;; -------------------------------------------------------

(defclass clientnotify-module (module) ()
  (:documentation "Sends a NOTICE to all attached clients when another client
attaches to or detaches from the same network."))

(defun clientnotify--notify-others (bouncer client network-name text)
  "Send a NOTICE from *clientnotify to all OTHER clients on NETWORK-NAME."
  (bt:with-lock-held ((cloak.bouncer:bouncer-lock bouncer))
    (dolist (c (cloak.bouncer:bouncer-clients bouncer))
      (when (and (not (eq c client))
                 (cloak.downstream:client-authenticated-p c)
                 (string-equal (cloak.downstream:client-network c) network-name))
        (cloak.downstream:client-send c
          (format nil ":*clientnotify!module@CLoak NOTICE ~a :~a"
                  (or (cloak.downstream:client-nick c) "*") text))))))

(defmethod on-client-attach ((mod clientnotify-module) bouncer client user-name network-name)
  (declare (ignore user-name))
  (clientnotify--notify-others bouncer client network-name
    (format nil "Client ~a attached" (or (cloak.downstream:client-nick client) "?"))))

(defmethod on-client-detach ((mod clientnotify-module) bouncer client)
  (let ((network (cloak.downstream:client-network client)))
    (when network
      (clientnotify--notify-others bouncer client network
        (format nil "Client ~a detached" (or (cloak.downstream:client-nick client) "?"))))))

(register-module "clientnotify" 'clientnotify-module
  :description "Notifies attached clients when another client connects or disconnects"
  :scope :global)

;;; -------------------------------------------------------
;;; fail2ban - Rate-limit failed auth attempts
;;; -------------------------------------------------------

(defclass fail2ban-module (module)
  ((attempts :initform (make-hash-table :test 'equal) :accessor f2b-attempts
             :documentation "Maps IP -> (count . first-attempt-time)")
   (lock :initform (bt:make-lock "fail2ban") :accessor f2b-lock)
   (max-attempts :initform 5 :accessor f2b-max-attempts)
   (ban-duration :initform 300 :accessor f2b-ban-duration
                 :documentation "Ban duration in seconds.")
   (attempt-window :initform 60 :accessor f2b-attempt-window
                   :documentation "Time window for counting attempts.")
   (bans :initform (make-hash-table :test 'equal) :accessor f2b-bans
         :documentation "Maps IP -> ban-until universal time."))
  (:documentation "Rate-limits failed authentication attempts and temporarily bans IPs."))

(defmethod on-load ((mod fail2ban-module) bouncer)
  (declare (ignore bouncer))
  (let ((data (load-module-data "fail2ban")))
    (when data
      (setf (f2b-max-attempts mod) (or (getf data :max-attempts) 5))
      (setf (f2b-ban-duration mod) (or (getf data :ban-duration) 300))
      (setf (f2b-attempt-window mod) (or (getf data :attempt-window) 60))))
  ;; Start a cleanup timer to expire old bans/attempts
  (start-module-timer mod "cleanup" 60
    (lambda ()
      (let ((now (get-universal-time)))
        (bt:with-lock-held ((f2b-lock mod))
          ;; Remove expired bans
          (maphash (lambda (ip until)
                     (when (> now until)
                       (remhash ip (f2b-bans mod))
                       (format t "[CLoak] Fail2ban: unbanned ~a~%" ip)))
                   (f2b-bans mod))
          ;; Remove old attempt records
          (maphash (lambda (ip record)
                     (when (> now (+ (cdr record) (f2b-attempt-window mod)))
                       (remhash ip (f2b-attempts mod))))
                   (f2b-attempts mod))))))
  (format t "[CLoak] Module loaded: fail2ban (max ~d attempts, ~ds ban)~%"
          (f2b-max-attempts mod) (f2b-ban-duration mod)))

(defmethod on-new-connection ((mod fail2ban-module) bouncer client-ip)
  (declare (ignore bouncer))
  (bt:with-lock-held ((f2b-lock mod))
    (let ((ban-until (gethash client-ip (f2b-bans mod))))
      (when (and ban-until (> ban-until (get-universal-time)))
        (format t "[CLoak] Fail2ban: rejected banned IP ~a~%" client-ip)
        (return-from on-new-connection :drop))))
  nil)

(defmethod on-auth-failure ((mod fail2ban-module) bouncer client-ip)
  (declare (ignore bouncer))
  (let ((now (get-universal-time)))
    (bt:with-lock-held ((f2b-lock mod))
      (let ((record (gethash client-ip (f2b-attempts mod))))
        (if (and record (< (- now (cdr record)) (f2b-attempt-window mod)))
            ;; Within window — increment
            (let ((new-count (1+ (car record))))
              (setf (gethash client-ip (f2b-attempts mod))
                    (cons new-count (cdr record)))
              (when (>= new-count (f2b-max-attempts mod))
                ;; Ban the IP
                (setf (gethash client-ip (f2b-bans mod))
                      (+ now (f2b-ban-duration mod)))
                (remhash client-ip (f2b-attempts mod))
                (format t "[CLoak] Fail2ban: banned ~a for ~d seconds (~d failed attempts)~%"
                        client-ip (f2b-ban-duration mod) new-count)))
            ;; New window
            (setf (gethash client-ip (f2b-attempts mod))
                  (cons 1 now)))))))

(defmethod module-settings-html ((mod fail2ban-module))
  (spinneret:with-html-string
    (:div :class "form-group"
      (:label :class "form-label" "Max Failed Attempts")
      (:input :class "form-input" :type "number" :name "max-attempts"
              :value (format nil "~d" (f2b-max-attempts mod))))
    (:div :class "form-group"
      (:label :class "form-label" "Ban Duration (seconds)")
      (:input :class "form-input" :type "number" :name "ban-duration"
              :value (format nil "~d" (f2b-ban-duration mod))))
    (:div :class "form-group"
      (:label :class "form-label" "Attempt Window (seconds)")
      (:input :class "form-input" :type "number" :name "attempt-window"
              :value (format nil "~d" (f2b-attempt-window mod))))))

(defmethod on-save-settings ((mod fail2ban-module) params)
  (flet ((parse-int (key default min)
           (let ((val (cdr (assoc key params))))
             (if (and val (plusp (length val)))
                 (let ((n (parse-integer val :junk-allowed t)))
                   (if (and n (>= n min)) n default))
                 default))))
    (setf (f2b-max-attempts mod) (parse-int :max-attempts 5 1))
    (setf (f2b-ban-duration mod) (parse-int :ban-duration 300 10))
    (setf (f2b-attempt-window mod) (parse-int :attempt-window 60 10))
    (save-module-data "fail2ban"
      (list :max-attempts (f2b-max-attempts mod)
            :ban-duration (f2b-ban-duration mod)
            :attempt-window (f2b-attempt-window mod)))))

(register-module "fail2ban" 'fail2ban-module
  :description "Rate-limits failed auth and temporarily bans IPs after repeated failures"
  :scope :global)

;;; -------------------------------------------------------
;;; controlpanel - Admin commands via IRC *controlpanel
;;; -------------------------------------------------------

(defclass controlpanel-module (module) ()
  (:documentation "Provides admin commands via /msg *controlpanel for managing
users, networks, and modules without the web UI."))

(defun cp--reply (client text)
  "Send a NOTICE from *controlpanel to CLIENT."
  (cloak.downstream:client-send client
    (format nil ":*controlpanel!module@CLoak NOTICE ~a :~a"
            (or (cloak.downstream:client-nick client) "*") text)))

(defmethod on-downstream-message ((mod controlpanel-module) bouncer client raw-line msg)
  (declare (ignore raw-line))
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (string= command "PRIVMSG")
      (let ((target (first (cloak.protocol:irc-message-params msg))))
        (when (string-equal target "*controlpanel")
          (let* ((text (second (cloak.protocol:irc-message-params msg)))
                 (parts (and text (split-sequence:split-sequence #\Space text
                                    :remove-empty-subseqs t)))
                 (cmd (string-downcase (or (first parts) "")))
                 (args (rest parts))
                 (user-name (cloak.downstream:client-user client))
                 (cfg (cloak.bouncer:bouncer-config bouncer))
                 (user-cfg (cloak.config:find-user user-name cfg)))
            ;; Only admins can use controlpanel
            (unless (and user-cfg (cloak.config:user-admin-p user-cfg))
              (cp--reply client "Permission denied. Admin access required.")
              (return-from on-downstream-message :halt))
            (cond
              ;; --- Help ---
              ((string= cmd "help")
               (cp--reply client "Control panel commands:")
               (cp--reply client "  listusers                    - List all users")
               (cp--reply client "  adduser <name> <password>    - Add a user")
               (cp--reply client "  deluser <name>               - Delete a user")
               (cp--reply client "  setpassword <user> <pass>    - Change a user's password")
               (cp--reply client "  addnetwork <user> <name> <host> <port> <nick> - Add network")
               (cp--reply client "  delnetwork <user> <name>     - Delete a network")
               (cp--reply client "  listmods                     - List all modules")
               (cp--reply client "  loadmod <name>               - Load/enable a module")
               (cp--reply client "  unloadmod <name>             - Unload/disable a module"))

              ;; --- User management ---
              ((string= cmd "listusers")
               (dolist (u (cloak.config:config-users cfg))
                 (cp--reply client
                   (format nil "  ~a~a (~d network~:p)"
                           (cloak.config:user-name u)
                           (if (cloak.config:user-admin-p u) " [admin]" "")
                           (length (cloak.config:user-networks u)))))
               (cp--reply client
                 (format nil "~d user(s) total." (length (cloak.config:config-users cfg)))))

              ((string= cmd "adduser")
               (if (>= (length args) 2)
                   (let ((name (first args))
                         (pass (second args)))
                     (if (cloak.config:find-user name cfg)
                         (cp--reply client (format nil "User ~a already exists." name))
                         (progn
                           (let ((new-user (make-instance 'cloak.config:user-config
                                             :name name
                                             :password-hash (cloak.config:hash-password pass)
                                             :networks nil)))
                             (setf (cloak.config:config-users cfg)
                                   (append (cloak.config:config-users cfg) (list new-user)))
                             (cloak.config:save-config cfg)
                             (cp--reply client (format nil "User ~a created." name))))))
                   (cp--reply client "Usage: adduser <name> <password>")))

              ((string= cmd "deluser")
               (if (first args)
                   (let ((target (first args)))
                     (if (string-equal target user-name)
                         (cp--reply client "Cannot delete yourself.")
                         (if (cloak.config:find-user target cfg)
                             (progn
                               (setf (cloak.config:config-users cfg)
                                     (remove target (cloak.config:config-users cfg)
                                             :key #'cloak.config:user-name
                                             :test #'string-equal))
                               (cloak.config:save-config cfg)
                               (cp--reply client (format nil "User ~a deleted." target)))
                             (cp--reply client (format nil "User ~a not found." target)))))
                   (cp--reply client "Usage: deluser <name>")))

              ((string= cmd "setpassword")
               (if (>= (length args) 2)
                   (let* ((target (first args))
                          (pass (second args))
                          (target-cfg (cloak.config:find-user target cfg)))
                     (if target-cfg
                         (progn
                           (setf (cloak.config:user-password-hash target-cfg)
                                 (cloak.config:hash-password pass))
                           (cloak.config:save-config cfg)
                           (cp--reply client (format nil "Password updated for ~a." target)))
                         (cp--reply client (format nil "User ~a not found." target))))
                   (cp--reply client "Usage: setpassword <user> <password>")))

              ;; --- Network management ---
              ((string= cmd "addnetwork")
               (if (>= (length args) 5)
                   (let* ((target-user (first args))
                          (net-name (second args))
                          (host (third args))
                          (port (parse-integer (fourth args) :junk-allowed t))
                          (nick (fifth args))
                          (target-cfg (cloak.config:find-user target-user cfg)))
                     (cond
                       ((not target-cfg)
                        (cp--reply client (format nil "User ~a not found." target-user)))
                       ((not port)
                        (cp--reply client "Invalid port number."))
                       ((cloak.config:find-network target-user net-name cfg)
                        (cp--reply client (format nil "Network ~a already exists for ~a."
                                                  net-name target-user)))
                       (t
                        (let ((net (make-instance 'cloak.config:network-config
                                     :name net-name :server host :port port
                                     :tls (>= port 6697) :nick nick)))
                          (setf (cloak.config:user-networks target-cfg)
                                (append (cloak.config:user-networks target-cfg) (list net)))
                          (cloak.config:save-config cfg)
                          (cp--reply client
                            (format nil "Network ~a added for ~a (~a:~d)."
                                    net-name target-user host port))))))
                   (cp--reply client "Usage: addnetwork <user> <name> <host> <port> <nick>")))

              ((string= cmd "delnetwork")
               (if (>= (length args) 2)
                   (let* ((target-user (first args))
                          (net-name (second args))
                          (target-cfg (cloak.config:find-user target-user cfg)))
                     (if target-cfg
                         (if (cloak.config:find-network target-user net-name cfg)
                             (progn
                               (setf (cloak.config:user-networks target-cfg)
                                     (remove net-name (cloak.config:user-networks target-cfg)
                                             :key #'cloak.config:network-name
                                             :test #'string-equal))
                               (cloak.config:save-config cfg)
                               (cp--reply client
                                 (format nil "Network ~a removed from ~a." net-name target-user)))
                             (cp--reply client (format nil "Network ~a not found." net-name)))
                         (cp--reply client (format nil "User ~a not found." target-user))))
                   (cp--reply client "Usage: delnetwork <user> <name>")))

              ;; --- Module management ---
              ((string= cmd "listmods")
               (dolist (entry (cloak.modules:list-registered-modules))
                 (let* ((name (car entry))
                        (active (cloak.modules:module-active-p name)))
                   (cp--reply client
                     (format nil "  ~a~a" name (if active " [loaded]" "")))))
               (cp--reply client
                 (format nil "~d module(s) registered."
                         (length (cloak.modules:list-registered-modules)))))

              ((string= cmd "loadmod")
               (if (first args)
                   (let ((name (first args)))
                     (if (cloak.modules:module-active-p name)
                         (cp--reply client (format nil "Module ~a already loaded." name))
                         (if (cloak.modules:load-module name bouncer)
                             (progn
                               ;; Add to enabled modules
                               (pushnew name (cloak.config:config-enabled-modules cfg)
                                        :test #'string=)
                               (cloak.config:save-config cfg)
                               (cp--reply client (format nil "Module ~a loaded." name)))
                             (cp--reply client (format nil "Unknown module: ~a" name)))))
                   (cp--reply client "Usage: loadmod <name>")))

              ((string= cmd "unloadmod")
               (if (first args)
                   (let ((name (first args)))
                     (if (cloak.modules:module-active-p name)
                         (progn
                           (cloak.modules:unload-module name bouncer)
                           (setf (cloak.config:config-enabled-modules cfg)
                                 (remove name (cloak.config:config-enabled-modules cfg)
                                         :test #'string=))
                           (cloak.config:save-config cfg)
                           (cp--reply client (format nil "Module ~a unloaded." name)))
                         (cp--reply client (format nil "Module ~a not loaded." name))))
                   (cp--reply client "Usage: unloadmod <name>")))

              (t
               (cp--reply client
                 (format nil "Unknown command: ~a (try 'help')" cmd)))))
          :halt)))))

(register-module "controlpanel" 'controlpanel-module
  :description "Admin commands via /msg *controlpanel (adduser, addnetwork, loadmod, etc.)"
  :scope :global)

;;; -------------------------------------------------------
;;; watch - Notify when specific users come online/offline
;;; -------------------------------------------------------

(defclass watch-module (module)
  ((watchlist :initform nil :accessor watch-list
              :documentation "List of nicks to watch (case-insensitive).")
   (online :initform (make-hash-table :test 'equalp) :accessor watch-online
           :documentation "Set of currently-online watched nicks."))
  (:documentation "Notifies when watched users come online or go offline.
Uses IRC QUIT/JOIN/NICK events to track presence."))

(defmethod on-load ((mod watch-module) bouncer)
  (declare (ignore bouncer))
  (let ((data (load-module-data "watch")))
    (when data
      (setf (watch-list mod) (getf data :nicks))))
  (format t "[CLoak] Module loaded: watch (~d nicks)~%"
          (length (watch-list mod))))

(defun watch--notify-clients (bouncer upstream text)
  "Send watch notification to all clients on this network."
  (let ((network (cloak.upstream:upstream-network-name upstream)))
    (bt:with-lock-held ((cloak.bouncer:bouncer-lock bouncer))
      (dolist (c (cloak.bouncer:bouncer-clients bouncer))
        (when (and (cloak.downstream:client-authenticated-p c)
                   (string-equal (cloak.downstream:client-network c) network))
          (cloak.downstream:client-send c
            (format nil ":*watch!module@CLoak NOTICE ~a :~a"
                    (or (cloak.downstream:client-nick c) "*") text)))))))

(defmethod on-upstream-message ((mod watch-module) bouncer upstream raw-line msg)
  (declare (ignore raw-line))
  (let ((command (cloak.protocol:irc-message-command msg))
        (source-nick (cloak.protocol:source-nick
                      (cloak.protocol:irc-message-source msg))))
    (when (and source-nick
               (member source-nick (watch-list mod) :test #'string-equal))
      (cond
        ;; User joined a channel — they're online
        ((string= command "JOIN")
         (unless (gethash source-nick (watch-online mod))
           (setf (gethash source-nick (watch-online mod)) t)
           (watch--notify-clients bouncer upstream
             (format nil "~a is now online" source-nick))))
        ;; User quit — they're offline
        ((string= command "QUIT")
         (when (gethash source-nick (watch-online mod))
           (remhash source-nick (watch-online mod))
           (watch--notify-clients bouncer upstream
             (format nil "~a is now offline" source-nick))))
        ;; User changed nick
        ((string= command "NICK")
         (let ((new-nick (first (cloak.protocol:irc-message-params msg))))
           (when new-nick
             (remhash source-nick (watch-online mod))
             (watch--notify-clients bouncer upstream
               (format nil "~a changed nick to ~a" source-nick new-nick))
             ;; Track new nick if it's also on watchlist
             (when (member new-nick (watch-list mod) :test #'string-equal)
               (setf (gethash new-nick (watch-online mod)) t))))))))
  nil)

;; Handle watch add/remove via /msg *watch
(defmethod on-downstream-message ((mod watch-module) bouncer client raw-line msg)
  (declare (ignore bouncer raw-line))
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (string= command "PRIVMSG")
      (let ((target (first (cloak.protocol:irc-message-params msg))))
        (when (string-equal target "*watch")
          (let* ((text (second (cloak.protocol:irc-message-params msg)))
                 (parts (and text (split-sequence:split-sequence #\Space text
                                    :remove-empty-subseqs t)))
                 (cmd (string-downcase (or (first parts) "")))
                 (nick (second parts)))
            (flet ((reply (txt)
                     (cloak.downstream:client-send client
                       (format nil ":*watch!module@CLoak NOTICE ~a :~a"
                               (or (cloak.downstream:client-nick client) "*") txt))))
              (cond
                ((string= cmd "add")
                 (if nick
                     (if (member nick (watch-list mod) :test #'string-equal)
                         (reply (format nil "~a is already on your watchlist." nick))
                         (progn
                           (push nick (watch-list mod))
                           (save-module-data "watch" (list :nicks (watch-list mod)))
                           (reply (format nil "Added ~a to watchlist." nick))))
                     (reply "Usage: add <nick>")))
                ((string= cmd "remove")
                 (if nick
                     (if (member nick (watch-list mod) :test #'string-equal)
                         (progn
                           (setf (watch-list mod)
                                 (remove nick (watch-list mod) :test #'string-equal))
                           (remhash nick (watch-online mod))
                           (save-module-data "watch" (list :nicks (watch-list mod)))
                           (reply (format nil "Removed ~a from watchlist." nick)))
                         (reply (format nil "~a is not on your watchlist." nick)))
                     (reply "Usage: remove <nick>")))
                ((string= cmd "list")
                 (if (watch-list mod)
                     (progn
                       (dolist (n (watch-list mod))
                         (reply (format nil "  ~a~a" n
                                        (if (gethash n (watch-online mod))
                                            " [online]" ""))))
                       (reply (format nil "~d nick(s) watched." (length (watch-list mod)))))
                     (reply "Watchlist is empty.")))
                (t
                 (reply "Commands: add <nick>, remove <nick>, list")))))
          :halt)))))

(register-module "watch" 'watch-module
  :description "Notifies when watched users come online/offline (via /msg *watch)"
  :scope :global)

;;; -------------------------------------------------------
;;; flooddetach - Detach from channel if flooded
;;; -------------------------------------------------------

(defclass flooddetach-module (module)
  ((threshold :initform 20 :accessor flood-threshold
              :documentation "Max messages per window before detaching.")
   (window :initform 5 :accessor flood-window
           :documentation "Time window in seconds.")
   (counters :initform (make-hash-table :test 'equal) :accessor flood-counters
             :documentation "Maps channel -> (count . window-start-time)")
   (lock :initform (bt:make-lock "flooddetach") :accessor flood-lock))
  (:documentation "Automatically parts from a channel if the message rate exceeds
a threshold, protecting clients from flood-related disconnects."))

(defmethod on-upstream-message ((mod flooddetach-module) bouncer upstream raw-line msg)
  (declare (ignore raw-line))
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (string= command "PRIVMSG")
      (let* ((target (first (cloak.protocol:irc-message-params msg)))
             (now (get-universal-time)))
        ;; Only track channel messages (channels start with # or &)
        (when (and target (plusp (length target))
                   (member (char target 0) '(#\# #\&)))
          (bt:with-lock-held ((flood-lock mod))
            (let ((record (gethash target (flood-counters mod))))
              (if (and record (< (- now (cdr record)) (flood-window mod)))
                  ;; Within window — increment
                  (let ((new-count (1+ (car record))))
                    (setf (gethash target (flood-counters mod))
                          (cons new-count (cdr record)))
                    (when (>= new-count (flood-threshold mod))
                      ;; Flood detected — part the channel
                      (remhash target (flood-counters mod))
                      (cloak.upstream:upstream-send upstream
                        (format nil "PART ~a :Flood protection triggered" target))
                      (format t "[CLoak] Flooddetach: parted ~a on ~a (~d msgs in ~ds)~%"
                              target (cloak.upstream:upstream-network-name upstream)
                              new-count (flood-window mod))
                      ;; Notify attached clients
                      (let ((network (cloak.upstream:upstream-network-name upstream)))
                        (bt:with-lock-held ((cloak.bouncer:bouncer-lock bouncer))
                          (dolist (c (cloak.bouncer:bouncer-clients bouncer))
                            (when (and (cloak.downstream:client-authenticated-p c)
                                       (string-equal (cloak.downstream:client-network c) network))
                              (cloak.downstream:client-send c
                                (format nil ":*flooddetach!module@CLoak NOTICE ~a :Parted ~a (flood protection: ~d msgs in ~ds)"
                                        (or (cloak.downstream:client-nick c) "*")
                                        target new-count (flood-window mod)))))))))
                  ;; New window
                  (setf (gethash target (flood-counters mod))
                        (cons 1 now)))))))))
  nil)

(defmethod module-settings-html ((mod flooddetach-module))
  (spinneret:with-html-string
    (:div :class "form-group"
      (:label :class "form-label" "Message Threshold")
      (:input :class "form-input" :type "number" :name "threshold"
              :value (format nil "~d" (flood-threshold mod))))
    (:div :class "form-group"
      (:label :class "form-label" "Time Window (seconds)")
      (:input :class "form-input" :type "number" :name "window"
              :value (format nil "~d" (flood-window mod))))))

(defmethod on-save-settings ((mod flooddetach-module) params)
  (let ((thresh (cdr (assoc :threshold params)))
        (win (cdr (assoc :window params))))
    (when (and thresh (plusp (length thresh)))
      (let ((n (parse-integer thresh :junk-allowed t)))
        (when (and n (> n 0))
          (setf (flood-threshold mod) n))))
    (when (and win (plusp (length win)))
      (let ((n (parse-integer win :junk-allowed t)))
        (when (and n (> n 0))
          (setf (flood-window mod) n))))))

(register-module "flooddetach" 'flooddetach-module
  :description "Parts from channels if message rate exceeds threshold (flood protection)"
  :scope :global)

;;; -------------------------------------------------------
;;; clientbuffer - Per-client playback positions
;;; -------------------------------------------------------

(defclass clientbuffer-module (module)
  ((positions :initform (make-hash-table :test 'equal) :accessor cb-positions
              :documentation "Maps client-key -> universal-time of last playback.")
   (lock :initform (bt:make-lock "clientbuffer") :accessor cb-lock))
  (:documentation "Tracks per-client playback positions so that multiple devices
each receive only messages they haven't seen. Client identity is derived from
user/network/ident (ident = IRC USER username)."))

(defun cb--client-key (client)
  "Return a stable key for CLIENT based on user/network/ident.
  The ident (from IRC USER command) distinguishes different client programs
  (e.g. clatter vs Revolution IRC) so each gets independent playback."
  (format nil "~a/~a/~a"
          (or (cloak.downstream:client-user client) "?")
          (or (cloak.downstream:client-network client) "?")
          (or (cloak.downstream:client-ident client)
              (cloak.downstream:client-nick client)
              "?")))

(defmethod on-load ((mod clientbuffer-module) bouncer)
  (declare (ignore bouncer))
  (let ((data (load-module-data "clientbuffer")))
    (when data
      (loop for (key time) on data by #'cddr
            do (setf (gethash (string key) (cb-positions mod)) time))))
  (format t "[CLoak] Module loaded: clientbuffer (~d positions tracked)~%"
          (hash-table-count (cb-positions mod))))

(defun cb--save-positions (mod)
  "Persist all position data."
  (let (plist)
    (bt:with-lock-held ((cb-lock mod))
      (maphash (lambda (key time)
                 (push time plist)
                 (push key plist))
               (cb-positions mod)))
    (save-module-data "clientbuffer" plist)))

(defmethod on-client-attach ((mod clientbuffer-module) bouncer client user-name network-name)
  (declare (ignore bouncer))
  ;; Attach hooks fire BEFORE playback, so we can set client-last-playback
  ;; to our stored position. Playback will then only send messages since that time.
  (let* ((key (format nil "~a/~a/~a"
                       user-name network-name
                       (or (cloak.downstream:client-ident client)
                           (cloak.downstream:client-nick client)
                           "?")))
         (stored-time (bt:with-lock-held ((cb-lock mod))
                        (gethash key (cb-positions mod)))))
    (when stored-time
      (setf (cloak.downstream:client-last-playback client) stored-time)
      (format t "[CLoak] Clientbuffer: restored position for ~a (last seen ~ds ago)~%"
              key (- (get-universal-time) stored-time)))))

(defmethod on-client-detach ((mod clientbuffer-module) bouncer client)
  (declare (ignore bouncer))
  ;; Save playback position with a 60s safety margin.
  ;; If the client crashed, it may not have processed recent messages.
  ;; A small overlap of duplicates on reconnect is preferable to lost messages.
  (let ((key (cb--client-key client))
        (safe-time (- (get-universal-time) 60)))
    (bt:with-lock-held ((cb-lock mod))
      (setf (gethash key (cb-positions mod)) safe-time))
    (cb--save-positions mod)))

(register-module "clientbuffer" 'clientbuffer-module
  :description "Tracks per-client playback positions for multi-device support"
  :scope :global)

;;; -------------------------------------------------------
;;; playback - Request buffer playback via /msg *playback
;;; -------------------------------------------------------

(defclass playback-module (module) ()
  (:documentation "Allows clients to request message playback by time range.
Commands via /msg *playback:
  since <duration>  - Replay messages from last N[h/m/s] (e.g. '2h', '30m')
  all               - Replay all buffered messages
  clear             - Clear playback position (next attach replays all)"))

(defun pb--reply (client text)
  "Send a NOTICE from *playback to CLIENT."
  (cloak.downstream:client-send client
    (format nil ":*playback!module@CLoak NOTICE ~a :~a"
            (or (cloak.downstream:client-nick client) "*") text)))

(defun pb--parse-duration (str)
  "Parse a duration string like '2h', '30m', '90s', '1h30m' into seconds.
Returns NIL on failure."
  (when (and str (plusp (length str)))
    (let ((total 0)
          (current 0)
          (s (string-downcase str)))
      (loop for ch across s
            do (cond
                 ((digit-char-p ch)
                  (setf current (+ (* current 10) (digit-char-p ch))))
                 ((char= ch #\h) (incf total (* current 3600)) (setf current 0))
                 ((char= ch #\m) (incf total (* current 60)) (setf current 0))
                 ((char= ch #\s) (incf total current) (setf current 0))
                 (t (return-from pb--parse-duration nil))))
      ;; Bare number with no suffix = seconds
      (incf total current)
      (when (plusp total) total))))

(defmethod on-downstream-message ((mod playback-module) bouncer client raw-line msg)
  (declare (ignore raw-line))
  (let ((command (cloak.protocol:irc-message-command msg)))
    (when (string= command "PRIVMSG")
      (let ((target (first (cloak.protocol:irc-message-params msg))))
        (when (string-equal target "*playback")
          (let* ((text (second (cloak.protocol:irc-message-params msg)))
                 (parts (and text (split-sequence:split-sequence #\Space text
                                    :remove-empty-subseqs t)))
                 (cmd (string-downcase (or (first parts) "")))
                 (arg (second parts))
                 (user-name (cloak.downstream:client-user client))
                 (network (cloak.downstream:client-network client)))
            (cond
              ;; --- since <duration> ---
              ((string= cmd "since")
               (let ((secs (pb--parse-duration arg)))
                 (if secs
                     (let ((since (- (get-universal-time) secs))
                           (prefix (format nil "~a/~a/" user-name network))
                           (total 0))
                       (pb--reply client
                         (format nil "Replaying messages from last ~a..." arg))
                       (maphash (lambda (key buffer)
                                  (when (alexandria:starts-with-subseq prefix key)
                                    (dolist (m (cloak.buffer:buffer-messages-since buffer since))
                                      (incf total)
                                      (cloak.downstream:client-send client
                                        (cloak.buffer:stored-message-raw m)))))
                                (cloak.bouncer:bouncer-buffers bouncer))
                       (pb--reply client
                         (format nil "Playback complete: ~d message~:p." total)))
                     (pb--reply client
                       "Invalid duration. Use e.g. '2h', '30m', '1h30m', '90s'."))))

              ;; --- all ---
              ((string= cmd "all")
               (let ((prefix (format nil "~a/~a/" user-name network))
                     (total 0))
                 (pb--reply client "Replaying all buffered messages...")
                 (maphash (lambda (key buffer)
                            (when (alexandria:starts-with-subseq prefix key)
                              (dolist (m (cloak.buffer:buffer-messages-all buffer))
                                (incf total)
                                (cloak.downstream:client-send client
                                  (cloak.buffer:stored-message-raw m)))))
                          (cloak.bouncer:bouncer-buffers bouncer))
                 (pb--reply client
                   (format nil "Playback complete: ~d message~:p." total))))

              ;; --- clear ---
              ((string= cmd "clear")
               (setf (cloak.downstream:client-last-playback client) 0)
               ;; Also clear clientbuffer position if that module is active
               (let ((cb (cloak.modules:active-module "clientbuffer")))
                 (when cb
                   (let ((key (format nil "~a/~a/~a"
                                       user-name network
                                       (or (cloak.downstream:client-ident client)
                                           (cloak.downstream:client-nick client)
                                           "?"))))
                     (remhash key (cb-positions cb)))))
               (pb--reply client "Playback position cleared. Next attach will replay all."))

              ;; --- help / unknown ---
              (t
               (pb--reply client "Usage: /msg *playback <command>")
               (pb--reply client "  since <duration>  - Replay from last N (e.g. 2h, 30m, 1h30m)")
               (pb--reply client "  all               - Replay all buffered messages")
               (pb--reply client "  clear             - Reset playback position"))))
          :halt)))))

(register-module "playback" 'playback-module
  :description "Request buffer playback by time via /msg *playback (since 2h, all, clear)"
  :scope :global)
