;;;; run-test.lisp - Quick test: connect CLoak upstream to Libera
;;;; Usage: sbcl --load run-test.lisp
;;;;
;;;; Set CLOAK_PASSWORD env var or edit the password below.

(push #P"/home/glenn/SourceCode/cloak/" asdf:*central-registry*)
(ql:quickload "cloak" :silent t)

;; Configure a single network for testing
(setf cloak.config:*config*
      (make-instance 'cloak.config:bouncer-config
        :users (list
                (make-instance 'cloak.config:user-config
                  :name "glenn"
                  :password-hash "test"
                  :admin-p t
                  :networks (list
                             (make-instance 'cloak.config:network-config
                               :name "libera"
                               :server "irc.libera.chat"
                               :port 6697
                               :tls t
                               :nick "cloak-test"
                               :autojoin '("#cloak-test")))))))

;; Create upstream directly for testing (bypass full bouncer start)
(let* ((user (first (cloak.config:config-users cloak.config:*config*)))
       (net-cfg (first (cloak.config:user-networks user)))
       (upstream (cloak.upstream:make-upstream net-cfg
                   :message-handler
                   (lambda (upstream raw-line msg)
                     (declare (ignore upstream msg))
                     (format t "<< ~a~%" raw-line)))))
  (format t "~%[CLoak Test] Connecting to Libera...~%")
  (if (cloak.upstream:upstream-connect upstream)
      (progn
        (format t "[CLoak Test] Connected! Watching for 30 seconds...~%")
        (format t "[CLoak Test] Press Ctrl+C to stop early.~%~%")
        (handler-case
            (loop for i from 1 to 30
                  do (sleep 1)
                     (when (= (mod i 10) 0)
                       (format t "~%[CLoak Test] ~d seconds elapsed, state: ~a~%"
                               i (cloak.upstream:upstream-connected-p upstream))))
          (#+sbcl sb-sys:interactive-interrupt
           #-sbcl condition ()
           (format t "~%[CLoak Test] Interrupted.~%")))
        (format t "~%[CLoak Test] Final state: connected=~a, channels=~a, caps=~a~%"
                (cloak.upstream:upstream-connected-p upstream)
                (let (chans)
                  (maphash (lambda (k v) (declare (ignore v)) (push k chans))
                           (cloak.upstream:upstream-channels upstream))
                  chans)
                (cloak.upstream:upstream-cap-enabled upstream))
        (cloak.upstream:upstream-disconnect upstream))
      (format t "[CLoak Test] Connection failed.~%")))

(format t "[CLoak Test] Done.~%")
(uiop:quit)
