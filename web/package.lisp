;;;; web/package.lisp - Web admin interface packages for CLoak

(defpackage #:cloak.web
  (:use #:cl)
  (:local-nicknames (#:fx #:fluxion)
                    (#:server #:fluxion.server)
                    (#:bt #:bordeaux-threads))
  (:export
   #:start-web-admin
   #:stop-web-admin))
