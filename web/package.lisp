;;;; web/package.lisp - Web admin interface packages for CLoak

(defpackage #:cloak.web
  (:use #:cl)
  (:local-nicknames (#:fx #:fluxion)
                    (#:comp #:fluxion.components)
                    (#:cells #:fluxion.cells)
                    (#:events #:fluxion.events)
                    (#:server #:fluxion.server)
                    (#:render #:fluxion.render)
                    (#:valid #:fluxion.validation)
                    (#:config #:cloak.config)
                    (#:bouncer #:cloak.bouncer)
                    (#:upstream #:cloak.upstream)
                    (#:downstream #:cloak.downstream)
                    (#:modules #:cloak.modules)
                    (#:bt #:bordeaux-threads)
                    (#:alex #:alexandria))
  (:export
   #:start-web-admin
   #:stop-web-admin))
