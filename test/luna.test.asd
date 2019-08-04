(asdf:defsystem "luna.test"
  :version "0.0"
  :author "Gnuxie <Gnuxie@protonmail.com>"
  :license "AGPL v3+"
  :depends-on ("luna" "parachute")
  :components ((:file "package")
               (:module "config" :components ((:file "config")))
               (:file "test-all")
               (:file "create-group")
               (:module "commands"
                        :components
                        ((:file "group-ban")
                         (:file "redact-all")))))
