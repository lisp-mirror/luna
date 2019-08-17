(asdf:defsystem "luna.test"
  :version "0.0"
  :author "Gnuxie <Gnuxie@protonmail.com>"
  :license "NON-VIOLENT PUBLIC LICENSE v1"
  :depends-on ("luna" "parachute")
  :components ((:file "package")
               (:module "config" :components ((:file "config")))
               (:file "test-all")
               (:file "create-group")
               (:module "commands"
                        :components
                        ((:file "group-ban")
                         (:file "redact-all")
                         (:file "soft-ban")
                         (:file "help")))))
