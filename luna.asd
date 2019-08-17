(asdf:defsystem "luna"
  :version "0.0"
  :author "Gnuxie <Gnuxie@protonmail.com>"
  :licence "AGPL v3+"
  :depends-on ("luna.framework")
  :serial t
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "group-management")
                         (:module "commands"
                                  :components
                                  ((:file "create-group")
                                   (:file "group-ban")
                                   (:file "redact-all")
                                   (:file "help")
                                   (:file "list")
                                   (:file "soft-ban")))
                         (:file "hooks")

))))
