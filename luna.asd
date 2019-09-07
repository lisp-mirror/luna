(asdf:defsystem "luna"
  :version "0.0"
  :author "Gnuxie <Gnuxie@protonmail.com>"
  :licence "NON-VIOLENT PUBLIC LICENSE v1+"
  :homepage "https://gnuxie.gitlab.io/luna/"
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
                                   (:file "list")
                                   (:file "soft-ban")))
                         (:file "hooks"))))
  :description "extensible group management bot for matrix (matrix.org) to aid moderation of rooms and spam removal.")
