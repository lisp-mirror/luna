(asdf:defsystem "luna.framework"
  :version "0.0"
  :author "Gnuxie <Gnuxie@protonmail.com>"
  :license "AGPL v3+"
  :depends-on ("cl-matrix" "verbose" "cl-strings")
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "room-ops")
               (:file "single-step")
               (:file "room-step")
               (:file "value-step")
               (:file "plan")
               (:file "command-parser")
               (:file "command-step")
               (:file "listener")))
