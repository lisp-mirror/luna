(asdf:defsystem "luna.framework"
  :version "0.0"
  :author "Gnuxie <Gnuxie@protonmail.com>"
  :license "AGPL v3+"
  :depends-on ("cl-matrix" "verbose" "cl-strings" "lparallel")
  :components ((:file "package")
               (:file "conditions")
               (:file "room-ops")
               (:file "command-parser")
               (:file "report")
               (:file "step")
               (:file "listener")))
