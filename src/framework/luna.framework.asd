(asdf:defsystem "luna.framework"
  :version "0.0"
  :author "Gnuxie <Gnuxie@protonmail.com>"
  :license "NON-VIOLENT PUBLIC LICENSE v1"
  :homepage "https://gnuxie.gitlab.io/luna/"
  :depends-on ("cl-matrix" "verbose" "cl-strings" "lparallel" "parse-float" "unix-opts")
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "room-ops")
               (:file "command-parser")
               (:file "step")
               (:file "listener")
               (:file "report")
               (:file "hooks"))
  :description "framework used to build luna")
