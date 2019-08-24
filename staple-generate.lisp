(in-package :cl-user)

(ql:quickload :luna)
(ql:quickload :staple-markdown)
(ql:quickload :staple-markless)

(loop :while (null (find-package "STAPLE")) :do
     (sleep 1))

(staple:generate :luna
                 :subsystems (list (list (asdf:find-system "luna.framework") :packages (list :luna.framework :luna.framework.hooks)))
                 :packages (list :luna :luna.hooks)
                 :if-exists :supersede
                 :images (list (asdf:system-relative-pathname :luna "doc/luna-red.svg"))
                 :output-directory (asdf:system-relative-pathname :luna "doc/"))
