(in-package :cl-user)

(ql:quickload :luna)
(ql:quickload :staple-markdown)
(ql:quickload :staple-markless)

(loop :while (null (find-package "STAPLE")) :do
     (sleep 1))

(staple:generate :luna
                 :subsystems (list (asdf:find-system "luna.framework"))
                 :if-exists :supersede
                 :images (list (asdf:system-relative-pathname :luna "doc/luna-red.svg"))
                 :documents (list (asdf:system-relative-pathname :luna "README.md"))
                 :output-directory (asdf:system-relative-pathname :luna "doc/"))
