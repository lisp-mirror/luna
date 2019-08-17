(in-package :cl-user)

(ql:quickload :luna)
(ql:quickload :staple-markdown)

(loop :while (null (find-package "STAPLE")) :do
     (sleep 1))

;;; grrr, something is wrong...
(staple:generate :luna :packages '(:luna.framework :luna)
                 :if-exists :supersede
                 :documents (list (asdf:system-relative-pathname :luna "README.md"))
                 :output-directory (asdf:system-relative-pathname :luna "doc/"))
