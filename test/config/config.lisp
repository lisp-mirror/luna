;;; wait why do i need a config file? ever heard of load-file? smh.
(in-package :luna.test)

(defvar *luna-user* nil)
(defvar *mod-user* nil)
(defvar *normal-user* nil)

(defun prepare-synapse ()
  (uiop:run-program (format nil "/bin/sh ~a" (asdf:system-relative-pathname :luna.test "config/local-synapse.sh")))

  (setf *luna-user* (cl-matrix:login "@luna:localhost" "lunapass" :homeserver "localhost:8008" :scheme "http://"))
  (setf *mod-user* (cl-matrix:login "@coolmod:localhost" "coolmodpass" :homeserver "localhost:8008" :scheme "http://"))
  (setf *normal-user* (cl-matrix:login "@user:localhost" "pass" :homeserver "localhost:8008" :scheme "http://")))
