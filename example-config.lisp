(in-package :cl-user)

(ql:quickload :luna)

(v:define-pipe () (v:file-faucet :file (asdf:system-relative-pathname :luna "luna-log.txt")))

(let ((account (cl-matrix:make-account "@luna:your-home" "your-access-token")))
  (cl-matrix:with-account (account)
    (luna:make-luna-kernal)
    (luna:start-listening :sync-rate 2)))
