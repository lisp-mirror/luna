#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

;; get sync token from config when it first starts up
(defun make-listener (&optional (sync-token (cl-matrix:now-token)))
  (let ((sync-token sync-token))
    (lambda ()
      (v:info :listener "starting listener ~a" sync-token)
      (loop :do
           ;; we need to be able to give this sync-token to something if we die.
           (multiple-value-bind (sync-data next-token) (cl-matrix:account-sync :since sync-token)
             (v:info :listener "sync got ~a rooms for token: ~a " (length (jsown:keywords
                                                                           (jsown:filter sync-data "rooms" "join")))
                     sync-token)
             (cl-matrix.base-events:issue-sync-event sync-data)
             (setf sync-token next-token))
           (sleep 5)
))))

(defun start-listening (&optional (sync-token (cl-matrix:now-token)))
  (let ((*standard-output* *standard-output*)
        (cl-matrix:*account* cl-matrix:*account*))
    (bt:make-thread (make-listener sync-token))))
