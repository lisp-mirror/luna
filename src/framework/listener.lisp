#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

(defun sync-listener (seconds sync-token)
  (cl-async:with-delay (seconds)
    (multiple-value-bind (sync-data next-token) (cl-matrix:account-sync :since sync-token)
      (v:info :listener "sync got ~a rooms for token: ~a " (length (jsown:keywords
                                                                    (jsown:filter sync-data "rooms" "join")))
              sync-token)
      (cl-matrix.base-events:issue-sync-event sync-data)
      (sync-listener seconds next-token))))

;; get sync token from config when it first starts up
(defun make-listener (sync-token sync-rate)
  (let ((sync-token sync-token))
    (v:info :listener "starting listener ~a" sync-token)
    (lambda ()
      (cl-async:with-event-loop ()
        (sync-listener sync-rate sync-token)))))

(defun start-listening (&key (sync-token (cl-matrix:now-token)) (sync-rate 0.2))
  (bt:make-thread (make-listener sync-token sync-rate)
                  :initial-bindings `((*standard-output* . ,*standard-output*)
                                      (cl-matrix:*account* . ,cl-matrix:*account*))))
