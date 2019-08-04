#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.test)

(define-test group-ban

  (cl-matrix:with-account (*luna-user*)
    (multiple-value-bind (rooms listener) (create-group-and-start-listening "ban-test" 3)

      (let ((before-token (cl-matrix:now-token)))
        (send-command (car rooms) "!luna ban ban-test @meow:localhost")
        (true (wait-until (car rooms) #'replyp :sync-token before-token)))

      (dolist (target (cdr rooms))
        (is string= "ban"
            (jsown:filter (cl-matrix:room-state target "m.room.member" "@meow:localhost") "membership")))

      (bt:destroy-thread listener))))
