#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.test)

(define-test redact-all
  :parent luna-test
  (with-fixtures '(luna.framework::*debug-execution*)
    (setf luna.framework::*debug-execution* t)
    (cl-matrix:with-account (*luna-user*)
      (multiple-value-bind (rooms listener) (create-group-and-start-listening "redact-test" 3 :sync-rate 5)

        (cl-matrix:with-account (*normal-user*)
          (cl-matrix:room-join (cadr rooms))
          (dotimes (i 10)
            (cl-matrix:msg-send "spam!!!!" (cadr rooms))))
        (let ((before-token (cl-matrix:now-token)))
          (send-command (car rooms) (format nil "!luna redact redact-test ~a" (cl-matrix:username *normal-user*)))
          (true (wait-until (car rooms) #'replyp :sync-token before-token :timeout 120 :sleep-time 10)))

        (let ((target (cadr rooms)))
          (false (remove-if-not (lambda (e)
                                  (string= (jsown:val e "sender") (cl-matrix:username *normal-user*)))
                                (cl-matrix:room-messages target (cl-matrix:now-token) "b"))
                 "should be no events from the normal user in the room."))

        (bt:destroy-thread listener)))))
  
