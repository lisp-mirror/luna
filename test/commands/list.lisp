#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.test)

(define-test list-group
  :parent luna-test

  (with-fixtures '(luna.framework::*debug-execution*)
    (setf luna.framework::*debug-execution* t)
    (cl-matrix:with-account (*luna-user*)
      (multiple-value-bind (rooms listener)
          (create-group-and-start-listening "list-test" 3 :sync-rate 0.1)

        (let ((before-token (cl-matrix:now-token)))
          (cl-matrix:with-account (*mod-user*)
            (cl-matrix:room-join (car rooms))
            (cl-matrix:msg-send "!luna list list-test" (car rooms)))
          (true (wait-until (car rooms) #'replyp
                            :sync-token before-token :timeout 120 :sleep-time 2))

          (sleep 0.2)
          (let ((messages (cl-matrix:room-messages (car rooms)
                                                   (cl-matrix:now-token) "b"
                                                   :limit "30" :to before-token)))
            (is = 1
                (length
                 (remove-if-not (lambda (e)
                                  (and (string= (jsown:val e "sender")
                                                (cl-matrix:username *luna-user*))
                                       (string= (jsown:val e "type") "m.room.message")))
                                messages))
                "should be 1 messages, 1 for list")))
        (bt:destroy-thread listener)))))
