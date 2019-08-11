#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.test)

(define-test help
  :parent luna-test


  (define-test help-parser

    (let ((result (funcall (luna.framework::get-parser 'help) "help" "" "!foo:foo.net" "23904")))
      (true (not (null (cl-ppcre:scan "displays" result))))
      (true (not (null (cl-ppcre:scan "COMMAND" result))))
      (true (null (cl-ppcre:scan "Will" result))))

    (let ((result (funcall (luna.framework::get-parser 'help) "help" "help" "!foo:foo.net" "flkdfjl")))
      (true (cl-ppcre:scan "Will" result))))

  
  (with-fixtures '(luna.framework::*debug-execution*)
    (setf luna.framework::*debug-execution* t)
    (cl-matrix:with-account (*luna-user*)
      (multiple-value-bind (rooms listener) (create-group-and-start-listening "help-test" 1 :sync-rate 0.1)

        (cl-matrix:with-account (*mod-user*)
          (cl-matrix:room-join (car rooms))
          (cl-matrix:msg-send "!luna foo" (car rooms))
          (cl-matrix:msg-send "!luna " (car rooms))
          (cl-matrix:msg-send "!luna help" (car rooms))
          (cl-matrix:msg-send "!luna" (car rooms)))

        (let ((before-token (cl-matrix:now-token)))
          (true (wait-until (car rooms) #'replyp :sync-token before-token :timeout 120 :sleep-time 10)))

        (sleep 0.2)
        (let ((messages (cl-matrix:room-messages (car rooms) (cl-matrix:now-token) "b" :limit "30")))
          (is = 4 (length (remove-if-not (lambda (e)
                                           (and (string= (jsown:val e "sender") (cl-matrix:username *luna-user*))
                                                (string= (jsown:val e "type") "m.room.message")))
                                         messages))

                 "should be 3 messages, 1 for foo, 1 for help and 2 for the create group."))
        (bt:destroy-thread listener)))))
