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

        (cl-matrix:with-account (*mod-user*)
          (cl-matrix:room-join (cadr rooms) (caddr rooms))
          (cl-matrix:msg-send "not-spam" (cadr rooms))
          (cl-matrix:msg-send "not-spam" (caddr rooms)))

        (let ((before-token (cl-matrix:now-token)))
          (send-command (car rooms) (format nil "!luna redact redact-test ~a" (cl-matrix:username *normal-user*)))
          (true (wait-until (car rooms) #'replyp :sync-token before-token :timeout 120 :sleep-time 10)))

        (let* ((target (cadr rooms))
              (messages (cl-matrix:room-messages target (cl-matrix:now-token) "b" :limit "20")))
          (is = 0 (length (remove-if-not (lambda (e)
                                           (and (string= (jsown:val e "sender") (cl-matrix:username *normal-user*))
                                                (jsown:keywords (jsown:val e "content" #| ensure there is no content |#))))
                                         messages))

                 "should be no events from the normal user in the room.")

          (true  (find-if (lambda (e)
                            (and (string= (jsown:val e "sender") (cl-matrix:username *mod-user*))
                                 (string= (jsown:filter e "content" "body") "not-spam")))
                          messages)
                 "should be other peoples messages here")

          (true (null
                 (remove-if-not (lambda (e)
                                  (and (string= "m.room.redact" (jsown:val e "type"))))
                                (cl-matrix:room-messages (caddr rooms) (cl-matrix:Now-token) "b" :limit "20")))
                "messages were removed from other rooms."))

        (bt:destroy-thread listener)))))
  
