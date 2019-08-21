#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.test)

(define-test soft-ban
  :parent luna-test
  (with-fixtures '(luna.framework::*debug-execution*)
    (setf luna.framework::*debug-execution* t)
    (cl-matrix:with-account (*luna-user*)
      (let ((group-name "soft-ban-test"))
        (multiple-value-bind (rooms listener) (create-group-and-start-listening group-name 3)
          (let ((already-joined (cadr rooms))
                (not-joined-yet (caddr rooms)))
            (cl-matrix:with-account (*normal-user*)
              (cl-matrix:room-join already-joined))
            (let ((before-token (cl-matrix:now-token)))
              (send-command (car rooms) (format nil  "!luna ban ~a ~a" group-name (cl-matrix:username *normal-user*)))
              (true (wait-until (car rooms) #'replyp :sync-token before-token)))

            (sleep 0.1)
            (true (string= "ban" (let ((membership (cl-matrix:room-state already-joined "m.room.member"
                                                                         (cl-matrix:username *normal-user*))))
                                   (and (jsown:keyp membership "membership")
                                        (jsown:val membership "membership"))))

                  "user was not banned from room they were already in.")
            (dolist (target (cddr rooms))
              (true (null (string= "ban"
                                   (let ((membership (cl-matrix:room-state target "m.room.member"
                                                                           (cl-matrix:username *normal-user*))))
                                     (and (jsown:keyp membership "membership")
                                          (jsown:val membership "membership"))))))

              (true (not (null (cl-matrix:room-state target luna:*luna.soft-ban*
                                                     (subseq (cl-matrix:username *normal-user*) 1))))
                    "luna.soft_ban state event not present. "))


            (let ((count 0)
                  (before-token (cl-matrix:now-token)))
              (cl-matrix:with-account (*normal-user*)
                (cl-matrix:room-join not-joined-yet))

              (true (wait-until not-joined-yet (lambda (e)
                                                 (and (or (and (string= "m.room.member" (jsown:val e "type"))
                                                               (incf count))

                                                          (and (string= luna:*luna.soft-ban* (jsown:val e "type"))
                                                               (incf count)))
                                                      (= count 3)))
                                :sync-token before-token)
                    "the normal user was not banned when joining a room they were soft banned from.")

              (true (wait-until (car rooms) (lambda (e)
                                              (string= "m.room.message" (jsown:val e "type")))
                                :sync-token before-token)
                    "the report has not been sent to the control."))

            (false (jsown:val
                    (cl-matrix:room-state not-joined-yet luna:*luna.soft-ban*
                                          (subseq (cl-matrix:username *normal-user*) 1)) "activep")
                   "the soft-ban is still active."))
          (bt:destroy-thread listener))))))
