#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.test)
(defparameter *listeners* nil "listeners to clean")
(defun send-command (room-id command)
  (cl-matrix:msg-send command room-id))

(defun create-rooms (n)
  (loop :for i :from 0 :to n :collect
       (cl-matrix:room-create )))

(defun wait-until (room-id predicate &key (sync-token (cl-matrix:now-token)) (sleep-time 1) (timeout 20))
  "wait until an event that matches the predicate has been found in the room.
will return t if the predicate was matched, nil if there was a timeout. "
  (let ((generator (cl-matrix:history-generator room-id :start-token sync-token))
        (found? nil)
        (sleep-count 0))
    (loop :until (or found? (> sleep-count timeout)) :do
         (let ((chunk (funcall generator)))
           (setf found? (find-if predicate chunk))
           (sleep sleep-time)
           (incf sleep-count sleep-time)))
    found?))

;;; we need a test that tests the *debug-execution* parameter.
(define-test create-group

  (with-fixtures '(luna.framework:*debug-execution*)
    (setf luna.framework:*debug-execution* t)
    (let ((control-room (cl-matrix:room-create))
          (targets (create-rooms 3))
          (test-group "test-group")
          (begin-test-token (cl-matrix:now-token)))
      (let ((listener (luna.framework::start-listening begin-test-token)))
        (push listener *listeners*) ; clean up later.
        (send-command control-room (format nil "!luna add-to-group ~a ~{~a ~}" test-group targets))
        (true (wait-until control-room (lambda (e)
                                         (let ((e (jsown:val e "content")))
                                           (when (jsown:keyp e "m.relates_to")
                                             (print (jsown:val e "body") *standard-output*))))
                          :sync-token begin-test-token
                          :timeout 100)
              "timed out waiting for luna to reply to group command")

        ;; assert that the group has been created properly
        (let ((group-event (cl-matrix:room-state control-room luna::+state-type+ test-group)))
          (is equal targets (jsown:val group-event "target_rooms")))

        (dolist (target targets)
          (is string= control-room (jsown:val (cl-matrix:room-state target luna::+state-type+ test-group) "control_room")
              "the control room is not set for each of the targets"))))))

(defun clean ()
  (dolist (listener *listeners*)
    (handler-case (bt:destroy-thread listener)
      (error (c) (print c *standard-output*))
      (:no-error (c) (declare (ignore c)))))
  (setf *listeners* nil)
    
  (dolist (room-id (cl-matrix:user-joined-rooms))
    (cl-matrix:room-leave room-id)
    (cl-matrix:room-forget room-id)))


