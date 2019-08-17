#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.test)
(defun send-command (room-id command)
  (cl-matrix:msg-send command room-id))

(defparameter *listeners* nil)

(defun create-group-and-start-listening (group-name nrooms &key (sync-rate 0.2))
  "(values rooms listener)
rooms is a list of rooms, the car being the control an the cdr being the targets. "
  (let ((rooms (create-rooms nrooms)))
    (let ((listener (luna.framework:start-listening :sync-rate sync-rate)))
      (push listener *listeners*) ; clean up later.
      (let ((before-token (cl-matrix:now-token)))
        (send-command (car rooms) (format nil "!luna add-to-group ~a ~{~a ~}" group-name (cdr rooms)))
        (wait-until (car rooms) #'replyp :sync-token before-token))

      (values rooms listener))))

(defun create-rooms (n)
  (loop :for i :from 0 :to n :collect
       (cl-matrix:room-create)))

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

(defun replyp (e)
   (let ((e (jsown:val e "content")))
     (jsown:keyp e "m.relates_to")))

(defun target-equal (targets1 targets2)
  (let ((equal? t))
    (loop :while equal? :for r :in targets1 :do
         (setf equal? (member r targets2 :test #'string=)))
    (and equal? (= (length targets1) (length targets2)))))

(define-test create-group

  :parent luna-test
  ;; this tests normal operation of the command, we also need tests for unexpected/malicious things.
  (with-fixtures '(luna.framework::*debug-execution*)
    (setf luna.framework::*debug-execution* t)
    (cl-matrix:with-account (*luna-user*)
      (let ((control-room (cl-matrix:room-create))
            (targets (create-rooms 3))
            (test-group "test-group")
            (begin-test-token (cl-matrix:now-token)))
        (let ((listener (luna.framework::start-listening :sync-token begin-test-token)))
          (push listener *listeners*) ; clean up later.
          (send-command control-room (format nil "!luna add-to-group ~a ~{~a ~}" test-group targets))
          (true (wait-until control-room (lambda (e)
                                           (let ((e (jsown:val e "content")))
                                             (jsown:keyp e "m.relates_to")))
                            :sync-token begin-test-token
                            :timeout 100)
                "timed out waiting for luna to reply to group command")

          ;; assert that the group has been created properly
          (let ((group-event (cl-matrix:room-state control-room luna:*luna.group* test-group)))
            (is target-equal targets (jsown:val group-event "target_rooms")))

          (dolist (target targets)
            (is string= control-room (jsown:val (cl-matrix:room-state target luna:*luna.group* test-group) "control_room")
                "the control room is not set for each of the targets"))

          (bt:destroy-thread listener)))))

  (define-test create-group-underpowerd

    (cl-matrix:with-account (*luna-user*)
      (let ((control-room (cl-matrix:room-create))
            (legit-target (cl-matrix:room-create))
            (bad-target   (cl-matrix:room-create))
            (begin-test-token (cl-matrix:now-token)))

        (dolist (room (list control-room legit-target))
          (cl-matrix:change-power-level room (cl-matrix:username *mod-user*) 100))

        (let ((listener (luna.framework::start-listening :sync-token begin-test-token)))
          (push listener *listeners*) ; clean up later.
          (cl-matrix:with-account (*mod-user*)
            (cl-matrix:room-join control-room legit-target bad-target)
            (send-command control-room (format nil "!luna add-to-group ~a ~{~a ~}" "test-group"
                                               (list legit-target bad-target))))
          (true (wait-until control-room (lambda (e)
                                           (let ((e (jsown:val e "content")))
                                             (jsown:keyp e "m.relates_to")))
                            :sync-token begin-test-token
                            :timeout 100)
                "timed out waiting for luna to reply to group command")

          ;; assert that the bad-target has not been added to the group by the bot
          (let ((group-event (cl-matrix:room-state bad-target luna:*luna.group* "test-group")))
            (false group-event))

          (bt:destroy-thread listener)))))

  (define-test add-to-group

    (with-fixtures '(luna.framework::*debug-execution*)
    (setf luna.framework::*debug-execution* t)
    (cl-matrix:with-account (*luna-user*)
      (multiple-value-bind (rooms listener) (create-group-and-start-listening "add-test" 2 :sync-rate 0.2)

        (let ((new-room (cl-matrix:room-create))
              (control (car rooms))
              (before-token (cl-matrix:now-token)))
            
           (send-command control (format nil "!luna add-to-group ~a ~{~a ~}" "add-test"
                                    (list new-room new-room)))
           (true (wait-until control #'replyp :sync-token before-token :timeout 120 :sleep-time 10))

           (let ((group-event (cl-matrix:room-state control luna:*luna.group* "add-test")))
             (true (member new-room (jsown:val group-event "target_rooms") :test #'string=))
             (true (member (cadr rooms) (jsown:val group-event "target_rooms") :test #'string=))
             (is = 1 (length (remove-if-not (lambda (r) (string= r new-room))
                                            (jsown:val group-event "target_rooms")))
                 "the new room has been added twice instead of once.")))
        
        (bt:destroy-thread listener))))))

(defun clean ()
  (dolist (listener *listeners*)
    (handler-case (bt:destroy-thread listener)
      (error (c) (declare (ignore c)))
      (:no-error (c) (declare (ignore c)))))
  (setf *listeners* nil)
  
  (dolist (account (list *luna-user* *mod-user* *normal-user*))
    (cl-matrix:with-account (account)
      (dolist (room-id (cl-matrix:user-joined-rooms))
        (cl-matrix:room-leave room-id)
        (cl-matrix:room-forget room-id)))))


