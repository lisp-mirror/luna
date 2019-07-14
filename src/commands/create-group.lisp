#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna)

(define-target-step add-room-to-group (target-room control-room group-name sender-id) ()
  (unless (can-send-state-p target-room (cl-matrix:username cl-matrix:*account*) "luna.group")
    (error 'luna-permission-error :description (format nil "~a doesn't have permission in ~a to start a group"
                                                       (cl-matrix:username cl-matrix:*account*) target-room)))
  (unless (can-send-state-p target-room sender-id "luna.group")
    (error 'luna-permission-error :description (format nil "~a doesn't permission to add ~a to the group ~a"
                                                       sender-id target-room control-room)))

  (add-control-to-target group-name control-room target-room))

(define-step-reporter add-room-to-group (stream target-room control-room group-name sender)
  (declare (ignore sender))
  (format stream "~&add ~a to the group ~a with control room ~a" target-room group-name control-room))

(define-plan-builder group-builder (control group sender &rest targets)
  (unless (can-send-state-p control sender +state-type+)
    (error 'luna-permission-error :description (format nil "~a doesn't have permission to send ~a events"
                                                       sender +state-type+)))

  ;; what if you wanted to add the targets to control after it's been confirmed that the
  ;; add-control-to-target/s have worked and remove the ones that havn't form the list :thinking:
  ;; although, this method is quite safe due to the mapgroup also (ie mapgroup won't work if the apply
  ;; form here hasn't worked.
  (apply #'add-targets-to-control group control targets)
  (mapgroup (lambda (r)
              (create-job (get-step 'add-room-to-group) r control group sender))
            control group))

(define-step-reporter group-builder (stream control group sender &rest targets)
  (declare (ignore sender))
  (format stream "~&plan to add~%~a~%~%to ~a as group ~a"
          targets control group))

;;; !luna add-to-group cheesewheel &rest
(define-command-parser add-to-group (name rest room-id event)
  (declare (ignore name))
  (cl-ppcre:register-groups-bind (group-name targets)
      ("^(\\S+)\\s+(\\S+)\\s+(.+)" rest)
    (when (and group-name targets)
      (let ((targets (cl-strings:split targets)))
        (apply #'create-job (get-step 'group-builder)
               room-id group-name (jsown:val event "sender") targets)))))
