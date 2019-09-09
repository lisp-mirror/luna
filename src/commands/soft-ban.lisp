#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna)
(defvar *luna.soft-ban* "luna.soft_ban")

(declaim (inline user->state-key))
(defun user->state-key (user)
  (declare (type string user))
  (subseq user 1))

(defun soft-ban (room-id user-id &key reason report-to command)
  (flet ((safe-kv (k v) (and v `(,k . ,v))))
    (cl-matrix.api.client:put-rooms/roomid/state/eventtype/statekey
     cl-matrix:*account* room-id *luna.soft-ban* (user->state-key user-id)
     (jsown:to-json (remove-if #'null `(:obj ("activep" . t) ,(safe-kv "reason" reason)
                                             ,(safe-kv "report_to" report-to)
                                             ,(safe-kv "command" command)))))))

(define-step check-soft-ban (room-id user-id)
  "Check if the user has a soft-ban event over their name and if so, ban them.
If no condition occurs, will return T if the user was soft-banned and nil otherwise.

See define-step"
  (let ((luna.soft-ban (cl-matrix:room-state room-id *luna.soft-ban* (user->state-key user-id))))
    (when (and (not (null luna.soft-ban))
               (jsown:keyp luna.soft-ban "activep") (jsown:val luna.soft-ban "activep"))
      ;; ban them and mark as inactive
      (let* ((reason (and (jsown:keyp luna.soft-ban "reason") (jsown:val luna.soft-ban "reason")))
             (reason (typecase reason (string reason) (otherwise nil))))

        ;; first ban them.
        (cl-matrix.api.client:post-rooms/roomid/ban
         cl-matrix:*account* room-id
         (jsown:to-json (remove-if #'null `(:obj ("user_id" . ,user-id) ,(and reason `("reason" . reason))))))

        ;; then if that is done with, update the soft-ban.
        (cl-matrix.api.client:put-rooms/roomid/state/eventtype/statekey
         cl-matrix:*account* room-id *luna.soft-ban* (user->state-key user-id)
         (jsown:to-json (jsown:extend-js luna.soft-ban ("activep" :false))))
        (step-result room-id)))))

(define-reporter check-soft-ban (room-id target-user result)
  (unless (null result)  ; there was no action necessary, no reason to report this.
    (flet ((send-report? (group-name room-id) ; we need to know if this room actually belongs to the group it says it does.
               (let* ((luna.group (get-group room-id group-name))
                      (control-room (and (jsown:keyp luna.group "control_room") (jsown:val luna.group "control_room"))))
                 (and control-room (target-present-in-control-p group-name control-room room-id)
                      control-room)))) ; we also want to return the control as the value.
      
      (let* ((luna.soft_ban (cl-matrix:room-state room-id luna:*luna.soft-ban* (user->state-key target-user)))
             (group-name (and (jsown:keyp luna.soft_ban "report_to") (jsown:val luna.soft_ban "report_to")))
             (command (and (jsown:keyp luna.soft_ban "command") (jsown:val luna.soft_ban "command")))
             (control (send-report? group-name room-id)))

        (if control
            (cond ((step-condition result)
                   (with-stream-to-report (s control command)
                     (write-string (room-preview room-id) s)
                     (format-indent 4 "~%Failed to ban ~a after they joined." target-user)
                     (format-indent 4 "~%~a" (cdr result))))

                  (t (with-stream-to-report (s control command)
                       (write-string (room-preview room-id) s)
                       (format s "~%Banned ~a after they joined." target-user))))
            (when (jsown:keyp luna.soft_ban "report_to")
              (v:error :check-soft-ban
                       "unable to report to ~a after enforcing soft_ban, target is missing from the control rooms group."
                       group-name)))))))

(define-target-step room-soft-ban (target control group event-id target-user reason)
  ;; we need to test that we have permission to send the soft-ba nstate event.
  (unless (can-send-state-p target (cl-matrix:username cl-matrix:*account*) *luna.soft-ban*)
    (error 'luna-permission-error :description (format nil "can't send ~a event in ~a" *luna.soft-ban* target)))

  ;; then test we have permission to ban a user.
  (unless (has-power-p target (cl-matrix:username cl-matrix:*account*) "ban")
    (error 'luna-permission-error :description (format nil "can't ban in ~a" target)))

  (let ((m.room.member (cl-matrix:room-state target "m.room.member" target-user)))
    (if (jsown:keyp m.room.member "membership")
        (cl-matrix:room-ban target-user (or reason "") target)
        ;; then we just issue the soft-ban event.
        (soft-ban target target-user :reason reason :report-to group :command event-id)))
  (step-result target))

(define-step group-soft-ban (control group event-id sender target-user reason)
  (unless (has-power-p control sender "ban")
    (error 'luna-permission-error :description
           (format nil "~a doesn't have permission to ban in this room." sender)))

  (step-result control :sub-steps
    (mapgroup (lambda (r)
                (room-soft-ban r control group event-id target-user reason))
              control group)))

(define-command-parser ban (name rest room-id event)
  "GROUP TARGET-USER [REASON...]
ban the user from all rooms in the group, without them force joining."
  (declare (ignore name))
  (cl-ppcre:register-groups-bind (group-name target-user reason)
      ("^(\\S+)\\s+(\\S+)(?:\\s+(.+))?" rest)
    (when (and group-name target-user)
      (funcall #'group-soft-ban room-id group-name (jsown:val event "event_id") (jsown:val event "sender")
                target-user reason))))

;;; see hooks.lisp for the join hook.
