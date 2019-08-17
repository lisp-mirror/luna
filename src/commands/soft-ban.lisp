#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna)
;;; alternativly we could put the luna.soft_ban inside the control room and have an affected rooms, has a much larger overhead when someone joins a room, all the groups will have to be downloaded and checked.
(defvar *luna.soft-ban* "luna.soft_ban")
;; might want to include a group-name to report to in the soft ban so it can alert when it's soft banned someone.
;; and if it's been attempted and failed.
;; it needs to be a group so that the bot can verify that the contorl room is still cotnrolilng the target
;; so someone doesn't use this as a vector to spam the control room.

(defun soft-ban (room-id user-id &optional reason)
  (cl-matrix.api.client:put-rooms/roomid/state/eventtype/statekey
   cl-matrix:*account* room-id *luna.soft-ban* (subseq user-id 1)
   (jsown:to-json (remove-if #'null `(:obj ("activep" . t) ,(and reason `("reason" . ,reason)))))))

(defun check-soft-ban (room-id user-id)
  (let ((luna.soft-ban (cl-matrix:room-state room-id *luna.soft-ban* (subseq user-id 1))))
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
         cl-matrix:*account* room-id *luna.soft-ban* (subseq user-id 1)
         (jsown:to-json (jsown:extend-js luna.soft-ban ("activep" :false))))))))

(define-target-step room-soft-ban (target control group target-user reason)
  ;; we need to test that we have permission to send the soft-ba nstate event.
  (unless (can-send-state-p target (cl-matrix:username cl-matrix:*account*) *luna.soft-ban*)
    (error 'luna-permission-error :description (format nil "can't send ~a event in ~a" *luna.soft-ban* target)))

  ;; then test we have permission to ban a user.
  (unless (can-send-state-p target (cl-matrix:username cl-matrix:*account*) "ban")
    (error 'luna-permission-error :description (format nil "can't ban in ~a" target)))

  ;; then we just issue the soft-ban event.
  (soft-ban target target-user reason)
  target)

(define-step group-soft-ban (control group sender target-user reason)
  (unless (has-power-p control sender "ban")
    (error 'luna-permission-error :description
           (format nil "~a doesn't have permission to ban in this room." sender)))

  (mapgroup (lambda (r)
              (room-soft-ban r control group target-user reason))
            control group))

(define-command-parser soft-ban (name rest room-id event)
  "GROUP TARGET-USER [REASON...]
ban the user from all rooms in the group, without forcing them to force join."
  (declare (ignore name))
  (cl-ppcre:register-groups-bind (group-name target-user reason)
      ("^(\\S+)\\s+(\\S+)(?:\\s+(.+))?" rest)
    (when (and group-name target-user)
      (funcall #'group-soft-ban room-id group-name (jsown:val event "sender") target-user reason))))

;;; ok these comments have fucked slime somehow
;;; ok use hooks.lisp
