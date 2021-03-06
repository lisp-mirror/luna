#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna)

(define-target-step room-ban (target control group target-user reason)
  (unless (has-power-p target (cl-matrix:username cl-matrix:*account*) "ban")
    (error 'luna-permission-error :description
           (format nil "~a doesn't have permission in ~a to ban users." (cl-matrix:username cl-matrix:*account*) target)))

  (cl-matrix:room-ban target-user reason target)
  (step-result target))

(define-step ban (control group sender target-user reason)
  (unless (has-power-p control sender "ban")
    (error 'luna-permission-error :description
           (format nil "~a doesn't have permission to ban in this room." sender)))

  (step-result nil :sub-steps
               (mapgroup (lambda (r)
                           (room-ban r control group target-user reason))
                         control group)))

(define-command-parser hard-ban (name rest room-id event)
  "GROUP TARGET-USER [REASON...]
USE BAN INSTEAD. ban the user from all rooms in the group, will cause them to \"force join\"."
  (declare (ignore name))
  (cl-ppcre:register-groups-bind (group-name target-user reason)
      ("^(\\S+)\\s+(\\S+)(?:\\s+(.+))?" rest)
    (when (and group-name target-user)
      (funcall #'ban room-id group-name (jsown:val event "sender") target-user reason))))
