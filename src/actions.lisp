#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna)

;;; ban
(define-room-step room-ban (room-id user-id reason) ("ban")
  (cl-matrix:room-ban user-id reason room-id))

(define-step-reporter room-ban (stream room-id user-id reason)
  (declare (ignorable user-id reason room-id))
  (format stream "~&ban ~a from ~a" user-id room-id))

(define-plan-builder ban-builder (sender control-room group-name user-id reason)
  (unless (has-power-p control-room sender "ban")
    (error 'luna-error :descriptor (format nil "~&~a doesn't have permission to ban in ~a" sender control-room)))
  (mapgroup (lambda (r)
              (create-job (get-step 'ban) r user-id reason))
            control-room group-name))

(define-step-reporter ban-builder (stream sender control-room group-name user-id reason)
  (declare (ignorable sender control-room group-name user-id reason))
  (format stream "~&plan to ban ~a from group ~a ~@[for reason ~a~]"
          user-id group-name reason))

(define-command-parser ban (name rest room-id event)
  (declare (ignore name))
  (cl-ppcre:register-groups-bind (group user-id reason)
      ("^(\\S+)\\s+(\\S+)\\s+(.+)?" rest)
    (when (and group user-id)
        (create-job (get-step 'ban-plan)
                    (jsown:val event "sender") room-id group user-id reason))))
