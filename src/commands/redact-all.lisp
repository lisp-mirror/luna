#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna)

(define-target-step room-redact-all (target control group target-user reason)
  (unless (has-power-p target (cl-matrix:username cl-matrix:*account*) "redact")
    (error 'luna-permission-error :description
           (format nil "~a doesn't have permission in ~a to redact events." (cl-matrix:username cl-matrix:*account*) target)))
  (let ((history-generator
         (cl-matrix:history-generator target :filter (format nil "{\"senders\":[~s]}" target-user))))
    (loop :for events := (funcall history-generator) :until (null events) :do
         (v:info :events events)
         (mapc 
          (lambda (e)
            (cl-matrix:room-redact target (jsown:val e "event_id") :reason reason)
            (v:info :redact-all "processing event"))
          events))
    target))

(define-step redact-all (control group sender target-user reason)
  (unless (has-power-p control sender "redact")
    (error 'luna-permission-error :description
           (format nil "~a doesn't have permission to redact in this room." sender)))

  (prog1
    (remove-if-not (lambda (f) (typep f 'error))
                   (mapgroup
                    (lambda (r)
                      (room-redact-all r control group target-user reason))
                             control group))
     (v:info :redact-all "exiting redact all")))

(define-command-parser redact (name rest room-id event)
  (declare (ignore name))
  (cl-ppcre:register-groups-bind (group-name target-user reason)
      ("^(\\S+)\\s+(\\S+)(?:\\s+(.+))?" rest)
    (when (and group-name target-user)
      (funcall #'redact-all room-id group-name (jsown:val event "sender") target-user reason))))
