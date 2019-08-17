#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna)

(define-step add-room-to-group (target control group sender)
  (declare (type string target control group sender))
  (unless (can-send-state-p target (cl-matrix:username cl-matrix:*account*) "luna.group")
    (error 'luna-permission-error :description (format nil "~a doesn't have permission in ~a to start a group"
                                                       (cl-matrix:username cl-matrix:*account*) target)))
  (unless (can-send-state-p target sender "luna.group")
    (error 'luna-permission-error :description (format nil "~a doesn't permission to add ~a to the group ~a"
                                                       sender target control)))

  (add-control-to-target group control target)
  target)

(define-step add-to-group (control group sender &rest targets)
  (unless (can-send-state-p control sender *luna.group*)
    (error 'luna-permission-error :description (format nil "~a doesn't have permission to send ~a events"
                                                       sender *luna.group*)))
  (let ((results
         (lparallel:pmapcar
          (luna-lambda (r)
            (add-room-to-group r control group sender))
          targets)))
    (apply #'add-targets-to-control group control (remove-if #'bad-resultp results))
    results))

;;; !luna add-to-group cheesewheel &rest
;;; could do reprots here? passing the conditions promise to them?
(define-command-parser add-to-group (name rest room-id event)
  "GROUP TARGET-ROOMS...
add the given rooms to the group."
  (declare (ignore name))
  (cl-ppcre:register-groups-bind (group-name targets)
      ("^(\\S+)\\s+((?:\\s*\\S+)+)" rest)
    (when (and group-name targets)
      (let ((targets (cl-strings:split targets)))
        (apply #'add-to-group room-id group-name (jsown:val event "sender") targets)))))
