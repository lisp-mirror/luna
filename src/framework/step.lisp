#| This file is part of luna
Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#
(in-package #:luna.framework)

(defvar *debug-execution* nil)

(declaim (inline step-result))
(defun step-result  (room-id &optional error)
  (declare (type string room-id))
  (cons room-id error))

(defmacro catch-error (effective-room &body body)
  "provides a common way to handle errors in steps. If there is an error, then under nomal circumstances, result-step is called with the effective-room and the condition to provide a result for the step.

See step-result
See define-step
"
  `(handler-bind ((error #'(lambda (c)
                             (unless *debug-execution* (invoke-restart 'return-condition c)))))
     (block restart-block
       (restart-case (progn ,@body)
         (return-condition (condition) (return-from restart-block (step-result ,effective-room condition)))))))


(defmacro define-step (step-name (effective-room &rest lambda-list) &body body)
  "effective-room, the room which the step is to operate on. This would be a control room in luna if the step was a group command, or it would be a target room if it was a single operation such as banning a user from a target room."
  (multiple-value-bind (body declerations docstring) (alexandria:parse-body body)
    `(defun ,step-name (,effective-room ,@lambda-list)
       ,@ (if (null docstring) declerations (append docstring declerations))
          (catch-error ,effective-room
                       ,@body))))


