#| This file is part of luna
Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#
(in-package #:luna.framework)

(defvar *debug-execution* nil
  "set to true if you want conditions to pass through handler-binds that would normally capture erros and report them to control rooms. Should only be used to debug.")

(declaim (inline step-result))
(defun step-result  (room-id &key sub-steps condition description)
  (declare (type string room-id))
  (apply #'make-step
         (remove-if #'null `((:room . ,room-id)
                             (:condition . ,condition)
                             (:sub-steps . ,sub-steps)
                             (:description . ,description))
                    :key #'cdr)))

(declaim (inline make-step))
(defun make-step (&rest step-args)
  (cons :step step-args))

(defun step-condition (step)
  "a bad result is a result with a condition

See define-step
See catch-error"
  (cdar (member :condition (cdr step) :key #'car)))

(defmacro catch-error (effective-room &body body)
  "provides a common way to handle errors in steps. If there is an error, then under nomal circumstances, result-step is called with the effective-room and the condition to provide a result for the step.

See step-result
See define-step
"
  `(handler-bind ((error #'(lambda (c)
                             (unless *debug-execution* (invoke-restart 'return-condition c)))))
     (block restart-block
       (restart-case (progn ,@body)
         (return-condition (condition) (return-from restart-block (step-result ,effective-room :condition condition)))))))


(defmacro define-step (step-name (effective-room &rest lambda-list) &body body)
  "Create a step that operates within the context of a single room (the effective-room).
This would be a control room in luna if the step was a group command, or it would be a target room if it was a single operation such as banning a user from a target room.

You don't have to use define-step, but it provides a way to talk to the default reporter depending on whether the step experiances a condition (which is to return a cons containing the effective-room and the condition).

See luna:define-target-step
See default-reporter
See catch-error"
  (multiple-value-bind (body declerations docstring) (alexandria:parse-body body)
    `(defun ,step-name (,effective-room ,@lambda-list)
       ,@ (if (null docstring) declerations (append docstring declerations))
          (catch-error ,effective-room
                       ,@body))))


