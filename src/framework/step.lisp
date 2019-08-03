#| This file is part of luna
Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#
(in-package #:luna.framework)

(defvar *debug-execution* nil)

(defmacro catch-error (&body body)
  `(handler-bind ((error #'(lambda (c)
                             (unless *debug-execution* (invoke-restart 'return-condition c)))))
     (block restart-block
       (restart-case (progn ,@body)
         (return-condition (condition) (return-from restart-block condition))))))


(defmacro define-step (step-name (&rest lambda-list) &body body)
  (multiple-value-bind (body declerations docstring) (alexandria:parse-body body)
    `(defun ,step-name ,lambda-list
       ,@ (if (null docstring) declerations (append docstring declerations))
       (catch-error ,@body))))
