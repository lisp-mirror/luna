#| This file is part of luna
Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#
(in-package #:luna.framework)

(defclass value-step (single-step) ()
  (:documentation "a step that needs to store the values returned by the step-function "))
(defclass value-job (job)
  ((%step-values :initarg :step-values
                  :accessor step-values
                  :type list
                  :initform nil
                  :documentation "stores the values result of the step-function "))
  (:documentation "a job that will store the values from it's values-step that can be accessed with step-values."))

(defmethod step-values :around ((job job))
  (values-list (call-next-method)))

(defmethod execute ((job job))
  (let ((result
         (handler-case (setf (step-values job)
                             (multiple-value-list (apply (step-function (step-obj job))
                                                         (arguments job))))
           (error (c) (add-conditions job c) +failed+)
           (:no-error (c) +passed+))))
    (setf (status job) result))
  job)

(define-job-creator value-step value-job)
