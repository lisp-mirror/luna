#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

(defclass plan (value-step) ())
(defclass plan-job (value-job)
  ((%step-values :reader sub-jobs)))

(defmethod sub-jobs :around ((job plan-job))
  (car (call-next-method)))

(defmacro define-plan-builder (name (&rest args) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((step-function
            (lambda ,args
              ,@body)))
       (intern-step ',name
                    (make-instance 'plan
                                   :step-function step-function)))))

(define-job-creator plan plan-job)

(defun %ensure-result-as-list-for-plan (job)
  "apply the job arguments to the step-function ensuring that the jobs that the plan returns are in a list
this is needed because a command-parser might return a single job that isn't in a list, this function also keeps
execute :before (job plan-job) clean.

See execute"
  (let ((results (multiple-value-list (apply (step-function (step-obj job))
                                             (arguments job)))))
    (unless (listp (car results))
      (setf (car results) (list (car results))))
    (setf (step-values job)
          results)))

(defmethod execute :before ((job plan-job))
  (setf (status job) +execute+)
  ;;; call the plan builder
  (let ((result
         (safely-execute job *debug-execution* +continue+ (%ensure-result-as-list-for-plan job))))
    (setf (status job) result))
  (unless (eql (status job) +continue+)
    (setf (status job) +failed+)))

(defmethod execute ((job plan-job))
  (when (eql (status job) +continue+)
    (setf (status job) +execute+)
    (mapc #'execute (sub-jobs job))
    (let ((bad-results
           (remove-if (lambda (r) (eql r +passed+)) (sub-jobs job) :key #'status)))
      (when bad-results
          (setf (status job) +contested+)))))

;;; later on use cl-markless and add colours to reports.
(defmethod report ((job plan-job) stream &rest args)
  (declare (ignore args))
  (format stream "~&~%plan-job finished with status ~a:" (status job))
  (write-string (ensure-left-margin 4 (apply #'report (step-obj job) nil (arguments job))) stream)
  (unless (eql (status job) +passed+)
    (format stream "~&experianced ~d conditions." (length (conditions job)))
    (format-indent 4 stream "~{~%~%~a~}" (conditions job))
    (write-string
     (ensure-left-margin 4 (with-output-to-string (s)
                             (mapc (lambda (j) (apply #'report j s (arguments j)))
                                   (sub-jobs job))))
     stream)))

