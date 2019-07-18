#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

(defclass single-step (base-step)
  ((step-function :initarg :step-function :accessor step-function
                  :type function
                  :documentation "this is a function that describes the step")

   (reporter :initarg :reporter :accessor reporter
             :initform (lambda (stream &rest args)
                         (format stream "step with: ~a" args))
             :type function
           :documentation "a function to be called with a step instance, stream and the same arguments as the step-funcion expects and report what the step is going to do to the stream in the context of the arguments provided.
if nil is provided as the stream then the report is expected to return a string.
")))

(defmacro define-step-reporter (step-name (&rest lambda-list) &body body)
  (let ((step (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,step (get-step ',step-name)))
         (if ,step
             (setf (reporter ,step)
                   (lambda ,lambda-list ,@body))
             (error 'luna-compile-error :description (format nil "there is no step with the name ~a" ',step-name)))))))

(defmethod report ((step single-step) stream &rest args)
  (apply (reporter step)
         stream args))

(defmacro define-job-creator (step-type job-type)
  `(defmethod create-job ((step ,step-type) &rest arguments)
     (make-instance ',job-type :arguments arguments
                    :step step)))

(define-job-creator single-step job)

(defclass job (base-job)
  ((arguments :initarg :arguments :accessor arguments
              :type list
              :documentation "the arguments to call the step-function with. or the job to use in some other way
when it is executed.")
   (status :initarg :status :accessor status
           :initform +wait+ :type symbol)

   (conditions :initarg :conditions :accessor conditions
               :initform nil :type list)

   (step-obj :initarg :step :accessor step-obj
             :type single-step)))

(defmethod print-object ((job job) stream)
  (print-unreadable-object (job stream :type t :identity t)
    (format stream "status: ~a, conditions: ~a" (status job) (length (conditions job)))))

(defun %status-branch-equal (status branch)
  (cond ((listp branch)
                  (member status branch))

                 (t (eql status branch))))
(defun %nth-status (status)
  (let* ((n 0)
         (result
          (loop :for branch :in *status-heirarchy* :do
               (if (%status-branch-equal status branch)
                   (return n)
                   (incf n)))))
    (or result
        (error 'luna-error :description (format nil "~a could not be found in the status heirarchy" status)))))

(define-cmps status %nth-status < =)

(defmethod (setf status) :around (new-status (job job))
  (let ((current-status (status job)))
    (if (status< new-status 
                  current-status)
        (error 'luna-error :description
               (format nil  "attempt to set the status of a job with status ~a to ~a"
                       current-status new-status))
        (call-next-method))))

(defmethod add-conditions ((job job) &rest conditions)
  (setf (conditions job)
        (list* (conditions job) conditions)))

;;; find if this passed goes through even if it fails.
(defmacro safely-execute (job debuggerp default-status &body body)
  "execute the body with default restart options if there is no-error +passed+ will be the result."
  `(handler-bind ((error #'(lambda (c)
                             (format t "entering handler bind!!!!")
                             (add-conditions ,job c)
                             (unless ,debuggerp (invoke-restart 'fail-job)))))
     (block restart-block
       (restart-case (progn ,@body ,default-status)
         (fail-job () (return-from restart-block +failed+))))))

(defmethod execute ((job job))
  (let ((result
         (safely-execute job *debug-execution* +passed+ (apply (step-function (step-obj job))
                              (arguments job)))))
    (setf (status job) result))
  job)

(defmethod report ((job job) stream &rest args)
  (declare (ignore args))
  (unless (eql +passed+ (status job))
    (format stream "~%job finshsed with status ~a for step:"
            (status job))
    (write-string (ensure-left-margin 4 (apply #'report (list* (step-obj job) nil (arguments job)))) stream)
    (format stream "~&experianced ~d conditions." (length (conditions job)))
    (format-indent 4 stream "~{~%~%~a~}" (conditions job))))
