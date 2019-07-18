#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)
(defconstant +passed+ :passed)
(defconstant +failed+ :failed
  "failed is when the job in question fails")
(defconstant +wait+ :wait)
(defconstant +execute+ :execute)
(defconstant +contested+ :contested
  "contested is when the job in question passes
but one or more sub job has an status different to passed ")
(defconstant +continue+ :continue
  "this is used for situations where an executor has method combination
and needs to tell the next-method that it is safe to continue.")
(defvar *status-heirarchy* (list +wait+ (list +execute+ +continue+) (list +passed+ +failed+ +contested+)))
#| status goes :wait -> :execute then either 
   :failed, :contested, :passed

|#

(defvar *debug-execution* nil
  "When t, executors will not invoke a restart when a condition is singaled in the step that is being executed and allow
for interactive debugging. ")

(defclass base-step () ())
(defclass base-job () ())

(defgeneric create-job (step &rest arguments)
  (:documentation "create a job from the step with the arguments"))
 
(defgeneric execute (job)
  (:documentation "execute the job and update the status"))

(defgeneric report (job/step stream &rest args)
  (:documentation "report the object to the stream with a summary of it's status, if nil is provided as the stream
then return a string.
this is an oppertunity to 

Example:
Job with status: :failed
   ban penis from !poo:matrix.org
with conditions
...
... you can see how control gets passed to other things."))

(defmethod report :around (job/step (stream (eql nil)) &rest args)
  (with-output-to-string (stream)
    (apply #'report job/step stream args)))

(defvar *step-table* (make-hash-table :test 'equal))
(defun get-step (name)
  (gethash name *step-table*))

(defun intern-step (name step)
  (setf (gethash name *step-table*) step))
