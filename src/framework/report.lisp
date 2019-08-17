#| This file is part of luna
Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#
(in-package #:luna.framework)

(defvar *report-table* (make-hash-table :test 'equal))

(defun bad-resultp (thing)
  "a bad result is a list that looks like this
(cons room-id-error-was-singaled-in condition)

See define-step"
  (and (listp thing)
       (and (typep (cdr thing) 'error))))

(defun default-reporter (target event-id result/s)
  "The default repoter

if results is a string, then return the string to the given room-id as passed.

if results is a list, report all the bad results in the list to the given room-id as contested.

if results is a bad-result then report to the given room as failed.

See bad-resultp"
  (let ((message
         (cond ((bad-resultp result/s)
                (with-output-to-string (s)
                  (format s "<font color=\"red\">Failed</font> with condition:~%~a" (cdr result/s))))

               ((stringp result/s) result/s)

               (t
                (let ((bad-rooms (remove-if-not #'bad-resultp result/s)))
                  (cond ((null bad-rooms)
                         (with-output-to-string (s)
                           (format s "<font color=\"green\">Finished</font> Succesfully")))

                        (t (with-output-to-string (s)
                             (format s "<font color=\"yellow\">Contested</font> with ~d conditions:" (length bad-rooms))
                             (dolist (r bad-rooms)
                               (format-indent 4 s "~%~a" (room-preview (car r)))
                               (format-indent 8 s "~%~a" (cdr r)))))))))))
    (report-summary target message event-id)))

(defun intern-reporter (name reporter)
  (setf (gethash (transform-parser-designator name)
                 *report-table*)
        reporter))

(defun get-reporter (name)
  (or (gethash (transform-parser-designator name) *report-table*)
      #'default-reporter))

(defmacro define-reporter (name (&rest lambda-list) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (intern-reporter ',name
                      (lambda ,lambda-list
                        ,@body))))

(defmacro with-generic-error-handling (category &body body)
  `(handler-bind ((error (lambda (c)
                               (unless *debug-execution* (invoke-restart 'return-and-log c)))))
     (block restart-block
       (restart-case (progn ,@body)
         (return-and-log (c) (v:error ,category c) (return-from restart-block))))))

(defmacro with-defered-task (log-category &body task)
  `(let ((*channel* (lparallel:make-channel)))
     (declare (special *channel*))
     (lparallel:submit-task *channel*
      (luna-lambda ()
        (with-generic-error-handling ,log-category
          ,@task)))))

(defmacro with-reporting (reporter-designator &rest args)
  `(funcall (get-reporter ,reporter-designator)
            ,@args))

(defun defer-report (parser-name rest room-id event)
  (with-defered-task :reporter
      (with-reporting parser-name
        room-id (jsown:val event "event_id")
        (funcall (get-parser parser-name) parser-name rest room-id event))))

(defun report-summary (control-id summary &optional event-id)
  (cl-matrix:msg-send summary control-id :type "m.notice" :event-id event-id
                      :format "org.matrix.custom.html"
                      :formatted-body (cl-strings:replace-all summary (coerce '(#\Newline) 'string) "<br/>"))
  (v:info :report (format nil "sent summary to ~a~@[ in response to ~a~]:~%~%~a" control-id event-id summary)))

(defmacro with-stream-to-report ((var room-id &optional event-id) &body body)
  "wraps with-output-to-string and report-summary, creates an output stream to write to and then sends the string to the room specified.

See report-summary"
  `(report-summary ,room-id (with-output-to-string (,var)
                             ,@body)
                  ,event-id))


