#| This file is part of luna
Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#
(in-package #:luna.framework)

(defvar *report-table* (make-hash-table :test 'equal))

(defun default-reporter (target event-id conditions)
  (let ((message
         (cond ((null conditions)
                (with-output-to-string (s)
                  (format s "Finished Succesfully")))

               (t (with-output-to-string (s)
                    (format s "Contested with ~d conditions:" (length conditions))
                    (format-indent 4 s "~{~%~a~}" conditions))))))
    (report-summary target message event-id)))

(defun intern-reporter (name reporter)
  (setf (gethash (transform-parser-designator name)
                 *report-table*)
        reporter))

(defun get-reporter (name)
  (or (gethash (transform-parser-designator name) *report-table*)
      #'default-reporter))

(defmacro define-reporter (name (room-id event-id &rest more-args) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (intern-reporter ,name
                      (lambda (,room-id ,event-id ,@more-args)
                        ,@body))))

(defun defer-report (channel parser-name rest room-id event)
  (lparallel:submit-task channel
    (luna-lambda ()
      (funcall (get-reporter parser-name)
               room-id (jsown:val event "event_id")
               (funcall (get-parser parser-name) parser-name rest room-id event)))))
