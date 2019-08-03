#| This file is part of luna
Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#
(in-package #:luna.framework)

(defvar *report-table* (make-hash-table :test 'equal))

(defun default-reporter (target event-id conditions)
  (let ((message
         (cond ((null conditions)
                (with-output-to-string (s)
                  (format s "<font color=\"green\">Finished</font> Succesfully")))

               ((atom conditions)
                (with-output-to-string (s)
                  (format s "<font color=\"red\">Failed</font> with condition:~%~a" conditions)))

               (t (with-output-to-string (s)
                    (format s "<font color=\"yellow\">Contested</font> with ~d conditions:" (length conditions))
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

(defun report-summary (control-id summary &optional event-id)
  (cl-matrix:msg-send summary control-id :type "m.notice" :event-id event-id
                      :format "org.matrix.custom.html" :formatted-body summary)
  (v:info :report (format nil "sent summary to ~a~@[ in response to ~a~]:~%~%~a" control-id event-id summary)))
