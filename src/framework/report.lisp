#| This file is part of luna
Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#
(in-package #:luna.framework)

(defvar *report-table* (make-hash-table :test 'equal))

(defun bad-resultp (thing)
  (and (listp thing)
       (and (typep (cdr thing) 'error))))

(defun default-reporter (target event-id result/s)
  (let ((message
         (if (bad-resultp result/s)
             (with-output-to-string (s)
               (format s "<font color=\"red\">Failed</font> with condition:~%~a" (cdr result/s)))
             (let ((bad-rooms (remove-if-not #'bad-resultp result/s)))
               (cond ((null bad-rooms)
                      (with-output-to-string (s)
                        (format s "<font color=\"green\">Finished</font> Succesfully")))

                     (t (with-output-to-string (s)
                          (format s "<font color=\"yellow\">Contested</font> with ~d conditions:" (length bad-rooms))
                          (dolist (r bad-rooms)
                            (format-indent 4 s "~%~a" (room-preview (car r)))
                            (format-indent 8 s "~%~a" (cdr r))))))))))
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

(defmacro with-generic-error-handling (category &body body)
  `(handler-case (progn ,@body)
     (error (c) (v:error ,category c))))

(defun defer-report (parser-name rest room-id event)
  (let ((*channel* (lparallel:make-channel)))
    (declare (special *channel*))
    (lparallel:submit-task *channel*
      (luna-lambda ()
        (with-generic-error-handling :reporter
          (funcall (get-reporter parser-name)
                   room-id (jsown:val event "event_id")
                   (funcall (get-parser parser-name) parser-name rest room-id event)))))))

(defun report-summary (control-id summary &optional event-id)
  (cl-matrix:msg-send summary control-id :type "m.notice" :event-id event-id
                      :format "org.matrix.custom.html" :formatted-body summary)
  (v:info :report (format nil "sent summary to ~a~@[ in response to ~a~]:~%~%~a" control-id event-id summary)))
