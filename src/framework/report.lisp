#| This file is part of luna
Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#
(in-package #:luna.framework)

(defvar *report-table* (make-hash-table :test 'equal))

(defun bad-resultp (thing)
  "a bad result is a list that looks like this
(cons effective-room condition)

See define-step
See catch-error"
  (and (listp thing)
       (and (typep (cdr thing) 'error))))

(defun default-reporter (target event-id result/s)
  "The default reporter

if results is a string, then report the string directly to the given room-id.

if results is a list, report all the bad results in the list to the given room-id and mark as \"contested\".
if the list is empty or contains no bad results, then report to the room as \"Success\".

if results is a bad-result then report the condition to the given room as \"failed\".

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
  "creates a reporter interned with the given name through transform-parser-designator.

In luna, when a parser is called through the issue-luna-command hook after someone uses !luna <command>,
the hook will try to search for a reporter for <command> and if it doesn't find one the hook will use the default-reporter.
The reporter is then funcalled with the room-id, the event-id in which the command was sent and also the result that comes from the parser. 

You are not limited by the issue-luna-command hook though, lunas soft ban functionality uses another hook and reporter which accepts different arguments.

See defer-report"
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
  "moves submits the body to the luna-kernal to be picked up by a worker thread.

See with-generic-error-handling"
  `(let ((*channel* (lparallel:make-channel)))
     (declare (special *channel*))
     (lparallel:submit-task *channel*
      (luna-lambda ()
        (with-generic-error-handling ,log-category
          ,@task)))))

(defmacro with-reporting (reporter-designator &rest args)
  "finds the reporter for the designator and applies it to the arguments given

See get-reporter"
  `(funcall (get-reporter ,reporter-designator)
            ,@args))

(defun defer-report (parser-name rest room-id event)
  "defer reporting and execution of the command to the luna-kernal.
Will use with-reporting to call the reporter on the parser and the given args.

See with-defered-task
See with-reporting"
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
  "wraps with-output-to-string and report-summary, creates an output stream to write to and then sends the string to the room specified, if an event-id is given then it will send the report as a reply to it.

See report-summary"
  `(report-summary ,room-id (with-output-to-string (,var)
                             ,@body)
                  ,event-id))


