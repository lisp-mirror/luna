#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

(defvar *parser-table* (make-hash-table :test 'equal)
  "the parser table needs to intern strings tbh
because package complicated things a lot.")

(defclass parser ()
  ((%%function :initarg :function
              :initform (error 'luna-compile-error "must supply a function to a parser")
              :accessor %parser-function
              :type function)

   (%%helpstring :initarg :helpstring
               :initform ""
               :accessor %parser-help
               :type string)))

(defun transform-parser-designator (designator)
  (string-upcase
   ;; first convert the designator to string, then upcase it so that it is EQUAL.
   (etypecase designator
     (string designator)
     (symbol (symbol-name designator)))))

(defun %get-parser (designator)
  (let ((designator (transform-parser-designator designator)))
    (gethash designator *parser-table*)))

(defun get-parser (designator)
  "get the parser function from the parser table for the given designator"
  (%parser-function (or (and designator (%get-parser designator)) (%get-parser "help"))))

(defun get-help (designator)
  "get the help message from the parser table for the given designator"
  (let ((parser (%get-parser designator)))
    (or (and parser (%parser-help parser))
        "")))

(defmethod get-synopsis ((parser parser))
  (car (cl-strings:split (%parser-help parser) (coerce '(#\Newline #\Newline) 'string))))

(defmethod get-synopsis (designator)
  (let ((parser (%get-parser designator)))
    (or (and parser (get-synopsis parser))
        "")))

(defun list-parsers-with-synopsis ()
  "used for help pages, doc purposes.

reutrns a string with the synopsis messages for all interned commands."
  (with-output-to-string (s)
    (maphash (lambda (k v)
               (format s "~a: ~a~%~%" (string-downcase k) (get-synopsis v)))
             *parser-table*)))

(defun intern-parser (designator parser)
  "parsers are interned by the symbol name as string.
At the minute there is no package system."
  (let ((designator (transform-parser-designator designator)))
    (setf (gethash designator *parser-table*) parser)))

(defmacro define-command-parser (name (given-command string-args room-id event) &body body)
  "define a parser for make-command-job and the luna-command hook to use.
name is the key the parser is to be interned with.
The strucutre of the lambda-list is as follows:
!luna <given-command> <&rest string-args>.
commands parsers are usually retrieved by doing (get-parser given-command) but it is possible to have
foo and baz point to the same parser in the parser-table, hence the need to pass a given-command to the parser so
it is possible to tell if it was called as foo or as baz.

The docstring supplied to this macro will be used as the help message for a given command. The paragraph (string seperated by two line breaks) of docstring will be used as the synopsis when listing all commands.

See intern-parser
See get-parser"
  (multiple-value-bind (new-body declarations docstring) (alexandria:parse-body body :documentation t)
    (declare (ignore new-body declarations))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (intern-parser ',name
                      (make-instance 'parser :function
                                     (lambda (,given-command ,string-args ,room-id ,event)
                                       ,@body)
                                     :helpstring ,(or docstring ""))))))
