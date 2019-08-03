#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

(defvar *parser-table* (make-hash-table :test 'equal)
  "the parser table needs to intern strings tbh
because package complicated things a lot.")
(defun transform-parser-designator (designator)
  (string-upcase
   ;; first convert the designator to string, then upcase it so that it is EQUAL.
   (etypecase designator
     (string designator)
     (symbol (symbol-name designator)))))

(defun get-parser (designator)
  (let ((designator (transform-parser-designator designator)))
    (gethash designator *parser-table*)))

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

See make-command-job
See luna-command
See intern-parser
See get-parser"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (intern-parser ',name
                    (lambda (,given-command ,string-args ,room-id ,event)
                      ,@body))))
