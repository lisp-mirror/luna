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

(defmacro define-command-parser (name (&rest lambda-list) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (intern-parser ',name
                    (make-instance 'plan
                                   :step-function
                                   (lambda ,lambda-list
                                     ,@body)))))
