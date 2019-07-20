#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

(defmacro define-room-step (name (room-id-sym &rest args) (&rest filter) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((step-function
            (lambda (,room-id-sym ,@args)
              ,@(prog1 (remove-if-not (lambda (l) (eql l 'declare)) body :key #'car)
                 (setf body (delete-if (lambda (l) (eql l 'declare)) body :key #'car)))
              ,(if filter
                   `(cond ((bot-powered-p ,room-id-sym ',filter)
                          ,@body)

                         (t (log-trace-then-signal :room-step 'luna-permission-error :description
                                                   (format nil "insufficent power level ~a in room ~a" ',filter ,room-id-sym))))
                   `(progn ,@body)))))
       (intern-step ',name
                    (make-instance 'single-step
                                   :step-function step-function)))))

