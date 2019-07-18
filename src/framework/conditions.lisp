#|
   This file is part of cl-matrix
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>
|#
(in-package #:luna.framework)

(define-condition luna-error (error)
  ((description :initarg :description
                :reader condition-description))

  (:report (lambda (condition stream)
             (write-string (format nil "~a~%~a" (type-of condition) (condition-description condition)) stream))))

(define-condition luna-execution-error (luna-error) ())
(define-condition luna-parse-error (luna-error) ())
(define-condition luna-compile-error (luna-error) ())
(define-condition luna-permission-error (luna-error) ())

(defun log-trace-then-signal (category datum &rest arguments)
  (let ((c (apply #'make-condition datum arguments)))
    (v:trace category c)
    (error c)))
