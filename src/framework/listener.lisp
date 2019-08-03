#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

(defvar *channel* nil)

(defun make-luna-kernal (&optional (workers 8))
  (setf lparallel:*kernel*
        (lparallel:make-kernel workers :name "luna kernal")))

(defun sync-listener (seconds sync-token)
  (let ((*channel* (lparallel:make-channel)))
    (declare (special *channel*))
    (loop :do
         (multiple-value-bind (sync-data next-token) (cl-matrix:account-sync :since sync-token)
           (cl-matrix.base-events:issue-sync-event sync-data)
           (setf sync-token next-token))
         (sleep seconds))))

;; get sync token from config when it first starts up
(defun make-listener (sync-token sync-rate)
  (let ((sync-token sync-token))
    (v:info :listener "starting listener ~a" sync-token)
    (lambda ()
      (sync-listener sync-rate sync-token))))

(defun start-listening (&key (sync-token (cl-matrix:now-token)) (sync-rate 0.2))
  (bt:make-thread (make-listener sync-token sync-rate)
                  :initial-bindings `((*standard-output* . ,*standard-output*)
                                      (cl-matrix:*account* . ,cl-matrix:*account*)
                                      (lparallel:*kernel* . ,lparallel:*kernel*))))

(defmacro luna-lambda ((&rest lambda-list) &body body)
  "gets the context needed for tasks before submitting to worker threads, such as cl-matrix:*account*"
  (let ((account-sym (gensym)))
    `(let ((,account-sym cl-matrix:*account*))
       (lambda ,lambda-list
         (cl-matrix:with-account (,account-sym)
           ,@body)))))
