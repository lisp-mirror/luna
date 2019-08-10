#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

(defun make-luna-kernal (&optional (workers 8))
  (setf lparallel:*kernel*
        (lparallel:make-kernel workers :name "luna kernal")))

(defun sync-listener (seconds sync-token)
  (loop :do
       (multiple-value-bind (sync-data next-token) (cl-matrix:account-sync :since sync-token)
         (cl-matrix.base-events:issue-sync-event sync-data)
         (setf sync-token next-token))
       (sleep seconds)))

;; get sync token from config when it first starts up
(defun make-listener (sync-token sync-rate)
  (let ((sync-token sync-token))
    (v:info :listener "starting listener ~a" sync-token)
    (lambda ()
      (tagbody listen-start
         (handler-bind ((error (lambda (c) (unless *debug-execution* (invoke-restart 'restart-listener c)))))
           (restart-case (sync-listener sync-rate sync-token)
             (restart-listener (c) (v:error :listener "condition hit listener top:~%~a~%~%" c)
                               (sleep 1) ; we should wait just incase things go really wrong.
                               (go reset-listener))))

       reset-listener
         (v:info :listener "restarting listener")
         (setf sync-token (cl-matrix:now-token))
         (go listen-start)))))

(defun start-listening (&key (sync-token (cl-matrix:now-token)) (sync-rate 0.2))
  (bt:make-thread (make-listener sync-token sync-rate)
                  :initial-bindings `((*standard-output* . ,*standard-output*)
                                      (cl-matrix:*account* . ,cl-matrix:*account*)
                                      (lparallel:*kernel* . ,lparallel:*kernel*))))

(defmacro luna-lambda ((&rest lambda-list) &body body)
  "gets the context needed for tasks before submitting to worker threads, such as cl-matrix:*account*"
  (let ((account-sym (gensym))
        (debug-sym (gensym)))
    `(let ((,account-sym cl-matrix:*account*)
           (,debug-sym *debug-execution*))
       (lambda ,lambda-list
         (let ((*debug-execution* ,debug-sym))
           (declare (special *debug-execution*))
           (cl-matrix:with-account (,account-sym)
             ,@body))))))

