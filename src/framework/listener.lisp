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

(defun make-listener (sync-token sync-rate)
  "return a function that will serve as the listener"
  (let ((sync-token sync-token))
    (v:info :listener "starting listener ~a" sync-token)
    (lambda ()
      (tagbody listen-start
         (handler-bind
             ((usocket:timeout-error
               (lambda (c) (unless *debug-execution*
                             (invoke-restart 'timeout-restart-listener c))))
              (error (lambda (c)
                       (unless *debug-execution*
                         (invoke-restart 'generic-restart-listener c)))))

           (restart-case
               (loop :do
                    (multiple-value-bind (sync-data next-token)
                        (cl-matrix:account-sync :since sync-token)
                      (cl-matrix.base-events:issue-sync-event sync-data)
                      (setf sync-token next-token))
                    (sleep sync-rate))

             (generic-restart-listener (c)
               (v:error :listener "condition hit listener top:~%~a~%~%" c)
               (sleep 1) ; we should wait just incase things go really wrong.
               (go generic-reset-listener))

             (timeout-restart-listener (c)
               (v:error :listener "caught timeout at top:~%~a~%" c)
               (sleep 10) ; wait until server comes back.
               (go timeout-reset))))

       generic-reset-listener
         (v:info :listener "getting new sync token for reset")
         (setf sync-token (cl-matrix:now-token))
       timeout-reset
         (v:info :listener "restarting listener")
         (go listen-start)))))

(defun start-listening (&key (sync-token (cl-matrix:now-token)) (sync-rate 0.2))
  (bt:make-thread (make-listener sync-token sync-rate)
                  :initial-bindings `((*standard-output* . ,*standard-output*)
                                      (cl-matrix:*account* . ,cl-matrix:*account*)
                                      (lparallel:*kernel* . ,lparallel:*kernel*))))

(defun start-luna (account &key (sync-rate 2))
  (cl-matrix:with-account (account)
    (make-luna-kernal)
    (start-listening :sync-rate sync-rate)))

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
