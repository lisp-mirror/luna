#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

#|
(defun command-step-function (command-name command-string-args room-id event)
  (let ((plan
         (handler-case (funcall (get-parser command-name) command-name command-string-args room-id event)
           (error (c) (v:error :command-parser "a parser failed with condition~%~a" )))))

    (handler-case  )
    (unless (eql +passed+ (status parse-job))
        (error 'luna-parse-error ))
      (cond (command-plan-job
             (add-conditions job (make-condition 'luna-parse-error :description
                                                  (format nil "unable to parse:~% ~a~%into ~a"
                                                          command-string-args command-name)))
             (setf (status job) +failed+))

            (t
             (setf (group-job job)
                   (new-group-job))

             ;; needs protecting
             (execute (group-job job))))))
|#
(defun make-command-job (command-name command-string-args room-id event)
  (create-job (get-parser command-name)
              command-name command-string-args room-id event))

