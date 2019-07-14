#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)
;;; do not forget to include version and space for other meta data
;;; in the room state for both the controller room and the target room
(defun ensure-left-margin (margin string)
  "for every newline in the string this will insert margin amount of spaces after."
  (declare (type string string)
           (type integer margin))

  (cl-strings:replace-all string (coerce '(#\Newline) 'string)
                          (format nil "~%~v@{~A~:*~}" margin " ")))

(defun format-indent (margin destination control-string &rest args)
  (write-string (ensure-left-margin margin
                                    (apply #'format (list* nil control-string args)))
                destination))


(declaim (inline get-user-from-levels))
(defun get-user-from-levels (user-id power-levels)
  "return the user power level, if the user is not present then the default power level will be assumed

returns another boolean value which is t if the default-level was given"
  (let ((users (jsown:filter power-levels "users"))
        (default-level (jsown:filter power-levels "users_default")))
    (if (jsown:keyp users user-id)
        (values (jsown:val users user-id) nil)
        (values default-level t))))

(defmacro apply-filter (obj &rest filter)
  `(jsown:filter ,obj ,@filter))

;;; fractions floats and stuff, matrix is really bad
(defun ensure-integer (i)
  (etypecase i
    (integer i)
    (string (parse-integer i :junk-allowed t))))

(defmacro define-cmps (name mapped-function &rest comparitors)
  "(defun weak< (&rest values) (apply #'< (mapcar #'ensure-integer values)))"
  `(progn
     ,@ (loop :for cmp :in comparitors :collect
             `(defun ,(intern (concatenate 'string
                                           (symbol-name name)
                                           (symbol-name cmp)))
                  (&rest numbers)
                (apply #',cmp (mapcar #',mapped-function numbers))))))


(define-cmps weak ensure-integer > < <= >= =)

(defun has-power-p (room-id user-id filter)
  (let* ((power-levels (cl-matrix:room-state room-id "m.room.power_levels"))
         (user-level (get-user-from-levels user-id power-levels))
         (required-level (apply-filter power-levels filter)))
    (weak>= user-level required-level)))

(defun bot-powered-p (room-id filter)
  "finds if the bot has permission in the room state for the power level given by the filter
will download the power_level state event and use cl-matrix:*account* to get the username"
  (has-power-p room-id (cl-matrix:username cl-matrix:*account*) filter))

(defun can-send-state-p (room-id user-id event-type)
  "finds if the user can send the state event"
  (let ((powered (has-power-p room-id user-id `("events" ,event-type))))
    (if powered
        t
        (has-power-p room-id user-id '("state_default")))))

(defun report-summary (control-id summary &optional event-id)
  (cl-matrix:msg-send summary control-id :type "m.notice" :event-id event-id)
  (v:info :report (format nil "sent summary to ~a~@[ in response to ~a~]:~%~%~a" control-id event-id summary)))
