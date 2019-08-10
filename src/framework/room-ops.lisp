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

(defun %apply-filter (obj filter)
  "Maybe I'm too sleepy but this gives me a unecessary feeling.
Returns a value if it exists for the filter, otherwise returns nil."
  (declare (type list filter))
  (if (null (cdr filter))
      (and (jsown:keyp obj (car filter))
           (jsown:val obj (car filter)))

      (and (jsown:keyp obj (car filter))
           (%apply-filter (jsown:val obj (car filter)) (cdr filter)))))

(defun has-power-p (room-id user-id &rest filter)
  "returns a second value so you can check if the filter was actually found."
  (let* ((power-levels (cl-matrix:room-state room-id "m.room.power_levels"))
         (user-level (get-user-from-levels user-id power-levels))
         (required-level (%apply-filter power-levels filter)))
    (if required-level
        (values (weak>= user-level required-level) t)
        (values nil nil))))

(defun bot-powered-p (room-id &rest filter)
  "finds if the bot has permission in the room state for the power level given by the filter
will download the power_level state event and use cl-matrix:*account* to get the username

See has-power-p"
  (apply #'has-power-p room-id (cl-matrix:username cl-matrix:*account*) filter))

(defun can-send-state-p (room-id user-id event-type)
  "finds if the user can send the state event"
  (let ((powered (has-power-p room-id user-id "events" event-type)))
    (if powered
        (values t t)
        (has-power-p room-id user-id "state_default"))))

;;; to do but not urgently (we aren't a client but we should still do this), escape the src=.
(defun room-preview (room)
  "return a readable org.matrix.custom.html room preview, something like 
[x](this is the room image as an img tag) <room name>"
  (let ((m.room.name  (cl-matrix:room-state room "m.room.name"))
        (m.room.avatar (cl-matrix:room-state room "m.room.avatar")))
    (format nil
            "~@[<img src=~s height=\"32\" alt\"room avatar\" vertical-align=\"middle\">~] ~@[~a~] (<code>~a</code>)"
            (and (jsown:keyp m.room.avatar "url") (jsown:val m.room.avatar "url"))
            (and (jsown:Keyp m.room.name "name") (jsown:val m.room.name "name"))
            room)))
#|
(cl-matrix:msg-send "<img src=\"mxc://matrix.org/fkUyAnpRqaCayYGPTLIKibfg\" height=\"32\" alt\"room avatar\" vertical-align=\"middle\"> Cool Room With Pic (<code>!HPFFIiVtELwKSMeato:matrix.org</code>)" "!HPFFIiVtELwKSMeato:matrix.org" :format "org.matrix.custom.html" :formatted-body "<img src=\"mxc://matrix.org/fkUyAnpRqaCayYGPTLIKibfg\" height=\"32\" alt\"room avatar\" vertical-align=\"middle\"> Cool Room With Pic (<code>!HPFFIiVtELwKSMeato:matrix.org</code>)")
|#
