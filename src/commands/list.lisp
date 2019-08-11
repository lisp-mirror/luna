#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna)

(define-step list-group (control group-name)
  (let ((group (get-group control group-name)))
    (unless (jsown:keyp group "target_rooms")
      (error 'luna-error :description (format nil "no target rooms for group ~a" group-name)))

    (with-output-to-string (s)
      (dolist (room (jsown:val group "target_rooms"))
        (format s "~a~%~%" (room-preview room))))))

(define-command-parser list (name rest room-id event)
  "GROUP
lists and displays the readable names and room-ids for the list of target rooms in the group."
  (declare (ignore name event))
  (cl-ppcre:register-groups-bind (group) ("^(\\S+)\\s*" rest)
    (list-group room-id group)))
