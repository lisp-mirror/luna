#| This file is part of luna
Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#
(in-package #:luna.framework)

(defun report-children (format stream ignoring &rest children)
  "report each of the child elements ignoring those that are in the ignore list"
  (mapc (lambda (c)
          (unless (member (car c) ignoring)
            (report (car c) c stream format)))
        children))

(defgeneric report (key association stream format))

(defmethod report (key association s format)
  (when (listp (cdr association))
    (apply #'report-children format s nil (cdr association))))

(defmethod report :around ((key (eql nil)) association (s (eql nil)) format)
  (with-output-to-string (s)
    (report (car association) association s format)))

(defmethod report :around ((key (eql :room)) association stream format)
  (unless (or (listp (cdr association)) (null (cdr association)))
    (let ((room (cdr association)))
      (let ((m.room.name (cl-matrix:room-state room  "m.room.name"))
            (m.room.avatar (cl-matrix:room-state room "m.room.avatar")))
        (setf (cdr association) nil)
        (pushnew `(:room-id . ,room) (cdr association))
        (pushnew `(:m.room.name . ,m.room.name) (cdr association))
        (pushnew `(:m.room.avatar . ,m.room.avatar) (cdr association)))))
  (call-next-method))

(defmethod report ((key (eql :room)) association stream (format (eql :text)))
  (format stream "~:[Unnamed Room.~;~:*~a~] (~a)"
          (cdr (assoc :m.room.name (cdr association)))
          (cdr (assoc :room-id (cdr association)))))

(defmethod report ((key (eql :room)) association stream (format (eql :org.matrix.custom.html)))
  (format stream "~@[<img src=~s height=\"32\" alt\"room avatar\" vertical-align=\"middle\">~]"
          (cdr (assoc :m.room.avatar (cdr association))))
  (format stream "~:[Unnamed Room.~;~:*~a~] (<code>~a</code>)"
          (cdr (assoc :m.room.name (cdr association)))
          (cdr (assoc :room-id (cdr association)))))

(defmethod report ((key (eql :step)) association stream format)
  (flet ((report-room-info-p (room-info condition sub-steps)
           (or (cdr room-info) (or condition sub-steps))))
    (let ((room-info (assoc :room (cdr association)))
          (condition (cdr (assoc :condition (cdr association))))
          (sub-steps (assoc :sub-steps (cdr association)))
          (description (cdr (assoc :description (cdr association)))))

      (when (report-room-info-p room-info sub-steps condition)
        (report (car room-info) room-info stream format))

      (let ((child-report
             (with-output-to-string (s)
               (when description (write-string description s))
               (when condition
                 (format s "~%Experianced condition:~%")
                 (format-indent 4 s "~:[<font color=\"yellow\">~a</font>~;~a~]"
                                (eql format :text) condition))
               (report-children format s '(:room :condition :description) (cdr association)))))
        (when (report-room-info-p room-info condition sub-steps)
          (ensure-left-margin 4 child-report))
        (write-string child-report stream)))))

