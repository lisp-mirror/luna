#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna)

(defconstant +state-type+ "luna.group")
#|
{ type : luna.group.control,
  state_key : all
  content :
    {
      target_rooms : [list]
    }
}

{ type : luna.group.target,
  state_key : all
  content :
    {
      control_room : foo
    }
}

;;; msc1772
{
    "type": "m.room.group",
    "state_key": "#room1:example.com",
    "contents": {
        "present": true
    }
}
{
    "type": "m.room.group",
    "state_key": "#room2:example.com",
    "contents": {
        "present": true,
        "autojoin": true
    }
}
{
    "type": "m.room.subgroup",
    "state_key": "+something:example.com",
    "contents": {
        "present": true
    }
}
{
    "type": "m.room.subgroup",
    "state_key": "+otherthing:example.com",
    "contents": {
        "present": true
    }
}
|#

(defun mapgroup (function control-room group-name)
  "calls the function for each room-id in the group specified in the control-room with group-name
(basically it wraps mapcar)."
  (let ((luna.group (cl-matrix:room-state control-room +state-type+ group-name)))
    (unless luna.group
      (error 'luna-error :description (format nil "~&there is no group named ~a for this room ~a." group-name control-room)))
    (let ((rooms (jsown:val luna.group "target_rooms")))
      (mapcar function rooms))))

(defun control-present-in-target-p (group-name control-id target-id)
  "true if the control room is present in the luna.group event"
  (declare (type string group-name control-id target-id))
  (let ((luna.group (cl-matrix:room-state target-id +state-type+ group-name)))
    (and luna.group
      (jsown:keyp luna.group "control_room")
      (string= control-id (jsown:val luna.group "control_room")))))

(defun add-control-to-target (group-name control-id target-id)
  "edits the room state of only the target room to add it to the group."
  (declare (type string group-name control-id target-id))
  (let ((existing-state (cl-matrix:room-state target-id +state-type+ group-name)))
    (matrix-requests:put-rooms/roomid/state/eventtype/statekey
     cl-matrix:*account*
     target-id
     +state-type+
     group-name
     (jsown:to-json
      (jsown:extend-js existing-state
        ("control_room" control-id))))))

(defun add-targets-to-control (group-name control &rest targets)
  "edits the room state of the control room and adds the targets to the group"
  (declare (type string group-name control))
  (let* ((existing-state (cl-matrix:room-state control +state-type+ group-name))
         (new-targets (append targets
                             (and (jsown:keyp existing-state "target_rooms")
                                  (jsown:val existing-state "target_rooms")))))
    (matrix-requests:put-rooms/roomid/state/eventtype/statekey
     cl-matrix:*account* control +state-type+ group-name
     (jsown:to-json
      (jsown:extend-js existing-state
        ("target_rooms" new-targets))))))

;;; make the group-name, control-id ignorable
(defmacro define-target-step (name (target-room control-room group-name &rest args) (&rest filter) &body body)
  "create a step that will assert that the target-room is controlled by control-room before executing the body
control-room and group-name are declared to be ignorable."
  `(define-room-step ,name (,target-room ,control-room ,group-name ,@args) (,filter)
     (declare (ignorable ,control-room ,group-name))
     ;; remove declerations from body and put them here (kinda eh but it'll do)
     ,@(prog1 (remove-if-not (lambda (l) (eql l 'declare)) body :key #'car)
        (setf body (delete-if (lambda (l) (eql l 'declare)) body :key #'car)))

     (unless (control-present-in-target-p ,group-name ,control-room ,target-room)
       (error 'luna-error :description (format nil "~a is not a group that has ~a as a control_room in ~a. Look at the room state event for luna.group to find out why." ,group-name ,control-room ,target-room)))

     ,@body))
