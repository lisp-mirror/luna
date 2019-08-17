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

(defun get-group (room group-name)
  (let ((luna.group (cl-matrix:room-state room +state-type+ group-name)))
    (unless luna.group
      (error 'luna-error :description (format nil "~&there is no group named ~a for this room ~a." group-name room)))
    luna.group))

(defun mapgroup (function control-room group-name)
  "calls the function for each room-id in the group specified in the control-room with group-name
uses lparallel:pmapcar. "
  (let ((luna.group (get-group control-room group-name)))
    (let ((rooms (jsown:val luna.group "target_rooms")))
      (mapcar function rooms))))

(defun control-present-in-target-p (group-name control-id target-id)
  "true if the control room is present in the luna.group event"
  (declare (type string group-name control-id target-id))
  (let ((luna.group (cl-matrix:room-state target-id +state-type+ group-name)))
    (and luna.group
      (jsown:keyp luna.group "control_room")
      (string= control-id (jsown:val luna.group "control_room")))))

(defun target-present-in-control-p (group-name control-id target-id)
  "true if the target is present in the control room for the given group."
  (declare (type string group-name control-id target-id))
  (let ((luna.group (cl-matrix:room-state control-id +state-type+ group-name)))
    (and luna.group
      (jsown:keyp luna.group "target_rooms")
      (member target-id (jsown:val luna.group "target_rooms") :test #'string=))))

(defun add-control-to-target (group-name control-id target-id)
  "edits the room state of only the target room to add it to the group."
  (declare (type string group-name control-id target-id))
  (let ((existing-state (cl-matrix:room-state target-id +state-type+ group-name)))
    (cl-matrix.api.client:put-rooms/roomid/state/eventtype/statekey
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
         (new-targets (and (jsown:keyp existing-state "target_rooms")
                           (jsown:val existing-state "target_rooms"))))
    (dolist (target targets)
      (pushnew target new-targets :test #'string=))
    (cl-matrix.api.client:put-rooms/roomid/state/eventtype/statekey
     cl-matrix:*account* control +state-type+ group-name
     (jsown:to-json
      (jsown:extend-js existing-state
        ("target_rooms" new-targets))))))

(defmacro define-target-step (name (target-room control-room group-name &rest args) &body body)
  "create a step that will assert that the target-room is controlled by control-room before executing the body
control-room and group-name are declared to be ignorable."
  (multiple-value-bind (body declerations docstring) (alexandria:parse-body body)
    (push `(declare (ignorable ,control-room ,group-name)) declerations)
    `(define-step ,name (,target-room ,control-room ,group-name ,@args)
       ,@ (if docstring
              `(docstring ,@declerations)
              declerations)
          (unless (control-present-in-target-p ,group-name ,control-room ,target-room)
            (error 'luna-error :description (format nil "~a is not a group that has ~a as a control_room in ~a. Look at the room state event for luna.group to find out why." ,group-name ,control-room ,target-room)))

          ,@body)))
