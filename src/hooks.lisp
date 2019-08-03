#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(defpackage luna.hooks
  (:use #:cl #:method-hooks #:cl-matrix.base-events #:luna.framework))

(in-package #:luna.hooks)

(defhook m.text issue-luna-command ((account cl-matrix:account) room-id event)
  (declare (ignore account))
  (cl-ppcre:register-groups-bind (name rest) ("^\!luna\\s+(\\S*)\\s*(.*)" (jsown:filter event "content" "body"))
    (when name
      (luna-command name rest room-id event))))

(defun luna-command (name rest room-id event)
  (v:debug :command-listener "Got command in ~a:~a ~a" room-id name rest)
  ;; this needs hanlder-case around it.
  (defer-report *channel* name rest room-id event))
