#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(defpackage luna.framework.hooks
  (:use #:cl #:method-hooks #:cl-matrix.base-events #:luna.framework)
  (:export
   #:issue-luna-command
   #:*bot-name*))

(in-package #:luna.framework.hooks)

(defvar *bot-name* "!luna"
  "The name of the bot to respond to when giving commands e.g. !luna")

(defhook m.text issue-luna-command ((account cl-matrix:account) room-id event)
  (declare (ignore account))
  (cl-ppcre:register-groups-bind (name rest) ((format nil "^~a\\s+(\\S+)\\s*(.*)" *bot-name*)
                                              (jsown:filter event "content" "body"))
    (when name
      (luna-command name rest room-id event))))

(defun luna-command (name rest room-id event)
  (v:debug :command-listener "Got command in ~a:~a ~a" room-id name rest)
  (defer-report name rest room-id event))
