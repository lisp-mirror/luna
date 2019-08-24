#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(defpackage luna.hooks
  (:use #:cl #:method-hooks #:cl-matrix.base-events #:luna.framework #:luna #:luna.framework.hooks))

(in-package #:luna.hooks)

(defhook room-event soft-ban-join-hook ((account cl-matrix:account) room-id data)
         (declare (ignore account))
         (when (and (string= "m.room.member" (jsown:val data "type")) (luna.framework::membership-change-p data))
           (let ((membership (jsown:filter data "content" "membership")))
             (when (string= membership "join")
               (with-defered-task :soft-ban-join-hook
                 (let ((target-user (jsown:val data "sender")))
                   (with-reporting :check-soft-ban room-id target-user
                     (luna::check-soft-ban room-id target-user))))))))
