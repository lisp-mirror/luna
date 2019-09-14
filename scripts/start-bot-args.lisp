(defpackage #:luna.framework.config
  (:export
   #:luna.framework.config
   #:parse-args)
  (:use #:cl))

(in-package #:luna.framework.config)

(defvar *luna-config* nil "bind this to a dunno config")

(format t "!!WARNING THIS SCRIPT SHOULD ONLY BE USED IN A PRODUCTION ENVIRONMENT (and event then, it is not required to operate luna at all.)~%~%")

(defmacro with-muffled-stdout (&body body)
  `(with-open-stream (*standard-output* (make-broadcast-stream))
     ,@body))

(with-muffled-stdout
    (ql:register-local-projects)
  (ql:quickload :dunno))

(handler-case (with-muffled-stdout (ql:quickload :luna.framework))
        (error (c) (format t "Fatal: Failed to load luna.framework, something is very wrong.")
               (signal c)))

(dunno:define-config luna.framework.config ()
  ((:bot-name :dynamic-var luna.framework.hooks:*bot-name*
              :documentation "The name the bot will respond to e.g. !luna")

   (:username     :name username     :initform (error "MUST SUPPLY A USERNAME TO LUNA CONFIG")
                  :documentation "Required. The full username of the matrix account e.g. @luna:example.com")

   (:access-token :name access-token :initform nil
                  :documentation "The Access token for the username")

   (:password     :name password     :initform nil
                  :documentation "A password to obtain an access token if one is not provided.")

   (:sync-rate    :name sync-rate    :initform 2
                  :documentation "The rate at which the bot should poll sync.")

   (:log-template :name log-template :initform nil
                  :documentation "Template name for the logoutput e.g. luna.log")

   (:protocol     :name protocol     :initform "https://"
                  :documentation "The protocol to connect to the homeserver with, only required if you are trying to connect over http")

   (:port         :name port         :initform nil
                  :documentation "Port of the homeserver is serving clients from another port.")

   (:hostname     :name hostname     :initform nil
                  :documentation "The hostname to use to connect to the server.")))

(opts:define-opts
  (:name :help
         :description "Display this help message"
         :short #\h
         :long "help")

  (:name :config
         :description "A file to load for the config"
         :short #\c
         :long "config"
         :arg-parser #'identity
         :meta-var "CONFIG"
         ))

(defun show-help ()
  (opts:describe
   :prefix "helper script to start and run luna."
   :suffix "You should pass which modules you want the framework to load, and they will be loaded into the image before luna starts (so that their hooks, command parsers etc are available to the listener).
This script is not required to operate luna.

e.g. `start-bot.sh -c config.lisp luna` would load the luna module and start the bot."
   :usage-of "start-bot.sh"
   :args "[MODULES...]"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun get-opts-with-handling ()
  (handler-case (handler-bind ((opts:unknown-option #'unknown-option)) (opts:get-opts))
     (opts:missing-arg (c) (format t "~%Fatal: You must supply argument ~a." (opts:option c)) (opts:exit 1))
     (opts:arg-parser-failed (c) (format t "~%Fatal: cannot parse ~a as an argument of ~a" (opts:raw-arg c) (opts:option c))
                             (opts:exit 1))
     (opts:missing-required-option (c)
       (format t "Fatal : ~a~%" c) (opts:exit 1))))

(defun parse-args ()
  (multiple-value-bind (options free-args) (get-opts-with-handling)
    (when (getf options :help)
      (show-help) (opts:exit 0))

    (unless (getf options :config)
      (format t "Fatal: no --config provided.")
      (opts:exit 1))

    (load (getf options :config))

    (with-slots (username access-token password sync-rate log-template protocol port hostname) *luna-config*
      (unless (or password access-token)
        (format t "Fatal: must supply an access-token or password in the config.")
        (opts:exit 1))

      (let ((homeserver (format nil "~a~@[:~a~]"
                                (and username (or hostname (cl-matrix:get-hostname username)))
                                port)))
        (when (string= protocol "http://")
          (format t "~&WARNING: You are connecting over http"))

        (when log-template
          (v:define-pipe () (v:rotating-file-faucet :template log-template :interval :weekly)))

        (dolist (module free-args)
          (handler-case (with-muffled-stdout (ql:quickload module))
            (error (c) (format t "Fatal: failed to load module ~a" module) (signal c) (opts:exit 1))))

        (let ((account
               (if access-token
                   (cl-matrix:make-account username access-token :scheme protocol
                                           :homeserver homeserver)
                   (cl-matrix:login username (print password) :homeserver homeserver :scheme protocol))))
          (format t "~&starting listener from start-bot script~%")
          (luna.framework:make-luna-kernal)
          (cl-matrix:with-account (account) ; we're doing this because we're in no-inform.
            (funcall (luna.framework:make-listener (cl-matrix:now-token) sync-rate))))))))
