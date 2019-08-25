(defpackage #:luna.framework.shell-args
  (:use #:cl))

(in-package #:luna.framework.shell-args)

(defmacro with-muffled-stdout (&body body)
  `(with-open-stream (*standard-output* (make-broadcast-stream))
    ,@body))

(format t "!!WARNING THIS SCRIPT SHOULD ONLY BE USED IN A PRODUCTION ENVIRONMENT (and event then, it is not required to operate luna at all.)~%~%")
(ql:register-local-projects)

(handler-case (with-muffled-stdout (ql:quickload :luna.framework))
        (error (c) (format t "Fatal: Failed to load luna.framework, something is very wrong.")
               (signal c)))

(opts:define-opts
  (:name :user
         :description "Matrix user id for the bot"
         :short #\u
         :long "username"
         :meta-var "@<localpart>:<homeserver>"
         :arg-parser #'identity)

  (:name :help
         :description "Display this help message"
         :short #\h
         :long "help")

  (:name :access-token
         :description "An access token to the user id"
         :short #\a
         :long "access-token"
         :arg-parser #'identity
         :meta-var "ACCESS-TOKEN")

  (:name :password
         :description "Password for the matrix user if you don't have an access token to supply"
         :short #\p
         :long "password"
         :arg-parser #'identity
         :meta-var "PASSWORD")

  (:name :sync-rate
         :description "The rate to poll sync in seconds"
         :short #\s
         :long "sync-rate"
         :meta-var "FLOAT"
         :arg-parser #'parse-float:parse-float)

  (:name :log-output
         :description "A file to write the log to."
         :short #\l
         :long "log-output"
         :meta-var "FILE"
         :arg-parser #'identity)

  (:name :bot-name
         :description "The name of the bot responds to, default is `!luna`"
         :short #\n
         :long "bot-name"
         :arg-pass #'identity
         :meta-var "NAME")

  (:name :verbose-asdf
         :description "show asdf output."
         :long "verbose-asdf")

  (:name :protocol
         :description "the protocol to use when communicating to the server, https by default."
         :long "protocol"
         :arg-parser #'identity
         :meta-var "PROTOCOL")

  (:name :port
         :description "The port to use when communicating to the server"
         :long "port"
         :arg-parser #'identity
         :meta-var "PORT")

  (:name :hostname
         :description "The hostname to use when communicating to the server"
         :long "hostname"
         :arg-parser #'identity
         :meta-var "HOST"
         ))

(defun show-help ()
  (opts:describe
   :prefix "helper script to start and run luna."
   :suffix "You should pass what modules you want the framework to load (so if you wanted luna then just luna), and they will be loaded into the image before luna starts (so that their hooks, command parsers etc are available to the listener).
This script is not required to operate luna."
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
    (let ((user-id (getf options :user))
          (access-token (getf options :access-token))
          (password (getf options :password))
          (sync-rate (or (getf options :sync-rate) 2))
          (log-output (getf options :log-output))
          (verbose-asdf (getf options :verbose-asdf))
          (bot-name (getf options :bot-name))
          (scheme (or (and (getf options :protocol) (format nil "~a://" (getf options :protocol))) "https://")))
      (let ((homeserver (format nil "~a~@[:~a~]"
                                (and user-id (or (getf options :hostname) (cl-matrix:get-hostname user-id)))
                                (getf options :port))))
        
        (when (getf options :help)
          (show-help) (opts:exit 0))
        
        (unless user-id
          (format t "Fatal: Must supply a username.")
          (opts:exit 1))

        (unless (or access-token password)
          (format t "Fatal: Must supply either an access-token or a password.")
          (opts:exit 1))

        (when bot-name
          (setf luna.framework.hooks:*bot-name* bot-name))

        (when (string= scheme "http://")
          (format t "~&WARNING: You are connecting over http"))
        (when log-output
          (v:define-pipe () (v:rotating-file-faucet :template log-output :interval :weekly)))

        (dolist (module free-args)
          (handler-case (with-muffled-stdout (ql:quickload module :verbose verbose-asdf))
            (error (c) (format t "Fatal: failed to load module ~a" module) (signal c) (opts:exit 1))))

        (let ((account
               (if access-token
                   (cl-matrix:make-account user-id access-token :scheme scheme
                                           :homeserver homeserver)
                   (cl-matrix:login user-id password :scheme scheme
                                    :homeserver homeserver))))
          (format t "~&starting listener from start-bot script~%")
          (cl-matrix:with-account (account) ; we're doing this because we're in no-inform. 
            (funcall (luna.framework:make-listener (cl-matrix:now-token) sync-rate))))))))

(parse-args)
