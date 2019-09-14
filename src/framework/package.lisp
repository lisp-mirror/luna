(defpackage luna.framework
  (:use #:cl)
  (:export
   ;; conditions.lisp
   #:luna-error
   #:luna-execution-error
   #:luna-parse-error
   #:luna-compile-error
   #:luna-permission-error
   #:log-trace-then-signal

   ;; step.lisp
   #:define-step
   #:get-step
   #:intern-step
   #:step-result

   #:*debug-execution*

   ;; report-syntax.lisp
   #:report

   ;; report.lisp
   #:define-reporter
   #:get-reporter
   #:intern-reporter
   #:defer-report
   #:step-condition
   #:default-reporter
   #:report-report

   #:with-defered-task
   #:with-reporting
   #:with-stream-to-report
   
   ;; room-ops.lisp
   #:has-power-p
   #:bot-powered-p
   #:can-send-state-p
   #:report-summary
   #:get-user-from-levels
   #:format-indent
   #:ensure-left-margin
   #:room-preview
   #:membership-change-p

   ;; command-parser.lisp
   #:transform-parser-designator
   #:get-parser
   #:intern-parser
   #:define-command-parser

   #:get-help
   #:get-synopsis
   #:list-parsers-with-synopsis

   ;; listener.lisp
   #:*channel*
   #:make-listener
   #:start-listening
   #:luna-lambda
   #:make-luna-kernal
   #:start-luna
   ))
