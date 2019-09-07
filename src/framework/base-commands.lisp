#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.framework)

(define-command-parser help (name rest room-id event)
  "[COMMAND]
displays the help message or the help for a command.

Will list all the known commands and related synopsis."
  (declare (ignore room-id event))
  (let ((command (car (cl-strings:split rest))))
    (with-output-to-string (s)
      (cond ((and (string-equal "help" name) (string= "" command))
             (write-string (list-parsers-with-synopsis) s))

            ((string= "" (get-help command))
             (format s "Can't find help for command ~a" command))

            (t (write-string (get-help command) s)))

      (format s "~%~%(λ () `(<a href=\"https://thufie.lain.haus/NPL.html\">🏴NON-VIOLENT PUBLIC LICENSE</a> . https://gitlab.com/Gnuxie/luna))"))))
