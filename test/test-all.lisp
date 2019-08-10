#| This file is part of luna
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com>|#

(in-package #:luna.test)

(define-test luna-test

)

(defun test-all (&key (report 'parachute:plain))
  (test 'luna-test :report report))
