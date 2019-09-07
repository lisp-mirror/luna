#!/bin/sh

exec sbcl --noinform --non-interactive --load start-bot-args.lisp --eval '(luna.framework.config:parse-args)' --disable-debugger --end-toplevel-options "$@"
