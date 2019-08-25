#! /bin/sh

sbcl --noinform --non-interactive --load start-bot-args.lisp --disable-debugger --quit --end-toplevel-options "$@"
