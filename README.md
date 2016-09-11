
This directory contains an interpreter for Flux running on Lisp.

# Prequisites

Some Common Lisp. Been using Clozure Common Lisp with Lispbox 0.7.

# Running

   (require 'asdf)
   (setq (append asdf:*central-registry* (list #P"path/to/wherever/the/files/are/for/FluxOnLisp3/")))
   (asdf:operate 'asdf:load-op 'fl) ; Will fail because I named the ASDF and file differently
   (asdf:operate 'asdf:load-op "FluxOnLisp")
   (in-package :fl)
   (run-unit-tests)

To pick up changes:

   (asdf:operate 'asdf:load-op "FluxOnLisp")

To fully reload in Slime:

   M-x slime-reload-system

# TODO

- Split into frontend and runtime

