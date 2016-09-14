
This directory contains an interpreter for Flux running on Lisp.

Note that it really works like an interpreter as it will not process and check the entire input in full
but rather work its way through wherever execution leads it. This also means that all name resolution happens
at runtime.

# Prequisites

Some Common Lisp. Been using Clozure Common Lisp with Lispbox 0.7.

# Running

   (require 'asdf)
   (setf asdf:*central-registry* (append asdf:*central-registry* (list #P"path/to/wherever/the/files/are/for/FluxOnLisp3/")))
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

