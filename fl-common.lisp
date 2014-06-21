
; Turn off optimizations to help debugging code.
(declaim (optimize (speed 0)
                   (safety 3)
                   (debug 3)))

(in-package :cl-user)

(defpackage :fl
  (:use :common-lisp))

