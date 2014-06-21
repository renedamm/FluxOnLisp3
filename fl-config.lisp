
(in-package :fl)

;;;;============================================================================
;;;;    Configuration.
;;;;============================================================================

;; Valid extensions for Flux source files.
(defparameter *flux-file-extensions* '("flux"))

;; Location of regression test suite.
(defparameter *regression-suite-directory*
  #+darwin
  #P"/Users/rene/Dropbox/Workspaces/FluxOnLisp/RegressionTests"
  #-darwin
  #P"C:/Users/Rene Damm/Dropbox/Workspaces/FluxOnLisp/RegressionTests")

(defparameter *flux-default-package* (defpackage :flux-program
                                       (:use :common-lisp)))
