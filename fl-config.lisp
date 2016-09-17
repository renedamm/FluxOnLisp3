
(in-package :fl)

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; Valid extensions for source files.
(defparameter *config-source-file-extensions* '("flux"))

;; -----------------------------------------------------------------------------
;; Location of all library source files to be picked up by the interpreter.
(defparameter *config-library-directory*
  #+darwin
  #P"/Users/rene/Dropbox/Workspaces/FluxOnLisp3/Libraries/"
  #-darwin
  #P"C:/Dropbox/Workspaces/FluxOnLisp3/Libraries/")

;; -----------------------------------------------------------------------------
;; Location of regression test suite.
(defparameter *config-regression-suite-directory*
  #+darwin
  #P"/Users/rene/Dropbox/Workspaces/FluxOnLisp/RegressionTests"
  #-darwin
  #P"C:/Users/Rene Damm/Dropbox/Workspaces/FluxOnLisp/RegressionTests")

;; -----------------------------------------------------------------------------
(defparameter *config-default-output-package* (defpackage :flux-program
                                                (:use :common-lisp)))
