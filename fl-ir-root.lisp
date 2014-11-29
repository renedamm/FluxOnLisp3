
(in-package :fl)

;;////REVIEW: allow IR to stick around for multiple runs; cache stuff like sources (with their ASTs)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; Root object of the IR.  Passed along from pass to pass.
(defclass ir-root ()
  ((sources
     :reader get-sources
     :writer set-sources
     :initarg :sources
     :initform nil
     :documentation "List of ir-sources.")
   (modules
     :reader get-modules
     :initform (make-hash-table :test #'equal))
   (programs
     :reader get-programs)))

;; -----------------------------------------------------------------------------
(defclass ir-settings ()
  ())

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun make-root (sources)
  (make-instance 'ir-root :sources sources))

;; -----------------------------------------------------------------------------
(defun add-module (root module)
  (declare (ignore root module))
  ())

;; -----------------------------------------------------------------------------
(defun add-program (root program)
  (declare (ignore root program))
  ())

