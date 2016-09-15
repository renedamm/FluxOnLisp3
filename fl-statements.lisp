
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass fl-expression ()
  ())

;; -----------------------------------------------------------------------------
(defclass fl-name-expression ()
  ((name
    :reader get-name
    :initarg :name)))

;; -----------------------------------------------------------------------------
(defclass fl-statement ()
  ())

(defclass fl-return-statement (fl-statement)
  ((expression
    :initform nil
    :reader get-expression
    :initarg :expression)))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun fl-return-statement (expression)
  (make-instance 'fl-return-statement
                 :expression expression))

;; -----------------------------------------------------------------------------
(defun fl-name-expression (name)
  (make-instance 'fl-name-expression
                 :name (canonicalize name)))

