
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass fl-object ()
  ((type
    :reader get-type
    :initarg :type)
   (slots
    :initform (make-hash-table)
    :reader get-slots)))

(defclass fl-singleton (fl-object)
  ((name
    :reader get-name
    :initarg :name)
   (qualified-name
    :initform nil
    :reader get-qualified-name
    :writer set-qualified-name)))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun fl-object (&key type)
  (make-instance 'fl-object
                 :type type))

;; -----------------------------------------------------------------------------
(defun fl-singleton (&key name base-type)
  (make-instance 'fl-singleton
                 :type (fl-singleton-type (if base-type base-type *top-type*))
                 :name (canonicalize name)))

;; -----------------------------------------------------------------------------
(defun fl-singleton-p (value)
  (typep value 'fl-singleton))

;; -----------------------------------------------------------------------------
(defmethod typeof ((object fl-object))
  (get-type object))

;; -----------------------------------------------------------------------------
(defsuite test-objects ()
  ())

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; Object representing System::Optional::Nothing.
(defparameter *object-nothing* (fl-object :type *nothing-type*))

