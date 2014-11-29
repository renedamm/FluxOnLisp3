
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass ir-type ()
  ((direct-derivations
     :reader get-direct-derivations
     :writer set-direct-derivations
     :initform nil
     :documentation "List of types derived directly derived from the type.")
   (direct-slots
     :reader get-direct-slots
     :writer set-direct-slots
     :initform nil
     :documentation "List of slots present on all objects that are subtypes of the type.")
   (parameters
     :reader get-parameters
     :writer set-parameters
     :initform nil)
   (singleton-instance
     :reader get-singleton-instance
     :writer set-singleton-instance
     :initform nil)))

;; -----------------------------------------------------------------------------
(defclass ir-derived-type (ir-type)
  ((base-type
     :reader get-base-type
     :initarg :base-type
     :documentation "Type that the type is derived type.")))

;; -----------------------------------------------------------------------------
(defclass ir-named-type (ir-derived-type)
  (name))

;; -----------------------------------------------------------------------------
(defclass ir-variable-type (ir-derived-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-instanced-type (ir-derived-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-dependent-type (ir-derived-type)
  ((constraint
     :reader get-constraint
     :initarg :constraint)))

;; -----------------------------------------------------------------------------
(defclass ir-alias-type (ir-derived-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-combination-type (ir-type)
  ((left-type
     :reader get-left-type
     :initarg :left-type)
   (right-type
     :reader get-right-type
     :initarg :right-type)))

;; -----------------------------------------------------------------------------
(defclass ir-function-type (ir-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-tuple-type (ir-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-union-type (ir-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-intersection-type (ir-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-predefined-type (ir-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-top-type (ir-predefined-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-bottom-type (ir-predefined-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-type-table ()
  ((all-types
     :reader get-all-types
     :initform nil);;(list *top-type* *bottom-type*))
   (named-types
     :reader get-named-types
     :initform (make-instance 'ir-binding-table))
   (exported-types)))

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *top-type* (make-instance 'ir-top-type))

;; -----------------------------------------------------------------------------
(defparameter *bottom-type* (make-instance 'ir-bottom-type))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defgeneric is-subtype-p (supertype subtype))

;; -----------------------------------------------------------------------------
(defgeneric is-supertype-p (subtype supertype))

;; -----------------------------------------------------------------------------
(defgeneric is-abstract-p (type))

;; -----------------------------------------------------------------------------
(defmethod is-subtype-p :around (supertype subtype)
  (if (eq supertype subtype)
    t
    (call-next-method)))

;; -----------------------------------------------------------------------------
(defmethod is-supertype-p :around (subtype supertype)
  (if (eq subtype supertype)
    t
    (call-next-method)))

;; -----------------------------------------------------------------------------
(defun derive-type (kind base-type)
  (declare (ignore kind base-type))
  ())

;; -----------------------------------------------------------------------------
;; Return the combination of the two given types using the specified kind
;; of type combination.
(defun combine-types (kind left-type right-type)
  (declare (ignore kind left-type right-type))
  ())

;; -----------------------------------------------------------------------------
(defsuite type-system-tests ()
  ())

