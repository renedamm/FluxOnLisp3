
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass fl-type ()
  ((parameters
    :initform nil
    :reader get-type-parameters)))

;; -----------------------------------------------------------------------------
(defclass fl-primitive-type (fl-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-top-type (fl-primitive-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-bottom-type (fl-primitive-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-named-type (fl-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-derived-type (fl-type)
  ((base-type
    :reader get-base-type
    :initarg :base)))

;; -----------------------------------------------------------------------------
(defclass fl-singleton-type (fl-derived-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-instanced-type (fl-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-dependent-type (fl-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-combination-type (fl-type)
  ((left-type
    :reader get-left-type
    :initarg :left)
   (right-right
    :reader get-right-type
    :initarg :right)))

;; -----------------------------------------------------------------------------
(defclass fl-union-type (fl-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-intersection-type (fl-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-function-type (fl-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-tuple-type (fl-combination-type)
  ())

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *top-type* (make-instance 'fl-top-type))

;; -----------------------------------------------------------------------------
(defparameter *bottom-type* (make-instance 'fl-bottom-type))

;; -----------------------------------------------------------------------------
(defparameter *nothing-type* (make-instance 'fl-singleton-type :base *top-type*))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun fl-derived-type (base-type)
  (make-instance 'fl-derived-type
                 :base base-type))

;; -----------------------------------------------------------------------------
(defun fl-function-type (argument-type result-type)
  (make-instance 'fl-function-type
                 :left argument-type
                 :right result-type))

;; -----------------------------------------------------------------------------
(defun subtype-p (super sub)
  (cond
    ((eq super sub) t)
    ((eq *top-type* super) t)
    ((eq *bottom-type* sub) t)
    ((and (typep super 'fl-function-type)
          (typep sub 'fl-function-type))
     (and (subtype-p (get-left-type sub) (get-left-type super))
          (subtype-p (get-right-type super) (get-right-type sub))))
    (nil nil)))

(deftest test-subtype-p-identity ()
  (let ((t1 (fl-derived-type *top-type*)))
    (test (subtype-p t1 t1))))

(deftest test-subtype-p-top-type ()
  (let ((t1 (fl-derived-type *top-type*)))
    (test (subtype-p *top-type* *top-type*))
    (test (subtype-p *top-type* t1))
    (test (not (subtype-p t1 *top-type*)))))

(deftest test-subtype-p-bottom-type ()
  (let ((t1 (fl-derived-type *top-type*)))
    (test (subtype-p *bottom-type* *bottom-type*))
    (test (subtype-p *top-type* *bottom-type*))
    (test (subtype-p t1 *bottom-type*))))

(deftest test-subtype-p-function-type ()
  (let ((object-to-object (fl-function-type *top-type* *top-type*))
        (nothing-to-nothing (fl-function-type *nothing-type* *nothing-type*))
        (nothing-to-anything (fl-function-type *nothing-type* *bottom-type*))
        (anything-to-nothing (fl-function-type *top-type* *nothing-type*)) ; Note how the meaning of 'anything' changes because of the contravariant position vs the covariant position above.
        (nothing-to-object (fl-function-type *nothing-type* *top-type*))
        (object-to-nothing (fl-function-type *top-type* *nothing-type*))
        (anything-to-anything (fl-function-type *top-type* *bottom-type*))
        (object-to-anything (fl-function-type *top-type* *bottom-type*)))
    (test (not (subtype-p object-to-object nothing-to-nothing)))
    (test (subtype-p object-to-object object-to-object))
    (test (subtype-p object-to-object object-to-nothing))))

(deftest test-subtype-p ()
  (test-subtype-p-identity)
  (test-subtype-p-top-type)
  (test-subtype-p-bottom-type)
  (test-subtype-p-function-type))

;; -----------------------------------------------------------------------------
;(defun supertype-p (super sub)
  ;(cond
    ;((eq super sub) t)
    ;((eq *top-type* super) t)
    ;((eq *bottom-type* sub) t)
    ;(nil nil)))

;; -----------------------------------------------------------------------------
(defsuite test-types ()
  (test-subtype-p))

