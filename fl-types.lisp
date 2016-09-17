
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass fl-type ()
  ((parameters
    :initform nil
    :reader get-type-parameters)
   (ast
    :initform nil
    :reader get-ast
    :initarg :ast)))

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
  ((name
    :reader get-name
    :initarg :name)
   (resolved-type
    :initform nil
    :reader get-resolved-type
    :writer set-resolved-type)))

;; -----------------------------------------------------------------------------
(defclass fl-derived-type (fl-type)
  ((base-type
    :reader get-base-type
    :initarg :base)))

;; -----------------------------------------------------------------------------
(defclass fl-singleton-type (fl-derived-type)
  ((singleton
    :initform nil
    :reader get-singleton
    :writer set-singleton)))

;; -----------------------------------------------------------------------------
(defclass fl-variable-type (fl-derived-type)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-instanced-type (fl-derived-type)
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

;; -----------------------------------------------------------------------------
(defclass fl-type-definition (fl-definition-with-type)
  ())

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *top-type* (make-instance 'fl-top-type))

;; -----------------------------------------------------------------------------
(defparameter *bottom-type* (make-instance 'fl-bottom-type))

;; -----------------------------------------------------------------------------
;;////REVIEW: this seems wrong... shouldn't this come from source?
(defparameter *nothing-type* (make-instance 'fl-singleton-type :base *top-type*))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun fl-type (name &key ast modifiers type)
  (make-instance 'fl-type-definition
                 :name (canonicalize name)
                 :ast ast 
                 :modifiers modifiers
                 :type type))

;; -----------------------------------------------------------------------------
(defun fl-named-type (name &key ast)
  (make-instance 'fl-named-type
                 :name (canonicalize name)
                 :ast ast))

;; -----------------------------------------------------------------------------
(defun fl-derived-type (base-type)
  (make-instance 'fl-derived-type
                 :base base-type))

;; -----------------------------------------------------------------------------
(defun fl-tuple-type (left-type right-type &key ast)
  (assert left-type)
  (assert right-type)
  (make-instance 'fl-tuple-type
                 :left left-type
                 :right right-type
                 :ast ast))

;; -----------------------------------------------------------------------------
(defun fl-function-type (argument-type result-type)
  (assert argument-type)
  (assert result-type)
  (make-instance 'fl-function-type
                 :left argument-type
                 :right result-type))

;; -----------------------------------------------------------------------------
(defun fl-singleton-type (base-type)
  (make-instance 'fl-singleton-type
                 :base base-type))

;; -----------------------------------------------------------------------------
(defun singleton-type-p (type)
  ;;////TODO: resolve type, if necessary
  (typep type 'fl-singleton-type))

;; -----------------------------------------------------------------------------
(defun subtype-p (super sub)
  ;;////TODO: resolve type, if necessary
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

