
(in-package :fl)

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *stack* nil)

;; -----------------------------------------------------------------------------
(defparameter *dispatchers* nil)

;; -----------------------------------------------------------------------------

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass fl-function ()
  ((name
    :reader get-name
    :initarg :name)
   (type
    :reader get-type
    :writer set-type
    :initarg :type)
   (modifiers
    :initform nil
    :reader get-modifiers
    :initarg :modifiers)
   (body
    :initform nil
    :reader get-body
    :initarg :body)
   (run-type
    :initform 'inside  ;; 'inside, 'before, 'after, or 'around
    :reader get-run-type
    :initarg :run-type)))

;; -----------------------------------------------------------------------------
(defclass fl-variable ()
  ((name
    :reader get-name
    :initarg name)
   (unique-key
    :reader get-unique-key
    :initarg :unique-key)))

;; -----------------------------------------------------------------------------
;; A collection of functions.
(defclass dispatcher ()
  ((function-name
    :reader get-function-name
    :initarg :function-name)
   (functions
    :initform nil
    :reader get-functions
    :writer set-functions)))

;; -----------------------------------------------------------------------------
(defclass stackframe ()
  ())

;; -----------------------------------------------------------------------------
(defclass callstack ()
  ())

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;;;;////TODO: canonicalize names everwhere

;; -----------------------------------------------------------------------------
(defun fl-function (name &key attributes modifiers type body)
  (let* ((run-type-modifiers (remove-if-not #'run-type-modifier-p (if (consp modifiers) modifiers (list modifiers))))
         (run-type (cond
                    ((not run-type-modifiers) 'inside)
                    ((> 1 (length run-type-modifiers)) (not-implemented "error: more than one run type modifier"))
                    (t (first run-type-modifiers)))))
    (if (not type)
      (not-implemented "error: no type supplied for function"))
    (make-instance 'fl-function
                   :name (canonicalize name)
                   :modifiers modifiers
                   :run-type run-type
                   :type type
                   :body body)))

(deftest test-fl-function-run-types ()
  (let ((type (fl-function-type *nothing-type* *nothing-type*)))
    (test-equal 'before (get-run-type (fl-function "test" :modifiers 'before :type type)))
    (test-equal 'after (get-run-type (fl-function "test" :modifiers 'after :type type)))
    (test-equal 'around (get-run-type (fl-function "test" :modifiers 'around :type type)))))

(deftest test-fl-function ()
  (test-fl-function-run-types))

;; -----------------------------------------------------------------------------
(defun fl-function-p (value)
  (typep value 'fl-function))

;; -----------------------------------------------------------------------------
(defun call (function argument &key resend-list) ;;////REVIEW: what about type arguments?
  (let ((body (get-body function)))
    ;;////TODO: logic should be that if last statement wasn't a return, implicitly do "return Nothing;"
    (if (not body)
        *object-nothing*
        (not-implemented "executing statements"))))

;; -----------------------------------------------------------------------------
(defun make-dispatcher (name)
  (make-instance 'dispatcher
                 :function-name (canonicalize name)))

;; -----------------------------------------------------------------------------
(defun add-function (dispatcher function)
  (set-functions (cons function (get-functions dispatcher)) dispatcher))

;; -----------------------------------------------------------------------------
(defun make-dispatcher-table ()
  (make-hash-table :test #'equal))

;; -----------------------------------------------------------------------------
(defun find-dispatcher (dispatcher-table name)
  (gethash name dispatcher-table))

;; -----------------------------------------------------------------------------
(defun find-or-add-dispatcher (dispatcher-table name)
  (let ((existing (gethash name dispatcher-table)))
    (if existing
        existing
        (let ((new-dispatcher (make-dispatcher name)))
          (setf (gethash name dispatcher-table) new-dispatcher)
          new-dispatcher))))

;; -----------------------------------------------------------------------------
(defun collect-applicable-functions (dispatcher type-arg value-arg)
  (let* ((value-type (typeof value-arg))
         (function-supertype (fl-function-type value-type *top-type*)))
    (if type-arg
        (not-implemented "dispatching with type arguments")
        (remove-if-not
          (lambda (function)
            (subtype-p function-supertype (get-type function)))
          (get-functions dispatcher)))))

(deftest test-collect-applicable-functions-with-same-type ()
  (let ((dispatcher (make-dispatcher "test"))
        (function (fl-function "test" :type (fl-function-type *nothing-type* *nothing-type*))))
    (add-function dispatcher function)
    (test-sequence-equal (list function) (collect-applicable-functions dispatcher nil *object-nothing*))))

(deftest test-collect-applicable-functions ()
  (test-collect-applicable-functions-with-same-type))

;; -----------------------------------------------------------------------------
(defun specialize (dispatch-tree type-arg value-arg)
  ())

;; -----------------------------------------------------------------------------
;; Builds a list of functions selected from the dispatch tree that
;; are applicable to the given type and value argument, then sorts the
;; list according to precedence, and invokes the functions one by one
;; until ...
;;;; not quite right; need to take before, after, and around into account properly
(defun dispatch (dispatcher type-arg value-arg)
  (let* ((functions (collect-applicable-functions dispatcher type-arg value-arg))
         (num-functions (length functions)))
    (cond
      ((eq 0 num-functions) (not-implemented "error: no applicable functions for given argument"))
      ((> 1 num-functions) (not-implemented "actual dispatching..."))
      (t (call (first functions) value-arg)))))

;; -----------------------------------------------------------------------------
(defsuite test-functions ()
  (test-fl-function)
  (test-collect-applicable-functions))
