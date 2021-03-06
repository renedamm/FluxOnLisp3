
(in-package :fl)

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *dispatchers* nil)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass fl-function (fl-definition-with-body)
  ((type
    :reader get-type
    :writer set-type
    :initarg :type)
   (run-type
    :initform 'inside  ;; 'inside, 'before, 'after, or 'around
    :reader get-run-type
    :initarg :run-type)))

;; -----------------------------------------------------------------------------
(defclass fl-variable (fl-definition)
  ((unique-key
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
(defun fl-function (name &key attributes modifiers type body ast)
  (let ((run-type (find-if #'run-type-modifier-p modifiers)))
    (if (not type)
      (not-implemented "error: no type supplied for function"))
    (make-instance 'fl-function
                   :name (canonicalize name)
                   :modifiers modifiers
                   :run-type run-type
                   :type type
                   :body body
                   :ast ast)))

(deftest test-fl-function-run-types ()
  (let ((type (fl-function-type *nothing-type* *nothing-type*)))
    (test-equal 'before (get-run-type (fl-function "test" :modifiers (list 'before) :type type)))
    (test-equal 'after (get-run-type (fl-function "test" :modifiers (list 'after) :type type)))
    (test-equal 'around (get-run-type (fl-function "test" :modifiers (list 'around) :type type)))))

(deftest test-fl-function ()
  (test-fl-function-run-types))

;; -----------------------------------------------------------------------------
(defun fl-function-p (value)
  (typep value 'fl-function))

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
(defsuite test-functions ()
  (test-fl-function)
  (test-collect-applicable-functions))

