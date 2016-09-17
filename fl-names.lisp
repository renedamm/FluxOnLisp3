
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass fl-definition ()
  ((name
    :reader get-name
    :initarg :name)
   (modifiers
    :initform nil
    :reader get-modifiers
    :initarg :modifiers)
   (ast
    :initform nil
    :reader get-ast
    :initarg :ast)
   (is-import
    :initform nil
    :reader is-import-p
    :writer set-is-import
    :initarg :is-import)
   (is-abstract
    :initform nil
    :reader is-abstract-p
    :writer set-is-abstract
    :initarg :is-abstract)
   (is-private
    :initform nil
    :reader is-private-p
    :writer set-is-private
    :initarg :is-private)))

;; -----------------------------------------------------------------------------
(defclass fl-definition-with-body (fl-definition)
  ((body
    :initform nil
    :reader get-body
    :initarg :body)))

;; -----------------------------------------------------------------------------
(defclass fl-definition-with-type (fl-definition)
  ((type
    :initform nil
    :reader get-type
    :writer set-type
    :initarg :type)))

;; -----------------------------------------------------------------------------
(defclass fl-namespace (fl-definition-with-body)
  ())

;; -----------------------------------------------------------------------------
(defclass scope ()
  ((ast
    :reader get-ast
    :initarg :ast)
   (parent
    :reader get-parent
    :initarg :parent)
   (children
    :initform nil
    :reader get-children
    :writer set-children)
   (bindings
    :initform (make-hash-table :test #'equal)
    :reader get-bindings
    :documentation "Mapping of name to list of entities bound to the name")))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmethod initialize-instance :after ((definition fl-definition) &key)
  (let ((modifiers (get-modifiers definition)))
    (if (has-modifier-p 'import modifiers)
      (set-is-import t definition))
    (if (has-modifier-p 'private modifiers)
      (set-is-private t definition))
    (if (has-modifier-p 'abstract modifiers)
      (set-is-abstract t definition))))

;; -----------------------------------------------------------------------------
(defun fl-namespace (name &key ast body modifiers)
  (make-instance 'fl-namespace
                 :name (canonicalize name)
                 :ast ast
                 :modifiers modifiers
                 :body body))

;; -----------------------------------------------------------------------------
(defun bind (scope name entity)
  (let* ((canonical-name (canonicalize name))
         (bindings (get-bindings scope))
         (existing (gethash canonical-name bindings)))
    (setf (gethash canonical-name bindings) (cons entity existing))))

(deftest test-bind-adds-bindings-to-hashtable ()
  (let ((scope (make-instance 'scope)))
    (bind scope "Test" 1)
    (test-equal 1 (first (gethash "TEST" (get-bindings scope))))))

(deftest test-bind ()
  (test-bind-adds-bindings-to-hashtable))

;; -----------------------------------------------------------------------------
(defun lookup (scope name)
  (let* ((canonical-name (canonicalize name))
         (result (gethash canonical-name (get-bindings scope))))
    (if result
        result
        (let ((parent (get-parent scope)))
          (if parent
              (lookup parent name))))))

(deftest test-lookup-can-find-binding-directly-in-scope ()
  (let ((scope (make-instance 'scope)))
    (bind scope "Test" 1)
    (test-sequence-equal '(1) (lookup scope "Test"))))

(deftest test-lookup-can-find-binding-in-parent-scope ()
  (let* ((outer (make-instance 'scope))
         (inner (make-instance 'scope :parent outer)))
    (bind outer "Test" 1)
    (test-sequence-equal '(1) (lookup inner "Test"))))

(deftest test-lookup ()
  (test-lookup-can-find-binding-directly-in-scope)
  (test-lookup-can-find-binding-in-parent-scope))

;; -----------------------------------------------------------------------------
(defun canonicalize (name)
  (do ((result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
       (scanner (make-string-scanner name))
       (previous-char nil))
      ((is-at-end-p scanner)
       result)
    (let ((ch (read-next-token scanner)))
      (cond
        ((and (upper-case-p ch)
              previous-char
              (lower-case-p previous-char))
         (vector-push-extend #\_ result))
        ((and previous-char
              (upper-case-p ch)
              (upper-case-p previous-char)
              (not (is-at-end-p scanner))
              (lower-case-p (peek-next-token scanner)))
         (vector-push-extend #\_ result))
        ((eq #\- ch) (setf ch #\_)))
      (vector-push-extend (char-upcase ch) result)
      (setf previous-char ch))))

(deftest test-canonicalize ()
  (test-equal "TEST" (canonicalize "test"))
  (test-equal "TEST" (canonicalize "TEST"))
  (test-equal "TE_ST" (canonicalize "teST"))
  (test-equal "TEST_THIS" (canonicalize "testThis"))
  (test-equal "TEST_THIS" (canonicalize "test_this"))
  (test-equal "TEST_THIS" (canonicalize "test_THIS"))
  (test-equal "TEST_THIS" (canonicalize "test-this"))
  (test-equal "TEST_THIS" (canonicalize "TestThis"))
  (test-equal "TEST::THIS" (canonicalize "test::this"))
  (test-equal "TEST_THIS::TOO" (canonicalize "testThis::Too"))
  (test-equal "UI_PROGRAM" (canonicalize "UIProgram"))
  (test-equal "TEST__THIS" (canonicalize "test__this"))
  (test-equal "_TEST" (canonicalize "_test")))

;; -----------------------------------------------------------------------------
(defsuite test-names ()
  (test-bind)
  (test-lookup)
  (test-canonicalize))


