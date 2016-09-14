
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass fl-namespace ()
  ((name
    :reader get-name
    :initarg :name)
   (ast
    :initform nil
    :reader get-ast
    :initarg :ast)
   (body
    :reader get-body
    :initarg :body)))

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
(defun fl-namespace (name &key ast body)
  (make-instance 'fl-namespace
                 :name (canonicalize name)
                 :ast ast
                 :body body))

;; -----------------------------------------------------------------------------
(defun bind (scope name entity)
  (let* ((canonical-name (canonicalize name))
         (bindings (get-bindings scope))
         (existing (gethash canonical-name bindings)))
    (setf (gethash canonical-name bindings) (cons entity existing))))

;; -----------------------------------------------------------------------------
(defun lookup (scope name)
  (let ((canonical-name (canonicalize name)))
    (gethash canonical-name (get-bindings scope))))

(deftest test-lookup ()
  (let ((scope (make-instance 'scope)))
    (bind scope "Test" 1)
    (test-equal 1 (first (gethash "TEST" (get-bindings scope))))
    (test-sequence-equal '(1) (lookup scope "Test"))))

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
  (test-lookup)
  (test-canonicalize))


