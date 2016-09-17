
(in-package :fl)

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *ast-global-qualifier-symbol* '|::|)

;; -----------------------------------------------------------------------------
(defparameter *ast-prefix-increment-operator* :prefix-increment)

;; -----------------------------------------------------------------------------
(defparameter *ast-postfix-increment-operator* :postfix-increment)

;; -----------------------------------------------------------------------------
(defparameter *ast-prefix-decrement-operator* :prefix-decrement)

;; -----------------------------------------------------------------------------
(defparameter *ast-postfix-decrement-operator* :postfix-decrement)

;; -----------------------------------------------------------------------------
(defparameter *ast-plus-operator* :plus)

;; -----------------------------------------------------------------------------
(defparameter *ast-minus-operator* :minus)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; Abstract base class for AST nodes.
(defclass ast-node ()
  ((region
     :reader get-source-region
     :initarg :source-region)))

;; -----------------------------------------------------------------------------
;; Node representing a parse error in the syntax tree.
(defclass ast-error (ast-node)
  ((diagnostic
     :reader get-diagnostic
     :initarg :diagnostic)))

;; -----------------------------------------------------------------------------
(defclass ast-list (ast-node)
  ((nodes
     :reader get-list
     :initarg :nodes
     :initform nil)))

;; -----------------------------------------------------------------------------
;; Representation of "::" denoting the global namespace.
(defclass ast-global-qualifier (ast-node)
  ())

;;////REVIEW: i think it makes more sense to make the qualifiers a class separate from identifiers where the list is in the order they need to be looked up
;; -----------------------------------------------------------------------------
;; A (potentially qualified) identifier.
(defclass ast-identifier (ast-node)
  ((qualifier
     :reader get-qualifier
     :initarg :qualifier
     :initform nil)
   (name
     :reader get-name
     :initarg :name)))

(defmethod initialize-instance :after ((id ast-identifier) &key)
  (let ((name (get-name id)))
    (if (not (symbolp name))
      (error (format nil "Expecting symbol for name of identifier but got '~a' instead." name)))))

;; -----------------------------------------------------------------------------
(defclass ast-modifier (ast-node)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-immutable-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-mutable-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-abstract-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-include-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-import-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-extend-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-read-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-write-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-default-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-final-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-sealed-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-instantiable-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-external-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-attribute (ast-node)
  ((name
    :reader get-identifier
    :initarg :name)
   (arguments
    :reader get-arguments
    :initarg :arguments)))

;; -----------------------------------------------------------------------------
(defclass ast-clause (ast-node)
  (expression
    :reader get-clause-expression
    :initarg :expression))

;; -----------------------------------------------------------------------------
(defclass ast-contract-clause (ast-clause)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-requires-clause (ast-contract-clause)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-ensures-clause (ast-contract-clause)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-invariant-clause (ast-contract-clause)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-when-clause (ast-clause)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-statement (ast-node)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-return-statement (ast-statement)
  ((expression
     :reader get-return-expression
     :initarg :expression)))

;; -----------------------------------------------------------------------------
(defclass ast-block-statement (ast-statement)
  ((statements
     :reader get-statements
     :initarg :statements)))

;; -----------------------------------------------------------------------------
(defclass ast-expression (ast-node)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-name-expression (ast-expression)
  ((name
     :reader get-identifier
     :initarg :name)))

;; -----------------------------------------------------------------------------
(defclass ast-literal-expression (ast-expression)
  ((value
     :reader get-literal-value
     :initarg :value)))

;; -----------------------------------------------------------------------------
(defclass ast-character-literal (ast-literal-expression)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-string-literal (ast-literal-expression)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-integer-literal (ast-literal-expression)
  ((type
     :reader get-integer-type
     :initarg :type
     :initform 'integer)))

;; -----------------------------------------------------------------------------
(defclass ast-float-literal (ast-literal-expression)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-operator-expression (ast-expression)
  ((operator
     :reader get-operator
     :initarg :operator
     :documentation "Keyword symbol for operator.")
   (operands
     :reader get-operands
     :initarg :operands
     :documentation "List of operand ASTs.")))

;; -----------------------------------------------------------------------------
(defclass ast-dot-expression (ast-expression)
  ((value
     :reader get-value-expression
     :initarg :value
     :documentation "Left side value expression AST.")
   (member
     :reader get-member-name
     :initarg :member
     :documentation "Right side ASTIdentifier denoting 'member' name.")))

;; -----------------------------------------------------------------------------
(defclass ast-call-expression (ast-expression)
  ((callee-expression
     :reader get-callee-expression
     :initarg :callee)
   (type-arguments
     :reader get-type-arguments
     :initarg :type-arguments)
   (value-arguments
     :reader get-value-arguments
     :initarg :value-arguments)))

;; -----------------------------------------------------------------------------
(defclass ast-type (ast-node)
  ((modifiers
     :reader get-modifiers
     :initarg :modifiers)))

;; -----------------------------------------------------------------------------
(defclass ast-special-type (ast-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-nothing-type (ast-special-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-named-type (ast-type)
  ((name
     :reader get-type-name
     :initarg :name)
   type-arguments))

;; -----------------------------------------------------------------------------
(defclass ast-combination-type (ast-type)
  ((left-type
     :reader get-left-type
     :initarg :left-type)
   (right-type
     :reader get-right-type
     :initarg :right-type)))

;; -----------------------------------------------------------------------------
(defclass ast-function-type (ast-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-union-type (ast-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-intersection-type (ast-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-definition (ast-node)
  ((attributes
     :reader get-attributes
     :initarg :attributes)
   (modifiers
     :reader get-modifiers
     :initarg :modifiers)
   (name
     :reader get-identifier
     :initarg :name)
   (type-parameters
     :reader get-type-parameters
     :initarg :type-parameters)
   (value-parameters
     :reader get-value-parameters
     :initarg :value-parameters)
   (clauses
     :reader get-clauses
     :initarg :clauses)
   (type
     :reader get-type
     :initarg :type)
   (value
     :reader get-value
     :initarg :value)
   (body
     :reader get-body
     :initarg :body)))

;; -----------------------------------------------------------------------------
(defclass ast-type-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-object-definition (ast-type-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-function-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-field-definition (ast-function-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-method-definition (ast-function-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-variable-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-parameter-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-value-parameter-definition (ast-parameter-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-type-parameter-definition (ast-parameter-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-features-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-namespace-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-unit-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-module-definition (ast-unit-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-library-definition (ast-unit-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-program-definition (ast-unit-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-compilation-unit (ast-node)
  ((definitions
     :reader get-definitions
     :initarg :definitions)))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmethod print-object ((object ast-identifier) stream)
  (print-unreadable-object (object stream :type t)
    (princ (identifier-to-string object) stream)))

;; -----------------------------------------------------------------------------
(defun make-ast-error (diagnostic)
  (make-instance 'ast-error
                 :source-region (get-diagnostic-source-region diagnostic)
                 :diagnostic diagnostic))

;; -----------------------------------------------------------------------------
(defun make-identifier (&rest names)
  (assert (>= (length names) 1))
  (loop
    for name in names
    for id = (if (eq name *ast-global-qualifier-symbol*)
               (make-instance 'ast-global-qualifier)
               (make-instance 'ast-identifier
                              :name name
                              :qualifier id))
    finally (return id)))

(deftest test-make-identifier-simple ()
  (let ((id (make-identifier 'test)))
    (test-equal "TEST" (symbol-name (get-name id)))
    (test-equal nil (get-qualifier id))))

(deftest test-make-identifier-with-global-qualifier ()
  (let ((id (make-identifier *ast-global-qualifier-symbol* 'outer 'inner 'test)))
    (test-equal "TEST" (symbol-name (get-name id)))
    (test-equal "INNER" (symbol-name (get-name (get-qualifier id))))
    (test-equal "OUTER" (symbol-name (get-name (get-qualifier (get-qualifier id)))))
    (test-type 'ast-global-qualifier (get-qualifier (get-qualifier (get-qualifier id))))))

(deftest test-make-identifier-with-namespace ()
  (let ((id (make-identifier 'outer 'inner)))
    (test-equal "INNER" (symbol-name (get-name id)))
    (test-type 'ast-identifier (get-qualifier id))
    (test-equal "OUTER" (symbol-name (get-name (get-qualifier id))))))

(deftest test-make-identifier ()
  (test-make-identifier-simple)
  (test-make-identifier-with-global-qualifier)
  (test-make-identifier-with-namespace))

;; -----------------------------------------------------------------------------
(defun is-globally-qualified-p (id)
  (let ((qualifier (get-qualifier id)))
    (cond
      ((not qualifier)
       nil)
      ((typep qualifier 'ast-global-qualifier)
       t)
      (t
       (is-globally-qualified-p qualifier)))))
 
;; -----------------------------------------------------------------------------
(defun identifier-to-string (id)
  (let ((qualifier (get-qualifier id)))
    (cond ((not qualifier)
           (symbol-name (get-name id)))
          ((typep qualifier 'ast-global-qualifier)
           (concatenate 'string "::" (symbol-name (get-name id))))
          (t
           (concatenate 'string (identifier-to-string qualifier) "::" (symbol-name (get-name id)))))))

(deftest test-identifier-to-string ()
  (let ((id1 (make-instance 'ast-identifier :name (intern "test"))))
    (test-equal "test" (identifier-to-string id1))))

;; -----------------------------------------------------------------------------
(defun definition-has-modifier-p (ast modifier-type)
  (let ((modifiers (get-modifiers ast)))
    (if (not modifiers)
        nil
        (find-if (lambda (modifier) (typep modifier modifier-type)) (get-list modifiers)))))

(deftest test-definition-has-modifier-p ()
  (let ((abstract-ast (make-instance 'ast-definition
                                     :modifiers (make-instance 'ast-list
                                                               :nodes (list (make-instance 'ast-abstract-modifier)))))
        (plain-ast (make-instance 'ast-definition
                                  :modifiers nil)))
    (test (definition-has-modifier-p abstract-ast 'ast-abstract-modifier))
    (test (not (definition-has-modifier-p plain-ast 'ast-abstract-modifier)))))

;; -----------------------------------------------------------------------------
(defun definition-abstract-p (ast)
  (definition-has-modifier-p ast 'ast-abstract-modifier))

;; -----------------------------------------------------------------------------
(defun definition-import-p (ast)
  (definition-has-modifier-p ast 'ast-import-modifier))

;; -----------------------------------------------------------------------------
(defsuite test-asts ()
  (test-make-identifier)
  (test-identifier-to-string)
  (test-definition-has-modifier-p))

