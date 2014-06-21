
(in-package :fl)

;;;;============================================================================
;;;;    Symbol Tables.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *declaration-kind-function* 'functions)
(defparameter *declaration-kind-variable* 'variables)
(defparameter *declaration-kind-module* 'modules)
(defparameter *declaration-kind-type* 'types)

;; -----------------------------------------------------------------------------
(defclass declaration ()
  ((namespace
     :reader declaration-namespace
     :initarg :namespace)
   (name
     :reader declaration-name
     :initarg :name)
   (mangled-name
     :accessor declaration-mangled-name)
   (definitions
     :reader declaration-definitions
     :initform nil)
   (kind
     :reader declaration-kind
     :initarg :kind)))

;; -----------------------------------------------------------------------------
(defclass scope ()
  ((modules
     :initform nil)
   (types
     :initform nil)
   (functions
     :initform nil)
   (variables
     :initform nil)))

;; -----------------------------------------------------------------------------
(defclass namespace ()
  ((name
     :reader namespace-name
     :initarg :name
     :initform nil)
   (parent-namespace
     :reader namespace-parent
     :initarg :parent
     :initform nil)
   (scope
     :reader namespace-scope
     :initarg :scope
     :initform nil)
   (children
     :reader namespace-children
     :initform nil)
   (declarations
     :reader namespace-declarations
     :initform nil)))

;; -----------------------------------------------------------------------------
(defmacro with-scope (state scope &body body)
  (with-gensyms (state-value scope-value)
    `(let ((,state-value ,state)
          (,scope-value ,scope))
      (push-scope (scope-stack ,state-value) ,scope-value)
      (unwind-protect
         (progn
           ,@body)
         (pop-scope (scope-stack ,state-value))))))

;; -----------------------------------------------------------------------------
(defparameter *namespace-separator* "::")

;; -----------------------------------------------------------------------------
(defun namespace-qualified-name (namespace)
  (let ((parent (namespace-parent namespace)))
    (cond
      ((not parent)
       "")
      
      ((not (namespace-parent parent))
       (symbol-name (namespace-name namespace)))
      
      (t
       (concatenate 'string
                    (namespace-qualified-name parent)
                    *namespace-separator*
                    (symbol-name (namespace-name namespace)))))))

(deftest test-namespace-qualified-name ()
  (let* ((global (make-instance 'namespace))
         (outer (make-instance 'namespace :name 'outer :parent global))
         (inner (make-instance 'namespace :name 'inner :parent outer)))
    (test-equal "" (namespace-qualified-name global))
    (test-equal "OUTER" (namespace-qualified-name outer))
    (test-equal "OUTER::INNER" (namespace-qualified-name inner))))

;; -----------------------------------------------------------------------------
(defun declaration-qualified-name (declaration)
  (let ((namespace (namespace-qualified-name (declaration-namespace declaration)))
        (name (symbol-name (declaration-name declaration))))
    (if (zerop (length namespace))
      name
      (concatenate 'string namespace *namespace-separator* name))))

(deftest test-declaration-qualified-name ()
  (let* ((global (make-instance 'namespace))
         (namespace (make-instance 'namespace :name 'namespace :parent global))
         (declaration (make-instance 'declaration :name 'test :namespace namespace)))
    (test-equal "NAMESPACE::TEST" (declaration-qualified-name declaration))))

;; -----------------------------------------------------------------------------
(defun make-scope-stack ()
  (make-array 10 :element-type 'scope :adjustable t :fill-pointer 0))

;; -----------------------------------------------------------------------------
(defun push-scope (scope-stack &optional scope)
  (if (not scope)
    (setf scope (make-instance 'scope)))
  (vector-push-extend scope scope-stack)
  scope)

;; -----------------------------------------------------------------------------
(defun pop-scope (scope-stack)
  (assert (>= (length scope-stack) 1))
  (vector-pop scope-stack))

;; -----------------------------------------------------------------------------
(defun current-scope (scope-stack &key (index 0))
  (let* ((scope-index (- (1- (length scope-stack)) index)))
    (assert (>= scope-index 0))
    (elt scope-stack scope-index)))

(deftest test-current-scope ()
  (let* ((stack (make-scope-stack))
         (global-scope (make-instance 'scope))
         (local-scope (make-instance 'scope)))
    (push-scope stack global-scope)
    (push-scope stack local-scope)
    (test-same local-scope (current-scope stack))
    (test-same global-scope (current-scope stack :index 1))))

;; -----------------------------------------------------------------------------
;; Adds 'definition' to the list of definitions for 'declarations'
;; and returns 'definition-ast'.
(defun add-definition (declaration definition-ast)
   (setf (slot-value declaration 'definitions)
         (cons definition-ast
               (slot-value declaration 'definitions)))
   (setf (ast-definition-declaration definition-ast) declaration)
   definition-ast)

;; -----------------------------------------------------------------------------
(defun find-declaration (namespace name)
  (let ((declarations (namespace-declarations namespace)))
    (if declarations
      (gethash name declarations)
      nil)))

;; -----------------------------------------------------------------------------
(defun find-or-insert-declaration (namespace name)
  (let* ((declarations (lazy-initialize-slot namespace 'declarations
                         (make-hash-table)))
         (declaration (gethash name declarations)))
    (if (not declaration)
      (setf declaration
            (setf (gethash name declarations)
                  (make-instance 'declaration
                                 :name name
                                 :namespace namespace))))
    declaration))

(deftest test-find-or-insert-declaration ()
  (let* ((namespace (make-instance 'namespace))
         (declaration1 (find-or-insert-declaration namespace 'test-function))
         (declaration2 (find-or-insert-declaration namespace 'test-function)))
    (test-type 'declaration declaration1)
    (test-type 'declaration declaration2)
    (test-same declaration1 declaration2)
    (test-equal declaration1 (find-declaration namespace 'test-function))))

;; -----------------------------------------------------------------------------
(defun create-child-namespace (parent-namespace name)
  (let ((child-namespace (make-instance 'namespace
                                        :name name
                                        :parent parent-namespace
                                        :scope (namespace-scope parent-namespace))))
    (setf (slot-value parent-namespace 'children)
          (cons child-namespace
                (slot-value parent-namespace 'children)))
    child-namespace))

(deftest test-create-child-namespace ()
  (let* ((parent (make-instance 'namespace
                                :name 'outer))
         (child (create-child-namespace parent 'inner)))
    (test-type 'namespace child)
    (test-equal 'inner (namespace-name child))
    (test-sequence-equal (list child) (namespace-children parent))))

;; -----------------------------------------------------------------------------
(defun find-child-namespace (parent-namespace name)
  (find-if (lambda (child-namespace)
             (equal (namespace-name child-namespace) name))
           (namespace-children parent-namespace)))

;; -----------------------------------------------------------------------------
(defun find-nested-child-namespace (parent-namespace identifier &key if-does-not-exist)
  (let ((qualifier (ast-id-qualifier identifier))
        (inner-namespace parent-namespace))
    (if qualifier
      (setf inner-namespace (find-nested-child-namespace parent-namespace
                                                         qualifier
                                                         :if-does-not-exist if-does-not-exist)))
    (if (not inner-namespace)
      nil
      (let* ((name (ast-id-name identifier))
             (child-namespace (find-child-namespace inner-namespace name)))
        (if child-namespace
          child-namespace
          (cond
            ((eq if-does-not-exist :create)
             (create-child-namespace inner-namespace name))
            (t nil)))))))

(deftest test-find-nested-child-namespace-returns-nil ()
  (let ((parent (make-instance 'namespace
                               :name 'outer)))
    (test-equal nil (find-nested-child-namespace parent
                                                 (make-instance 'ast-identifier
                                                                :name 'inner)))))

(deftest test-find-nested-child-namespace-create-if-does-not-exist ()
  (let* ((parent (make-instance 'namespace
                                :name 'outermost))
         (child (find-nested-child-namespace parent
                                             (make-instance 'ast-identifier
                                                            :name 'innermost
                                                            :qualifier (make-instance 'ast-identifier
                                                                                      :name 'inner))
                                             :if-does-not-exist :create)))
    (test-type 'namespace child)
    (test-equal 'innermost (namespace-name child))
    (test-equal 'inner (namespace-name (namespace-parent child)))
    (test-equal 'outermost (namespace-name (namespace-parent (namespace-parent child))))))

;; -----------------------------------------------------------------------------
(defun get-namespace-for-declarations (scope declaration-kind)
  (macrolet
    ((lazy-initialize (slot-name)
       `(lazy-initialize-slot scope ,slot-name
                             (make-instance 'namespace
                                             :scope scope))))
  (cond
    ((eq declaration-kind *declaration-kind-function*)
     (lazy-initialize 'functions))
    ((eq declaration-kind *declaration-kind-variable*)
     (lazy-initialize 'variables))
    ((eq declaration-kind *declaration-kind-module*)
     (lazy-initialize 'modules))
    ((eq declaration-kind *declaration-kind-type*)
     (lazy-initialize 'types))
    (t
     (error (format nil "Unknown declaration kind '~a'" declaration-kind))))))

(deftest test-get-namespace-for-declarations ()
  (let ((scope (make-instance 'scope)))
    (test-type 'namespace (get-namespace-for-declarations scope *declaration-kind-function*))
    (test-type 'namespace (get-namespace-for-declarations scope *declaration-kind-variable*))
    (test-type 'namespace (get-namespace-for-declarations scope *declaration-kind-module*))
    (test-type 'namespace (get-namespace-for-declarations scope *declaration-kind-type*))))

;; -----------------------------------------------------------------------------
(defun lookup-namespace (scope declaration-kind identifier &key if-does-not-exist)
  (find-nested-child-namespace (get-namespace-for-declarations scope declaration-kind)
                               identifier
                               :if-does-not-exist if-does-not-exist))

(deftest test-lookup-namespace-simple ()
  (let* ((scope (make-instance 'scope))
         (namespace (lookup-namespace scope
                                      *declaration-kind-function*
                                      (make-instance 'ast-identifier
                                                     :name 'inner
                                                     :qualifier (make-instance 'ast-identifier
                                                                               :name 'outer))
                                      :if-does-not-exist :create)))
    (test-type 'namespace namespace)
    (test-equal 'inner (namespace-name namespace))
    (test-equal (get-namespace-for-declarations scope *declaration-kind-function*)
                (namespace-parent (namespace-parent namespace)))))

;; -----------------------------------------------------------------------------
(defun find-or-create-declaration (scope declaration-kind identifier)
  (let* ((top-namespace (get-namespace-for-declarations scope declaration-kind))
         (qualifier (ast-id-qualifier identifier))
         (name (ast-id-name identifier))
         (namespace (if qualifier
                      (find-nested-child-namespace top-namespace qualifier :if-does-not-exist :create)
                      top-namespace)))
    (find-or-insert-declaration namespace name)))

(deftest test-find-or-create-declaration ()
  (let* ((scope (make-instance 'scope))
         (declaration (find-or-create-declaration scope *declaration-kind-function* (make-identifier 'my-function))))
    (test-type 'declaration declaration)
    (test-equal 'my-function (declaration-name declaration))))

;; -----------------------------------------------------------------------------
;;////REVIEW: lookup should go through a more generic interface than AST identifiers
(defun lookup-declaration (scope-stack declaration-kind identifier &key current-namespace)
  (let* ((qualifier (ast-id-qualifier identifier))
         (name (ast-id-name identifier))
         (is-globally-qualified (typep qualifier 'ast-global-qualifier)))

    (if is-globally-qualified
      (setf qualifier (ast-id-qualifier qualifier)))

    (flet ((lookup-in-scope (scope)
             (let* ((starting-namespace
                      (cond
                        (is-globally-qualified
                          (get-namespace-for-declarations scope declaration-kind))
                        ((and current-namespace
                              (eq (namespace-scope current-namespace) scope))
                         current-namespace)
                        (current-namespace
                          (not-implemented "finding corresponding namespace in different scope"))
                        (t
                          (get-namespace-for-declarations scope declaration-kind))))
                    (target-namespace
                      (cond
                        (qualifier
                          (find-nested-child-namespace starting-namespace qualifier))
                        (t
                          starting-namespace))))
               (if target-namespace
                 (find-declaration target-namespace name)))))

    (dotimes (scope-index (length scope-stack))
      (let* ((current-scope (current-scope scope-stack :index scope-index))
             (declaration (lookup-in-scope current-scope)))
        (if declaration
          (return declaration)))))))

(deftest test-lookup-declaration-in-current-scope ()
  (let* ((id (make-identifier 'my-function))
         (scope-stack (make-scope-stack))
         (scope (push-scope scope-stack))
         (declaration (find-or-create-declaration scope *declaration-kind-function* id)))
    (test-same declaration (lookup-declaration scope-stack *declaration-kind-function* id))))

;; -----------------------------------------------------------------------------
(defsuite test-symbol-table ()
  (test-namespace-qualified-name)
  (test-declaration-qualified-name)
  (test-find-or-insert-declaration)
  (test-create-child-namespace)
  (test-find-nested-child-namespace-returns-nil)
  (test-find-nested-child-namespace-create-if-does-not-exist)
  (test-get-namespace-for-declarations)
  (test-lookup-namespace-simple)
  (test-find-or-create-declaration)
  (test-lookup-declaration-in-current-scope))

;;;;============================================================================
;;;;    Naming Conventions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass naming-convention ()
  (prefix
   suffix
   first-character-case
   middle-character-case
   last-character-case))

;; -----------------------------------------------------------------------------
(defgeneric normalize-name (name convention))

;;;;============================================================================
;;;;    Name Mangling.
;;;;============================================================================

;; Once we have resolved a name to the declaration it refers to, we need to
;; emit a Lisp identifier that uniquely refers to that declaration.

;; -----------------------------------------------------------------------------
(defun mangled-name (declaration &key in-package)
  (let ((qualified-name (declaration-qualified-name declaration)))
    (if in-package
      (intern qualified-name in-package)
      (intern qualified-name))))

(deftest test-mangled-name-simple ()
  (let* ((scope (make-instance 'scope))
         (namespace (get-namespace-for-declarations scope *declaration-kind-function*))
         (declaration (find-or-create-declaration scope *declaration-kind-function* (make-identifier 'test))))
    (declare (ignore namespace))
    (test-equal '|TEST| (mangled-name declaration))))

(defsuite test-mangled-name ()
  (test-mangled-name-simple))

