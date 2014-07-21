
(in-package :fl)

;;////REVIEW: simplify this and do away with namespace entirely (make all names fully qualified)
;;////NOTE: represent clashing definitions (like multiple definitions for the same variable) as multiple on same decl

;;;;============================================================================
;;;;    Symbol Tables.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; Identifiers for the different kinds of declarations.
(defparameter *declaration-kind-function* 'functions)
(defparameter *declaration-kind-variable* 'variables)
(defparameter *declaration-kind-module* 'modules)
(defparameter *declaration-kind-type* 'types)

;; -----------------------------------------------------------------------------
;; Current stack of scopes.
(defparameter *declaration-scope-stack* nil)

;; -----------------------------------------------------------------------------
(defparameter *declaration-current-module-scope* nil)

;; -----------------------------------------------------------------------------
(defparameter *declaration-current-toplevel-scope* nil)

;; -----------------------------------------------------------------------------
(defparameter *declaration-current-namespace* nil)

;; -----------------------------------------------------------------------------
(defparameter *declaration-for-object-type* nil)

;; -----------------------------------------------------------------------------
(defparameter *declaration-package-for-names* *config-default-output-package*)

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
(defmacro with-package-for-symbols (package-for-symbols &body body)
  (with-gensyms (package-for-symbols-value)
    `(let* ((,package-for-symbols-value ,package-for-symbols)
            (*declaration-package-for-names*
              (cond ((packagep ,package-for-symbols)
                     ,package-for-symbols-value)
                    (,package-for-symbols-value
                      (let ((package (find-package ,package-for-symbols-value)))
                        (if (not package)
                          (setf package
                                (make-package ,package-for-symbols-value :use :common-lisp)))
                        package))
                    (t
                     *config-default-output-package*))))
       ,@body)))

;; -----------------------------------------------------------------------------
(defmacro with-new-toplevel-scope (&body body)
  `(let* ((*declaration-scope-stack* (make-scope-stack))
          (*declaration-current-toplevel-scope* (push-scope)))
     ,@body))

;; -----------------------------------------------------------------------------
(defmacro with-toplevel-scope (scope &body body)
  `(progn
    (let ((*declaration-scope-stack* (make-scope-stack))
          (*declaration-current-toplevel-scope* ,scope))
       (push-scope *declaration-current-toplevel-scope*)
       ,@body)))

;; -----------------------------------------------------------------------------
(defmacro with-new-scope (&body body)
  `(unwind-protect
     (progn
       (push-scope)
       ,@body)
     (pop-scope)))

;; -----------------------------------------------------------------------------
(defmacro with-scope (scope &body body)
  (with-gensyms (scope-value)
    `(let ((,scope-value ,scope))
       (push-scope ,scope-value)
       (unwind-protect
         (progn
           ,@body)
         (pop-scope)))))

;; -----------------------------------------------------------------------------
(defmacro with-module-scope (scope &body body)
  (with-gensyms (scope-value)
    `(let* ((,scope-value ,scope)
            (*declaration-current-module-scope* ,scope-value))
       (with-scope ,scope-value ,@body))))

;; -----------------------------------------------------------------------------
(defun make-declaration-name (text)
  (assert *declaration-package-for-names*)
  (intern text *declaration-package-for-names*))
                  
;; -----------------------------------------------------------------------------
(defmethod get-qualified-name ((namespace namespace))
  (let ((parent (namespace-parent namespace)))
    (cond
      ((not parent)
       "")
      
      ((not (namespace-parent parent))
       (symbol-name (namespace-name namespace)))
      
      (t
       (concatenate 'string
                    (get-qualified-name parent)
                    *namespace-separator*
                    (symbol-name (namespace-name namespace)))))))

(deftest test-get-namespace-qualified-name ()
  (let* ((global (make-instance 'namespace))
         (outer (make-instance 'namespace :name 'outer :parent global))
         (inner (make-instance 'namespace :name 'inner :parent outer)))
    (test-equal "" (get-qualified-name global))
    (test-equal "OUTER" (get-qualified-name outer))
    (test-equal "OUTER::INNER" (get-qualified-name inner))))

;; -----------------------------------------------------------------------------
(defmethod get-qualified-name ((declaration declaration))
  (let ((namespace (get-qualified-name (declaration-namespace declaration)))
        (name (symbol-name (declaration-name declaration))))
    (if (zerop (length namespace))
      name
      (concatenate 'string namespace *namespace-separator* name))))

(deftest test-get-declaration-qualified-name ()
  (let* ((global (make-instance 'namespace))
         (namespace (make-instance 'namespace :name 'namespace :parent global))
         (declaration (make-instance 'declaration :name 'test :namespace namespace)))
    (test-equal "NAMESPACE::TEST" (get-qualified-name declaration))))

;; -----------------------------------------------------------------------------
(defun make-scope-stack ()
  (make-array 10 :element-type 'scope :adjustable t :fill-pointer 0))

;; -----------------------------------------------------------------------------
(defun push-scope (&optional scope)
  (assert *declaration-scope-stack*)
  (if (not scope)
    (setf scope (make-instance 'scope)))
  (vector-push-extend scope *declaration-scope-stack*)
  scope)

;; -----------------------------------------------------------------------------
(defun pop-scope ()
  (assert *declaration-scope-stack*)
  (assert (>= (length *declaration-scope-stack*) 1))
  (vector-pop *declaration-scope-stack*))

;; -----------------------------------------------------------------------------
(defun get-toplevel-scope ()
  (elt *declaration-scope-stack* 0))

;; -----------------------------------------------------------------------------
(defun get-current-scope (&key (index 0))
  (assert *declaration-scope-stack*)
  (let* ((scope-index (- (1- (length *declaration-scope-stack*)) index)))
    (assert (>= scope-index 0))
    (elt *declaration-scope-stack* scope-index)))

(deftest test-get-current-scope ()
  (with-new-toplevel-scope
    (let* ((outer-scope (make-instance 'scope))
           (inner-scope (make-instance 'scope)))
      (push-scope outer-scope)
      (push-scope inner-scope)
      (test-same inner-scope (get-current-scope))
      (test-same outer-scope (get-current-scope :index 1)))))

;; -----------------------------------------------------------------------------
(defun get-current-module-scope ()
  *declaration-current-module-scope*)

;; -----------------------------------------------------------------------------
(defun get-current-toplevel-scope ()
  *declaration-current-toplevel-scope*)

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
(defun get-special-declaration (declaration-kind name)
  (let ((scope (or (get-current-module-scope)
                   (get-current-toplevel-scope))))
    (assert scope)
    (let ((declaration (lookup-declaration declaration-kind
                                           (make-identifier *ast-global-qualifier-symbol* (make-declaration-name name))
                                           :in-scope scope)))
      ;; If the declaration does not exist yet, create an import for it.
      (if (not declaration)
        ;;////TODO: mark as import
        (setf declaration (find-or-create-declaration scope
                                                      declaration-kind
                                                      (make-identifier (make-declaration-name name)))))
      declaration)))

;; -----------------------------------------------------------------------------
;; Return the declaration for the Object type.
(defun get-object-type-declaration ()
  (when (not *declaration-for-object-type*)
    (setf *declaration-for-object-type* (get-special-declaration *declaration-kind-type* *config-object-type-name*)))
  *declaration-for-object-type*)
                        
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
;; Find the namespace inside 'parent-namespace' referenced by 'identifier'.
;; Ignores global qualification, i.e. 'parent-namespace' must be the correct
;; starting namespace.
(defun find-nested-child-namespace (parent-namespace identifier &key if-does-not-exist)
  (let ((qualifier (ast-id-qualifier identifier))
        (inner-namespace parent-namespace))
    (if (and qualifier
             (not (typep qualifier 'ast-global-qualifier)))
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
;;////FIXME: use *declaration-current-namespace*
(defun lookup-declaration (declaration-kind identifier &key current-namespace in-scope)
  (let* ((qualifier (ast-id-qualifier identifier))
         (name (ast-id-name identifier))
         (is-globally-qualified (is-globally-qualified-p identifier)))

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
                        ((and qualifier
                              (not (typep qualifier 'ast-global-qualifier)))
                          (find-nested-child-namespace starting-namespace qualifier))
                        (t
                          starting-namespace))))
               (if target-namespace
                 (find-declaration target-namespace name)))))

    (if in-scope
      (lookup-in-scope in-scope)
      (dotimes (scope-index (length *declaration-scope-stack*))
        (let* ((current-scope (get-current-scope :index scope-index))
               (declaration (lookup-in-scope current-scope)))
          (if declaration
            (return declaration))))))))

(deftest test-lookup-declaration-in-current-scope ()
  (with-new-toplevel-scope
    (let* ((id (make-identifier 'my-function))
           (declaration (find-or-create-declaration (get-current-scope) *declaration-kind-function* id)))
      (test-same declaration (lookup-declaration *declaration-kind-function* id)))))

(deftest test-lookup-declaration-globally-qualified ()
  (with-new-toplevel-scope
    (let* ((id (make-identifier *ast-global-qualifier-symbol* 'my-function))
           (declaration (find-or-create-declaration (get-current-scope)
                                                    *declaration-kind-function*
                                                    (make-identifier 'my-function))))
      (test-same declaration (lookup-declaration *declaration-kind-function* id)))))

(deftest test-lookup-declaration ()
  (test-lookup-declaration-in-current-scope)
  (test-lookup-declaration-globally-qualified))

;; -----------------------------------------------------------------------------
(defsuite test-symbol-table ()
  (test-get-current-scope)
  (test-get-namespace-qualified-name)
  (test-get-declaration-qualified-name)
  (test-find-or-insert-declaration)
  (test-create-child-namespace)
  (test-find-nested-child-namespace-returns-nil)
  (test-find-nested-child-namespace-create-if-does-not-exist)
  (test-get-namespace-for-declarations)
  (test-lookup-namespace-simple)
  (test-find-or-create-declaration)
  (test-lookup-declaration))

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
;;////TODO: move in-package to global state
(defun get-mangled-name (declaration &key in-package)
  (let ((qualified-name (get-qualified-name declaration)))
    (if in-package
      (intern qualified-name in-package)
      (intern qualified-name))))

(deftest test-get-mangled-name-simple ()
  (let* ((scope (make-instance 'scope))
         (namespace (get-namespace-for-declarations scope *declaration-kind-function*))
         (declaration (find-or-create-declaration scope *declaration-kind-function* (make-identifier 'test))))
    (declare (ignore namespace))
    (test-equal '|TEST| (get-mangled-name declaration))))

(defsuite test-get-mangled-name ()
  (test-get-mangled-name-simple))

