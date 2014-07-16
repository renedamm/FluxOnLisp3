
(in-package :fl)

;;;;============================================================================
;;;;    Emitter State.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *emitter-package-name* nil)

;; -----------------------------------------------------------------------------
;; Stack of Lisp code blocks that we are currently assembling.
(defparameter *emitter-block-stack* nil)

;; -----------------------------------------------------------------------------
;; A block of emitted Lisp code.
(defclass emitter-block ()
  ((head
    :initform nil)
   (tail
    :initform nil)
   (name
    :initform nil)))

;;;;============================================================================
;;;;    Emitter Helper Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmacro with-new-emitter-state ((&key package-name) &body body)
  `(let ((*emitter-block-stack*
           (progn
             (let ((array (make-array 10 :adjustable t :fill-pointer 0 :element-type 'emitter-block)))
               (vector-push-extend (make-instance 'emitter-block) array)
               array)))
         (*emitter-package-name* (or ,package-name :flux-program)))
     ,@body))

;; -----------------------------------------------------------------------------
(defun get-current-emitter-block ()
  (elt *emitter-block-stack* (1- (length *emitter-block-stack*))))

(deftest test-get-current-emitter-block ()
  (with-new-emitter-state ()
    (test (get-current-emitter-block))))

;; -----------------------------------------------------------------------------
(defun get-current-emitter-block-head ()
  (slot-value (get-current-emitter-block) 'head))
  
;; -----------------------------------------------------------------------------
(defun get-current-emitter-block-tail ()
  (slot-value (get-current-emitter-block) 'tail))
  
;; -----------------------------------------------------------------------------
(defun set-current-block-name (name)
  (setf (slot-value (get-current-emitter-block) 'name) name))

;; -----------------------------------------------------------------------------
(defun get-current-block-name ()
  (slot-value (get-current-emitter-block) 'name))

;; -----------------------------------------------------------------------------
(defun get-emitted-code ()
  (slot-value (elt *emitter-block-stack* 0) 'head))

;; -----------------------------------------------------------------------------
(defun push-emitter-block (&key name)
  (let ((block (make-instance 'emitter-block)))
    (vector-push-extend block *emitter-block-stack*)
    (if name
        (set-current-block-name name))
    block))

;; -----------------------------------------------------------------------------
(defun pop-emitter-block ()
  (vector-pop *emitter-block-stack*))

;; -----------------------------------------------------------------------------
(defun append-code-to-current-emitter-block (list)
  (let* ((block (get-current-emitter-block))
         (head (slot-value block 'head)))
    (if (not head)
        (progn
          (setf (slot-value block 'head) list)
          (setf (slot-value block 'tail) (last list)))
        (progn
          (setf (cdr (slot-value block 'tail)) list)
          (setf (slot-value block 'tail) (last list)))))
  list)

(deftest test-append-code-to-current-emitter-block ()
  (with-new-emitter-state ()
    (let ((list1 (list 1 2 3))
          (list2 (list 4 5 6)))
      (append-code-to-current-emitter-block list1)
      (test (eq (get-current-emitter-block-head) list1))
      (test (eq (get-current-emitter-block-tail) (last list1)))

      (append-code-to-current-emitter-block list2)
      (test (eq (get-current-emitter-block-head) list1))
      (test (eq (get-current-emitter-block-tail) (last list2)))
      (test-equal (list 1 2 3 4 5 6) (get-current-emitter-block-head)))))

;; -----------------------------------------------------------------------------
(defmacro test-emitter (code parser declarations lambda-list &body checks)
  (with-gensyms (ast result)
    `(with-new-emitter-state ()
       (with-new-parser-state
         (let ((,ast (funcall ,parser (make-string-scanner ,code))))
           ,@(loop for decl in declarations
                   collect `(find-or-create-declaration (get-current-scope)
                                                        ,(car decl)
                                                        (make-identifier ',@(mapcar
                                                                              (lambda (id)
                                                                                (intern id :flux-program))
                                                                              (cdr decl)))))
           (let ((,result (emit ,ast)))
             ,(if (not (listp lambda-list))
                `(let ((,lambda-list ,result))
                   ,@checks)
                `(destructuring-bind ,lambda-list
                   ,result
                   ,@checks))))))))

;; -----------------------------------------------------------------------------
(defgeneric emit (ast))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-nothing-type))
  (intern "Nothing"));////TODO: use proper lookup

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-named-type))
  (let ((declaration (lookup-declaration *declaration-kind-type*
                                         (ast-type-name ast))))
    (if (not declaration)
      (not-implemented (format nil "declaration for ~a not found" (identifier-to-string (ast-type-name ast)))))
    ;;////TODO: mangled name should be stored in declaration rather than being generated over and over again
    (get-mangled-name declaration :in-package *emitter-package-name*)))

(deftest test-emit-named-type-simple ()
  (test-emitter
      "Foobar"
      #'parse-type
      ((*declaration-kind-type* "Foobar"))
      name
    (test-equal "Foobar" (symbol-name name))))

(deftest test-emit-named-type ()
  (test-emit-named-type-simple))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-literal-expression))
  (ast-literal-value ast))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-dot-expression))
  (let* ((value (emit (get-value-expression ast)))
         ;;////TODO: deal with lookup failure
         (declaration (lookup-declaration *declaration-kind-function*
                                          (get-member-name ast)))
         (function-name (get-mangled-name declaration :in-package *emitter-package-name*)))
    `(,function-name ,value)))

(deftest test-emit-dot-expression-simple ()
  (test-emitter
      "1.ToString"
      #'parse-expression
      ((*declaration-kind-function* "ToString"))
      (function-name argument)
    (test-equal "ToString" (symbol-name function-name))
    (test-equal 1 argument)))

(deftest test-emit-dot-expression ()
  (test-emit-dot-expression-simple))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-return-statement))
  (let* ((value-expression (ast-return-expression ast))
         (value (if value-expression
                  (emit value-expression)
                  nil))
         (code `(return-from ,(get-current-block-name) ,value)))
    (append-code-to-current-emitter-block code)))

(deftest test-emit-return-statement-with-simple-value ()
  (test-emitter
    "return 0;"
    #'parse-statement
    ()
    (operator name value)
    (declare (ignore operator name value))));////TODO

(deftest test-emit-return-statement ()
  (test-emit-return-statement-with-simple-value))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-type-definition))
  (let* ((declaration (ast-definition-declaration ast))
         (name (get-mangled-name declaration :in-package *emitter-package-name*))
         (object-type (get-object-type-declaration))
         (superclasses (if (not (ast-definition-type ast))
                         (if (not (eq declaration object-type))
                           (list (get-mangled-name object-type :in-package *emitter-package-name*))
                           ())
                         (list (emit (ast-definition-type ast)))))
         (class `(defclass ,name ,superclasses ())))
    (append-code-to-current-emitter-block (list class))
    (if (definition-abstract-p ast)
      (let ((error-message (format nil "Cannot instantiate abstract class ~a!"
                                   (identifier-to-string (ast-definition-name ast)))))
        (append-code-to-current-emitter-block (list `(defmethod make-instance :before ((instance ,name) &key)
                                                       (error ,error-message))))))
    class))

(deftest test-emit-type-definition-simple ()
  (test-emitter
      "type Foobar;"
      #'parse-definition
      ()
      (operator name (superclass) slots)
    (declare (ignore slots))
    (test-equal 'defclass operator)
    (test-equal "Foobar" (symbol-name name))
    (test-equal *config-object-type-name* (symbol-name superclass))))

(deftest test-emit-type-definition-single-supertype ()
  (test-emitter
      "type Foobar : Barfoo;"
      #'parse-definition
      ((*declaration-kind-type* "Barfoo"))
      (operator name (superclass) slots)
    (declare (ignore operator name slots))
    (test-equal "Barfoo" (symbol-name superclass))))

(deftest test-emit-type-definition ()
  (test-emit-type-definition-simple)
  (test-emit-type-definition-single-supertype))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-function-definition))
  (let* ((declaration (ast-definition-declaration ast))
         (name (get-mangled-name declaration :in-package *emitter-package-name*)))
    (with-scope (get-local-scope ast)
      (push-emitter-block :name name)
      (let* ((body-ast  (ast-definition-body ast))
             (body (if body-ast
                     (mapcan (lambda (statement) (emit statement)) (ast-list-nodes body-ast))
                     nil))
             (method `(defmethod ,name () ,body)))
        (pop-emitter-block)
        (append-code-to-current-emitter-block (list method))
        method))))

(deftest test-emit-function-definition-simple ()
  (test-emitter
      "method Foobar() {}"
      #'parse-definition
      ()
      (operator name () body)
    (declare (ignore body))
    (test-equal 'defmethod operator)
    (test-equal "Foobar" (symbol-name name))))

(deftest test-emit-function-definition ()
  (test-emit-function-definition-simple))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-field-definition))
  (not-implemented "emitting fields"))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-module-definition))
  (with-module-scope (get-local-scope ast)
    (let ((body (ast-definition-body ast)))
      (if body
        (mapcar (lambda (definition) (emit definition)) (ast-list-nodes body))))))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-compilation-unit))
  "Emit code for compilation unit."

  ;; Emit prologue.
  (let ((package-name *emitter-package-name*))
    (append-code-to-current-emitter-block
                  `((in-package :cl-user)
                    (defpackage ,package-name
                      (:use :common-lisp)
                      (:export :__main))
                    (in-package ,package-name))))

  ;; Emit definitions.
  (with-scope (get-local-scope ast)
    (mapc (lambda (definition) (emit definition))
          (ast-list-nodes (ast-unit-definitions ast))))

  (get-emitted-code))

(deftest test-emit-compilation-unit-prologue ()
  (test-emitter
      ""
      #'parse-compilation-unit
      ()
      (&rest code)
    (test (find `(in-package :flux-program) code :test #'equal))))

(deftest test-emit-compilation-unit ()
  (test-emit-compilation-unit-prologue))

;; -----------------------------------------------------------------------------
(defsuite test-emitters ()
  (test-get-current-emitter-block)
  (test-append-code-to-current-emitter-block)
  (test-emit-type-definition)
  (test-emit-function-definition)
  (test-emit-dot-expression)
  (test-emit-return-statement)
  (test-emit-compilation-unit))

