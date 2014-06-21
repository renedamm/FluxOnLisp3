
(in-package :fl)

;;;;============================================================================
;;;;    Emitter.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; A block of emitted Lisp code.
(defclass emitter-block ()
  ((head
    :initform nil)
   (tail
    :initform nil)
   (name
    :initform nil)))

;; -----------------------------------------------------------------------------
(defclass emitter-state ()
  ((block-stack
     :reader emitter-blocks
     :initform (progn
                 (let ((array (make-array 10 :adjustable t :fill-pointer 0 :element-type 'emitter-block)))
                   (vector-push-extend (make-instance 'emitter-block) array)
                   array)))
   (package-name
     :reader emitter-package-name
     :initarg :package-name
     :initform :flux-program)
   (scope-stack
     :reader scope-stack
     :initform (make-scope-stack))
   (current-namespace
     :accessor current-namespace
     :initform nil)
   functions
   types))

;; -----------------------------------------------------------------------------
;;////TODO: this has to go
(defparameter *object-class-name* (intern "Object"))

;; -----------------------------------------------------------------------------
(defun current-emitter-block (state)
  (let ((block-stack (slot-value state 'block-stack)))
    (elt block-stack (1- (length block-stack)))))

(deftest test-current-emitter-block ()
  (test (current-emitter-block (make-instance 'emitter-state))))

;; -----------------------------------------------------------------------------
(defun current-emitter-block-head (state)
  (slot-value (current-emitter-block state) 'head))
  
;; -----------------------------------------------------------------------------
(defun current-emitter-block-tail (state)
  (slot-value (current-emitter-block state) 'tail))
  
;; -----------------------------------------------------------------------------
(defun set-current-block-name (state name)
  (setf (slot-value (current-emitter-block state) 'name) name))

;; -----------------------------------------------------------------------------
(defun get-current-block-name (state)
  (slot-value (current-emitter-block state) 'name))

;; -----------------------------------------------------------------------------
(defun get-emitted-code (state)
  (current-emitter-block-head state))

;; -----------------------------------------------------------------------------
(defun push-emitter-block (state &key name)
  (let ((block (make-instance 'emitter-block)))
    (vector-push-extend block (slot-value state 'block-stack))
    (if name
        (set-current-block-name state name))
    block))

;; -----------------------------------------------------------------------------
(defun pop-emitter-block (state)
  (vector-pop (slot-value state 'block-stack)))

;; -----------------------------------------------------------------------------
(defun append-code-to-current-emitter-block (state list)
  (let* ((block (current-emitter-block state))
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
  (let ((state (make-instance 'emitter-state))
        (list1 (list 1 2 3))
        (list2 (list 4 5 6)))
    (append-code-to-current-emitter-block state list1)
    (test (eq (current-emitter-block-head state) list1))
    (test (eq (current-emitter-block-tail state) (last list1)))

    (append-code-to-current-emitter-block state list2)
    (test (eq (current-emitter-block-head state) list1))
    (test (eq (current-emitter-block-tail state) (last list2)))
    (test-equal (list 1 2 3 4 5 6) (current-emitter-block-head state))))

;; -----------------------------------------------------------------------------
(defmacro test-emitter (code parser declarations lambda-list &body checks)
  (with-gensyms (ast parser-value state result)
    `(let* ((,parser-value ,parser)
            (,ast (funcall ,parser-value
                           (make-string-scanner ,code)
                           (make-instance 'parser-state)))
            (,state (let* ((state (make-instance 'emitter-state)))
                      (push-scope (scope-stack state))
                      ,@(loop for decl in declarations
                              collect `(find-or-create-declaration (current-scope (scope-stack state))
                                                                   ,(car decl)
                                                                   (make-identifier ',@(mapcar
                                                                                        (lambda (id)
                                                                                          (intern id :flux-program))
                                                                                        (cdr decl)))))
                      state))
            (,result (emit ,ast ,state)))
       ,(if (not (listp lambda-list))
            `(let ((,lambda-list ,result))
               ,@checks)
            `(destructuring-bind ,lambda-list
                 ,result
               ,@checks)))))

;; -----------------------------------------------------------------------------
(defgeneric emit (ast state))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-nothing-type) state)
  (declare (ignore state))
  (intern "Nothing"));////TODO: user proper lookup

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-named-type) state)
  (let ((declaration (lookup-declaration (scope-stack state)
                                         *declaration-kind-type*
                                         (ast-type-name ast)
                                         :current-namespace (current-namespace state))))
    (if (not declaration)
      (not-implemented (format nil "declaration for ~a not found" (identifier-to-string (ast-type-name ast)))))
    ;;////TODO: mangled name should be stored in declaration rather than being generated over and over again
    (mangled-name declaration :in-package (emitter-package-name state))))

(deftest test-emit-named-type-simple ()
  (test-emitter
      "Foobar"
      #'parse-type
      ((*declaration-kind-type* "Foobar"))
      name
    (test-equal "Foobar" (symbol-name name))))

(defsuite test-emit-named-type ()
  (test-emit-named-type-simple))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-integer-literal) state)
  (declare (ignore state))
  (ast-literal-value ast))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-return-statement) state)
  (let* ((value-expression (ast-return-expression ast))
         (value (if value-expression
                  (emit value-expression state)
                  nil))
         (code `(return-from ,(get-current-block-name state) ,value)))
    (append-code-to-current-emitter-block state code)))

(deftest test-emit-return-statement-with-simple-value ()
  (test-emitter
    "return 0;"
    #'parse-statement
    ()
    (operator name value)
    (declare (ignore operator name value))));////TODO

(defsuite test-emit-return-statement ()
  (test-emit-return-statement-with-simple-value))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-type-definition) state)
  (let* ((declaration (ast-definition-declaration ast))
         (name (mangled-name declaration :in-package (emitter-package-name state)))
         ;;////TODO: need to properly look up Object
         (superclasses (if (not (ast-definition-type ast))
                           (if (not (eq name *object-class-name*))
                               (list *object-class-name*)
                               ())
                           (list (emit (ast-definition-type ast) state))))
         (class `(defclass ,name ,superclasses ())))
    (append-code-to-current-emitter-block state (list class))
    (if (definition-abstract-p ast)
      (let ((error-message (format nil "Cannot instantiate abstract class ~a!"
                                   (identifier-to-string (ast-definition-name ast)))))
        (append-code-to-current-emitter-block state
                                              (list `(defmethod make-instance :before ((instance ,name) &key)
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
    (test-equal *object-class-name* superclass)))

(deftest test-emit-type-definition-single-supertype ()
  (test-emitter
      "type Foobar : Barfoo;"
      #'parse-definition
      ((*declaration-kind-type* "Barfoo"))
      (operator name (superclass) slots)
    (declare (ignore operator name slots))
    (test-equal "Barfoo" (symbol-name superclass))))

(defsuite test-emit-type-definition ()
  (test-emit-type-definition-simple)
  (test-emit-type-definition-single-supertype))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-function-definition) state)
  (let* ((declaration (ast-definition-declaration ast))
         (name (mangled-name declaration :in-package (emitter-package-name state))))
    (with-scope state (local-scope ast)
      (push-emitter-block state :name name)
      (let* ((body-ast  (ast-definition-body ast))
             (body (if body-ast
                     (mapcan (lambda (statement) (emit statement state)) (ast-list-nodes body-ast))
                     nil))
             (method `(defmethod ,name () ,body)))
        (pop-emitter-block state)
        (append-code-to-current-emitter-block state (list method))
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

(defsuite test-emit-function-definition ()
  (test-emit-function-definition-simple))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-field-definition) state)
  (declare (ignore state))
  (not-implemented "emitting fields"))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-module-definition) state)
  (with-scope state (local-scope ast)
    (let ((body (ast-definition-body ast)))
      (if body
        (mapcar (lambda (definition) (emit definition state)) (ast-list-nodes body))))))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-compilation-unit) state)
  "Emit code for compilation unit."

  ;; Emit prologue.
  (let ((package-name (emitter-package-name state)))
    (append-code-to-current-emitter-block state
                  `((in-package :cl-user)
                    (defpackage ,package-name
                      (:use :common-lisp)
                      (:export :__main))
                    (in-package ,package-name))))

  ;; Emit definitions.
  (with-scope state (local-scope ast)
    (mapc (lambda (definition) (emit definition state))
          (ast-list-nodes (ast-unit-definitions ast))))

  (get-emitted-code state))

(deftest test-emit-compilation-unit-prologue ()
  (test-emitter
      ""
      #'parse-compilation-unit
      ()
      (&rest code)
    (test (find `(in-package :flux-program) code :test #'equal))))

(defsuite test-emit-compilation-unit ()
  (test-emit-compilation-unit-prologue))

;; -----------------------------------------------------------------------------
(defsuite test-emitters ()
  (test-current-emitter-block)
  (test-append-code-to-current-emitter-block)
  (test-emit-type-definition)
  (test-emit-function-definition)
  (test-emit-compilation-unit))

