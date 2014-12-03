
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass symbol-binding ()
  ((symbol
     :reader get-symbol
     :initarg :symbol)
   ;; List of definitions bound to the symbol.
   (definitions
     :reader get-definitions
     :writer set-definitions
     :initform nil)))

;; -----------------------------------------------------------------------------
(defclass symbol-scope ()
  ((bindings)
   (child-scopes)))

;; -----------------------------------------------------------------------------
(defclass symbol-table ()
  ())

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *translate-function-table* nil)

;; -----------------------------------------------------------------------------
(defparameter *translate-type-table* nil)

;; -----------------------------------------------------------------------------
;; ir-root for the current translation.
(defparameter *translate-root* nil)

;;;;============================================================================
;;;;    Macros.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmacro with-new-translation-state ((&key root) &body body)
  `(let ((*translate-root* ,root)
         (*translate-function-table* (make-instance 'symbol-table)))
     ,@body))

;; -----------------------------------------------------------------------------
(defmacro with-new-symbol-table-state (&body body)
  `(let ((*translate-function-table* (make-symbol-table))
         (*translate-type-table* (make-symbol-table)))
     ,@body))

;; -----------------------------------------------------------------------------
(defmacro test-translation (translate-function parser code &body checks)
  (with-gensyms (ast source code-value)
    `(with-new-parser-state ()
       (let* ((,code-value ,code)
              (,ast (funcall ,parser (make-string-scanner ,code-value)))
              (,source (make-source ,code-value)))
         (set-ast ,ast ,source)
         (with-new-translation-state (:root (make-root '(,source)))
           (funcall ,translate-function ,ast)
           ,@checks)))))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun make-symbol-table ()
  (make-instance 'symbol-table))

;; -----------------------------------------------------------------------------
(defun push-scope ()
  ())

;; -----------------------------------------------------------------------------
(defun pop-scope ()
  ())

;; -----------------------------------------------------------------------------
(defgeneric collect-definitions-from-ast (ast))

;; -----------------------------------------------------------------------------
(defmethod collect-definitions-from-ast ((ast ast-module-definition))
  (let* ((name-string (identifier-to-string (get-identifier ast)))
         (module (make-instance 'ir-module
                                :name name-string)))
    (pushnew module (get-modules *translate-root*))
    (with-new-symbol-table-state
      (collect-definitions-from-ast (get-body ast)))
    module))

(deftest test-collect-module-definitions-adds-module-instance ()
  (test-translation #'collect-definitions-from-ast
                    #'parse-compilation-unit
                    "module A {}"
    (test (not (eq nil (get-modules *translate-root*))))))

(deftest test-collect-module-definitions-sets-module-name ()
  (test-translation #'collect-definitions-from-ast
                    #'parse-compilation-unit
                    "module A::B::C {}"
    (test (some (lambda (definition)
                  (equal "A::B::C" (get-name definition)))
                (get-modules *translate-root*)))))

(deftest test-collect-module-definitions ()
  (test-collect-module-definitions-adds-module-instance)
  (test-collect-module-definitions-sets-module-name))

;; -----------------------------------------------------------------------------
(defmethod collect-definitions-from-ast ((ast ast-type-definition))
  (declare (ignore ast))
  ())

(deftest test-collect-type-definitions-simple ()
  (test-translation #'collect-definitions-from-ast
                    #'parse-definition
                    "type A;"))

(deftest test-collect-type-definitions ()
  (test-collect-type-definitions-simple))

;; -----------------------------------------------------------------------------
(defmethod collect-definitions-from-ast ((ast ast-function-definition))
  (declare (ignore ast))
  ())

;; -----------------------------------------------------------------------------
(defmethod collect-definitions-from-ast ((ast ast-list))
  (mapcar #'collect-definitions-from-ast (get-list ast)))
  
;; -----------------------------------------------------------------------------
(defmethod collect-definitions-from-ast ((ast ast-compilation-unit))
  (collect-definitions-from-ast (get-definitions ast)))

;; -----------------------------------------------------------------------------
(defun collect-definitions ()
  (dolist (source (get-sources *translate-root*))
    (collect-definitions-from-ast (get-ast source)))) 

;; -----------------------------------------------------------------------------
(defun generate-types ()
  ())

;; -----------------------------------------------------------------------------
(defun generate-functions ()
  ())

;; -----------------------------------------------------------------------------
(defun translate-sources (root)
  (with-new-translation-state (:root root)
    (collect-definitions)
    (generate-types)
    (generate-functions)))

;; -----------------------------------------------------------------------------
(defsuite test-translate ()
  (test-collect-type-definitions)
  (test-collect-module-definitions))

