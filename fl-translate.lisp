
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
(defun push-scope ()
  ())

;; -----------------------------------------------------------------------------
(defun pop-scope ()
  ())

;; -----------------------------------------------------------------------------
(defun collect-module (ast)
  (declare (ignore ast))
  ())

;; -----------------------------------------------------------------------------
(defun collect-type (ast)
  (declare (ignore ast))
  ())

(deftest test-collect-type-simple ()
  (test-translation #'collect-type
                    #'parse-type
                    "type A;"))

(deftest test-collect-type ()
  (test-collect-type-simple))

;; -----------------------------------------------------------------------------
(defun collect-function (ast)
  (declare (ignore ast))
  ())

;; -----------------------------------------------------------------------------
(defun collect-definition (ast)
  (declare (ignore ast))
  ())

;; -----------------------------------------------------------------------------
(defun collect-modules (ast)
  (declare (ignore ast))
  ())

(deftest test-collect-modules-simple ()
  (test-translation #'collect-modules
                    #'parse-compilation-unit
                    "module A {}"))

(deftest test-collect-modules ()
  (test-collect-modules-simple))

;; -----------------------------------------------------------------------------
(defun collect-symbols ()
  (dolist (source (get-sources *translate-root*))
    (collect-modules (get-ast source)))) 

;; -----------------------------------------------------------------------------
(defun generate-types ()
  ())

;; -----------------------------------------------------------------------------
(defun generate-functions ()
  ())

;; -----------------------------------------------------------------------------
(defun translate-sources (root)
  (with-new-translation-state (:root root)
    (collect-symbols)
    (generate-types)
    (generate-functions)))

;; -----------------------------------------------------------------------------
(defsuite test-translate ()
  (test-collect-type)
  (test-collect-modules))

