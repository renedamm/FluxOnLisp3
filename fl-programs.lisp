
(in-package :fl)

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *programs* (make-hash-table :test #'equal))

;; -----------------------------------------------------------------------------
(defparameter *libraries* (make-hash-table :test #'equal))

;; -----------------------------------------------------------------------------
(defparameter *modules* (make-hash-table :test #'equal))

;;;;============================================================================
;;;;    Macros.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmacro with-new-program-state ((&key include-standard-libraries) &body body)
  `(let ((*programs* (make-hash-table :test #'equal))
         (*libraries* (make-hash-table :test #'equal))
         (*modules* (make-hash-table :test #'equal)))
     (if ,include-standard-libraries
       (load-standard-libraries))
     ,@body))

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass fl-import ()
  ())

;; -----------------------------------------------------------------------------
(defclass fl-unit ()
  ((name
    :reader get-name
    :initarg :name)
   (imports
    :reader get-imports
    :initarg :imports);;////REVIEW:??
   (ast
    :initform nil
    :reader get-ast
    :initarg :ast)
   (body
    :initform nil
    :reader get-body
    :initarg :body)))

;; -----------------------------------------------------------------------------
(defclass fl-program (fl-unit)
  ((entrypoints
   :initform nil
   :reader get-entrypoints
   :writer set-entrypoints)))

;; -----------------------------------------------------------------------------
(defclass fl-library (fl-unit)
  ())

;; -----------------------------------------------------------------------------
(defclass fl-module (fl-unit)
  ())

;; -----------------------------------------------------------------------------
(defclass entrypoint ()
  ((path
    :reader get-path
    :initarg :path)
   (verb
    :initform 'GET
    :reader get-verb
    :initarg :verb)
   (function-name
    :reader get-function-name
    :initarg :function-name)))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun make-entrypoint (verb path &key function-name)
  (make-instance 'entrypoint
                 :path path
                 :verb verb
                 :function-name (canonicalize function-name)))

;; -----------------------------------------------------------------------------
(defun fl-unit (name &key body attributes ast table unit-type)
  (let* ((canonical-name (canonicalize name))
         (unit (make-instance unit-type
                              :name canonical-name
                              :body body
                              :ast ast)))
    (if (gethash canonical-name table)
      (not-implemented "error: unit with same name already defined"))
    (setf (gethash canonical-name table) unit)
    unit))

;; -----------------------------------------------------------------------------
(defun fl-module (name &key body attributes ast)
  (fl-unit name :body body :attributes attributes :ast ast :table *modules* :unit-type 'fl-module))

;; -----------------------------------------------------------------------------
(defun fl-library (name &key body attributes ast)
  (fl-unit name :body body :attributes attributes :ast ast :table *libraries* :unit-type 'fl-library))

;; -----------------------------------------------------------------------------
(defun fl-program (name &key body attributes ast)
  (fl-unit name :body body :attributes attributes :ast ast :table *programs* :unit-type 'fl-program))

(deftest test-fl-program-adds-program-to-state ()
  (with-new-program-state ()
    (let ((program (fl-program "test")))
      (test-equal 1 (hash-table-count *programs*))
      (test-equal program (gethash (canonicalize "test") *programs*)))))

(deftest test-fl-program-can-create-empty-program ()
  (with-new-program-state ()
    (let ((program (fl-program "test")))
      (test (typep program 'fl-program))
      (test-equal (canonicalize "test") (get-name program)))))

(deftest test-fl-program ()
  (test-fl-program-adds-program-to-state)
  (test-fl-program-can-create-empty-program))

;; -----------------------------------------------------------------------------
(defsuite test-programs ()
  (test-fl-program))

