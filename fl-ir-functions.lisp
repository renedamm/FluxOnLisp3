
(in-package :fl)

;;//// when specializing, move the subtree to the specialization and make the generic reuse the tree of the specialization

;;//// every function takes a static function type argument (optional) and then a variable amount of value arguments
;;////    the type argument is omitted if the type is the same for all implementations in the function

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; A generic function.  Composed of any number of implementations.  By default,
;; every generic function has type "Object->Object" but specializations may be
;; generated that handle only a specific subset of the generic function's full
;; range of arguments.
(defclass ir-function ()
  ((name
     :reader get-name
     :initarg :name)
   (function-type
     :reader get-type)
   (implementations
     :reader get-implementations
     :initform nil
     :documentation "List of implementations available for the function.")
   (dispatch-tree
     :reader get-dispatch-tree
     :initform (make-instance 'ir-dispatch-tree))
   (local-data)
   (specializations)))

;; -----------------------------------------------------------------------------
(defclass ir-function-implementation ()
  ((instructions
     :reader get-instructions
     :initarg :body)
   (function-type
     :reader get-type
     :initarg :type)))

;; -----------------------------------------------------------------------------
;; A dispatch tree is a tree of decisions to make in order to find the
;; right implementation to execute for a given function call.
(defclass ir-dispatch-tree-node ()
  ((before-implementations
     :documentation "Code to run when entering this dispatch node.")
   (after-implementations
     :documentation "Code to run when exiting the dispatch node.")))

;; -----------------------------------------------------------------------------
(defclass ir-value-argument-typecheck-node (ir-dispatch-tree-node)
  ())

;; -----------------------------------------------------------------------------
(defclass ir-dispatch-tree ()
  ())

;; -----------------------------------------------------------------------------
(defclass ir-function-table ()
  ())

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun lookup-function (table name);;////REVIEW: really?
  (declare (ignore table name))
  ())

;; -----------------------------------------------------------------------------
;; Specializes the given function such that it is only composed of implementations
;; that can handle the given function type (which may be parameterized).
(defun specialize-function (function function-type)
  (declare (ignore function function-type))
  ())

;; -----------------------------------------------------------------------------
(defun find-implementations (function function-type)
  (declare (ignore function function-type))
  ())

;; -----------------------------------------------------------------------------
(defun has-implementation-p (function function-type)
  (declare (ignore function function-type))
  ())

