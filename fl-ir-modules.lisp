
(in-package :fl)

;;; One important thing to note is that Flux's module model makes it impossible to
;;; directly reference anything in another module.  The only way to use a module is
;;; to bring it into the current module -- either through importing (which will only
;;; use the signatures of the exports found in the other module) or through inclusion
;;; (which will bring the entire contents of the other module into the current one).

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass ir-module ()
  ((name
     :reader get-name
     :initarg :name)
   (references)
   (exports
     :reader get-exports)
   (imports
     :reader get-imports
     :documentation "List of imports (modules, types, functions).")
   (data
     :reader get-data)
   (types
     :reader get-types
     :documentation "List of types defined in the module."
     :initform nil)
   (functions
     :reader get-functions
     :documentation "List of functions defined the module."
     :initform nil)))

;; -----------------------------------------------------------------------------
(defclass ir-module-reference ()
  ())

;; -----------------------------------------------------------------------------
(defclass ir-module-export ()
  ())

;; -----------------------------------------------------------------------------
(defclass ir-module-import ()
  ())

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
