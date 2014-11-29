
(in-package :fl)

;;//// start with module representing program and that drag stuff in from there

;;//// represent the program state itself (singletons and other stuff) with an object

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass ir-program ()
  ((name
     :reader get-name
     :initarg :name)
   (entry-points
     :reader get-entry-points)
   (root-module
     :reader get-root-module)))

;; -----------------------------------------------------------------------------
(defclass ir-program-entry-point ()
  ((name
     :reader get-name
     :initarg :name)))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun make-program (name)
  (make-instance 'ir-program
                 :name name))


