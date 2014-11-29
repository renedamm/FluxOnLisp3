
(in-package :fl)

;;////REVIEW: should the data model be more flexible?

;;; There's three locations for data: heap, stack, and static.
;;;
;;; Static data is packaged with a program.  It is a series of objects that are
;;; instantiated implicitly when the program is started.  There are no global
;;; variables; it's all objects that are retrieved through singletons.
;;;
;;; Heap data, too, is exclusively comprised of objects.  These objects can only
;;; be reached through either statics or stack data.  Statics will be converted
;;; into heap data on program startup.
;;;
;;; Stack data is exclusively comprised of variables and it is the only place
;;; where variables can exist.  Objects cannot exist directly on the stack (well,
;;; a low-level optimizer is free to convert heap allocation to stack allocation
;;; but this has to happen transparently).

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass ir-data ()
  ((data-type
     :reader get-data-type
     :initarg :data-type)))

