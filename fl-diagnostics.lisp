
(in-package :fl)

; 1xxxx are syntax errors
; 2xxxx are syntax warnings
; 3xxxx are semantic errors
; 4xxxx are semantic warnings
; 5xxxx are implementation-specific errors
; 6xxxx are implementation-specific warnings
; 7xxxx are implementation-specific infos

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass diagnostic-type ()
  ((code
     :reader get-diagnostic-code
     :initarg :code)
   (name
     :reader get-diagnostic-name
     :initarg :name)
   (format
     :reader get-diagnostic-format
     :initarg :format)))

;; -----------------------------------------------------------------------------
(defclass diagnostic ()
  ((type
     :reader get-diagnostic-type
     :initarg :type)
   (format-args
     :reader get-diagnostic-args
     :initarg :args)
   (source
     :reader get-diagnostic-source
     :initarg :source)
   (source-region
     :reader get-diagnostic-source-region
     :initarg :source-region)))

;; -----------------------------------------------------------------------------
(defclass diagnostic-collection ()
  ((diagnostics
     :reader get-diagnostics
     :initform (make-array 10 :adjustable t :fill-pointer 0))
   (error-count
     :reader get-error-count
     :initform 0)
   (warning-count
     :reader get-warning-count
     :initform 0)))

;;;;============================================================================
;;;;    Macros.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmacro defdiagnostic-type (code name format)
  (with-gensyms (instance)
  `(let ((,instance (make-instance 'diagnostic-type
                                   :code ,code
                                   :name ',name
                                   :format ,format)))
     (setf (gethash ,code *diagnostic-types-by-code*) ,instance)
     (setf (gethash ',name *diagnostic-types-by-name*) ,instance))))

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *diagnostic-types-by-name*
  (make-hash-table))

;; -----------------------------------------------------------------------------
(defparameter *diagnostic-types-by-code*
  (make-hash-table))

;; -----------------------------------------------------------------------------
(defdiagnostic-type 10001 block-not-closed "Expecting '}'")
(defdiagnostic-type 10002 list-not-closed "Expecting ')'")
(defdiagnostic-type 10003 separator-missing "Expecting ','")
(defdiagnostic-type 10004 name-missing "Expecting identifier")

;;;;============================================================================
;;;;    Functions
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun get-diagnostic-type-by-code (code)
  (gethash code *diagnostic-types-by-code*))

;; -----------------------------------------------------------------------------
(defun get-diagnostic-type-by-name (name)
  (gethash name *diagnostic-types-by-name*))

;; -----------------------------------------------------------------------------
(defun get-diagnostic-type-for-expecting (expecting)
  (cond ((equal expecting #\})
         (get-diagnostic-type-by-name 'block-not-closed))
        ((equal expecting #\))
         (get-diagnostic-type-by-name 'list-not-closed))
        ((equal expecting #\,)
         (get-diagnostic-type-by-name 'separator-missing))
        ((equal expecting 'parse-identifier)
         (get-diagnostic-type-by-name 'identifier-missing))
        ((symbolp expecting)
         (let ((code (get expecting :diagnostic-code))
               (name (get expecting :diagnostic-name))
               (type (get expecting :diagnostic-type)))
           (if type
               type
               (if code
                   (get-diagnostic-type-by-code code)
                   (if name
                       (get-diagnostic-type-by-name name)
                       (error (format nil "Neither :diagnostic-code nor :diagnostic-name nor :diagnostic-type is set for ~a" expecting)))))))
        (t
         (error (format nil "No diagnostic for expecting '~a'" expecting)))))

;; -----------------------------------------------------------------------------
(defun make-diagnostic (diagnostic-type &key source source-region)
  (cond ((numberp diagnostic-type)
         (setf diagnostic-type (get-diagnostic-type-by-code diagnostic-type)))
        ((stringp diagnostic-type)
         (setf diagnostic-type (get-diagnostic-type-by-name diagnostic-type))))
  (assert (typep diagnostic-type 'diagnostic-type))
  (make-instance 'diagnostic
                 :type diagnostic-type
                 :source source
                 :source-region source-region))

