
(in-package :fl)

;;;;============================================================================
;;;;    Diagnostics.
;;;;============================================================================

; 1xxxx are syntax errors
; 2xxxx are syntax warnings
; 3xxxx are semantic errors
; 4xxxx are semantic warnings
; 5xxxx are implementation-specific errors
; 6xxxx are implementation-specific warnings
; 7xxxx are implementation-specific infos

;; -----------------------------------------------------------------------------
(defparameter *diagnostic-types-by-name*
  (make-hash-table))

;; -----------------------------------------------------------------------------
(defparameter *diagnostic-types-by-code*
  (make-hash-table))

;; -----------------------------------------------------------------------------
(defclass diagnostic-type ()
  ((code
    :reader diagnostic-code
    :initarg :code)
   (name
    :reader diagnostic-name
    :initarg :name)
   (format
    :reader diagnostic-format
    :initarg :format)))

;; -----------------------------------------------------------------------------
(defclass diagnostic ()
  ((type
    :reader diagnostic-type
    :initarg :type)
   (format-args
    :reader diagnostic-args
    :initarg :args)
   (source
    :reader diagnostic-source
    :initarg :source)
   (source-region
    :reader diagnostic-source-region
    :initarg :source-region)
   (ast
    :reader diagnostic-arg
    :initarg :ast)))

;; -----------------------------------------------------------------------------
(defclass diagnostic-collection ()
  ((diagnostics
    :reader diagnostics
    :initform (make-array 10 :adjustable t :fill-pointer 0))
   (error-count
    :reader diagnostics-error-count
    :initform 0)
   (warning-count
    :reader diagnostics-warning-count
    :initform 0)))

;; -----------------------------------------------------------------------------
(defmacro defdiagnostic-type (code name format)
  (with-gensyms (instance)
  `(let ((,instance (make-instance 'diagnostic-type
                                   :code ,code
                                   :name ',name
                                   :format ,format)))
     (setf (gethash ,code *diagnostic-types-by-code*) ,instance)
     (setf (gethash ',name *diagnostic-types-by-name*) ,instance))))

;; -----------------------------------------------------------------------------
(defdiagnostic-type 10001 block-not-closed "Expecting '}'")
(defdiagnostic-type 10002 list-not-closed "Expecting ')'")
(defdiagnostic-type 10003 separator-missing "Expecting ','")
(defdiagnostic-type 10004 name-missing "Expecting identifier")

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
(defun make-diagnostic (diagnostic-type &key ast source source-region)
  (cond ((numberp diagnostic-type)
         (setf diagnostic-type (get-diagnostic-type-by-code diagnostic-type)))
        ((stringp diagnostic-type)
         (setf diagnostic-type (get-diagnostic-type-by-name diagnostic-type))))
  (if (and (not source-region) ast)
      (setf source-region (ast-source-region ast)))
  (assert (typep diagnostic-type 'diagnostic-type))
  (make-instance 'diagnostic
                 :type diagnostic-type
                 :source source
                 :source-region source-region))

