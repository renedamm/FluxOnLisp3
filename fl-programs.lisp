
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
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass fl-unit ()
  ((name
    :reader get-name
    :initarg :name)
   (imports
    :reader get-imports
    :initarg :imports);;////REVIEW:??
   (ast
    :reader get-ast
    :initarg :ast)
   (body
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
(defun fl-program (name &key body attributes)
  (let ((program (make-instance 'fl-program
                                :name name
                                :body body)))
    ;;////TODO: add to *programs*
    program))

(deftest test-can-create-empty-program ()
  (let ((program (fl-program "test")))
    (test (typep program 'fl-program))
    (test-equal "test" (get-name program))))

(deftest test-fl-program ()
  (test-can-create-empty-program))

;; -----------------------------------------------------------------------------
(defun collect-all-functions (unit)
  (remove-if-not #'fl-function-p (get-body unit)))

;; -----------------------------------------------------------------------------
(defun run (program &key verb path argument)
  (let ((*singletons* nil)
        (*stack* (make-instance 'callstack))
        (*dispatchers* (make-dispatcher-table)))

    ; Put all functions in dispatchers.
    (mapcar
      (lambda (function)
        (let ((dispatcher (find-or-add-dispatcher *dispatchers* (get-name function))))
          (add-function dispatcher function)))
      (collect-all-functions program))

    ; Find and call entry point.
    (let* ((entrypoint
            (if (not (get-entrypoints program))
              (if (or verb path)
                  (not-implemented "error: cannot find entry point")
                  (make-entrypoint 'GET "/main" :function-name "main")) ; Default entry point.
              (not-implemented "calling explicit entrypoints")))
           (dispatcher (find-dispatcher *dispatchers* (get-function-name entrypoint))))
      (if (not dispatcher)
          (not-implemented "error: no function implementing entry point")
          (dispatch dispatcher nil *object-nothing*))))) ;;////TODO: arguments

;;////FIXME: must be test-cannot-run-empty-program
;(deftest test-can-run-empty-program ()
  ;(test-equal *object-nothing* (run (fl-program "test"))))

(deftest test-can-run-program-with-empty-main-function ()
  (let* ((program (fl-program "test"
                              :body (list (fl-function "main"
                                                       :type (fl-function-type *nothing-type* *nothing-type*)))))
         (result (run program)))
   (test-equal *object-nothing* result)))

(deftest test-run ()
  (test-can-run-program-with-empty-main-function))

;; -----------------------------------------------------------------------------
(defsuite test-programs ()
  (test-fl-program)
  (test-run))

