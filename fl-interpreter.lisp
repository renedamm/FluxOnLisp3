
(in-package :fl)

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *current-value-scope* nil)

;; -----------------------------------------------------------------------------
(defparameter *current-type-scope* nil)

;; -----------------------------------------------------------------------------
(defparameter *current-function-scope* nil)

;; -----------------------------------------------------------------------------
(defparameter *current-module-scope* nil)

;; -----------------------------------------------------------------------------
(defparameter *stack* nil)

;;;;============================================================================
;;;;    Macros.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmacro with-new-scope ((&key ast) &body body)
  `(let ((*current-type-scope* (make-instance 'scope
                                              :parent *current-type-scope*
                                              :ast ,ast)))
    ,@body))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

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
  (with-new-program-state ()
    (let* ((program (fl-program "test"
                                :body (list (fl-function "main"
                                                         :type (fl-function-type *nothing-type* *nothing-type*)))))
           (result (run program)))
     (test-equal *object-nothing* result))))

(deftest test-run ()
  (test-can-run-program-with-empty-main-function))

;; -----------------------------------------------------------------------------
(defsuite test-interpreter ()
  (test-run))

