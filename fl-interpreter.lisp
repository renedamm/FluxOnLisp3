
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

;; -----------------------------------------------------------------------------
(defparameter *return-value* nil)

;; -----------------------------------------------------------------------------
(defparameter *current-namespace-prefix* nil)

;;;;============================================================================
;;;;    Macros.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmacro with-new-scope (() &body body)
  `(let ((*current-type-scope* (make-instance 'scope :parent *current-type-scope*))
         (*current-value-scope* (make-instance 'scope :parent *current-value-scope*)))
    ,@body))

;; -----------------------------------------------------------------------------
(defmacro with-new-namespace ((namespace) &body body)
  `(let* ((*current-namespace-prefix* (concatenate 'string *current-namespace-prefix* (get-name namespace) "::")))
    ,@body))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmethod interpret ((expression fl-name-expression))
  (let* ((results (lookup *current-value-scope* (get-name expression)))
         (num-results (length results)))
    (cond
      ((> 1 num-results) (not-implemented "error: overloaded value binding"))
      ((= 1 num-results) (first results))
      (t (not-implemented "error: cannot find value binding")))))

;; -----------------------------------------------------------------------------
(defmethod interpret ((statement fl-return-statement))
  (setf *return-value* (interpret (get-expression statement))))

;; -----------------------------------------------------------------------------
(defun call (function argument &key resend-list) ;;////REVIEW: what about type arguments?
  (with-new-scope ()
    (let ((body (get-body function)))
      (setf *return-value* *object-nothing*)
      (if body
          (mapcar #'interpret body))
      *return-value*)))

;; -----------------------------------------------------------------------------
;; Builds a list of functions selected from the dispatch tree that
;; are applicable to the given type and value argument, then sorts the
;; list according to precedence, and invokes the functions one by one
;; until ...
;;;; not quite right; need to take before, after, and around into account properly
(defun dispatch (dispatcher type-arg value-arg)
  (let* ((functions (collect-applicable-functions dispatcher type-arg value-arg))
         (num-functions (length functions)))
    (cond
      ((eq 0 num-functions) (not-implemented "error: no applicable functions for given argument"))
      ((> 1 num-functions) (not-implemented "actual dispatching..."))
      (t (call (first functions) value-arg)))))

;; -----------------------------------------------------------------------------
(defun qualified-name-of (definition)
  (concatenate 'string *current-namespace-prefix* (get-name definition)))

;; -----------------------------------------------------------------------------
(defmethod collect-definition ((singleton fl-singleton))
  (let ((qualified-name (qualified-name-of singleton)))
    (set-qualified-name qualified-name singleton)
    (bind *current-value-scope* qualified-name singleton)))

;; -----------------------------------------------------------------------------
(defmethod collect-definition ((function fl-function))
  (let ((dispatcher (find-or-add-dispatcher *dispatchers* (qualified-name-of function))))
    (add-function dispatcher function)))

;; -----------------------------------------------------------------------------
(defmethod collect-definition ((namespace fl-namespace))
  (with-new-namespace (namespace)
    (collection-definitions (get-body namespace))))

;; -----------------------------------------------------------------------------
(defun collection-definitions (list)
  (mapcar
    #'collect-definition
    list))

;; -----------------------------------------------------------------------------
(defun run (program &key verb path argument)
  (with-new-scope ()
    (let ((*stack* (make-instance 'callstack))
          (*dispatchers* (make-dispatcher-table)))

      (collection-definitions (get-body program))

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
            (dispatch dispatcher nil *object-nothing*)))))) ;;////TODO: arguments

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

