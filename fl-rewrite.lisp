
(in-package :fl)

;; - Desugars

;; give classes initexpression slots with lambdas that are filled out by the rewriter and executed by the interpreter?

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-nothing-type))
  *nothing-type*)

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-named-type))
  (fl-named-type (identifier-to-string (get-type-name ast))
                 :ast ast))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-function-type))
  (let ((left-type (rewrite (get-left-type ast)))
        (right-type (rewrite (get-right-type ast))))
    (fl-function-type left-type right-type)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-object-definition))
  ;;////TODO: if inside namespace, need to take into account for name used for binding
  (let* ((type (fl-singleton-type *top-type*)) ;;////TODO: proper base type
         (singleton (fl-object :type type)))
    (set-singleton singleton type)
    type))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-function-definition))
  (fl-function (identifier-to-string (get-identifier ast))
               :type (rewrite (get-type ast))
               :ast ast))

;; -----------------------------------------------------------------------------
(defun rewrite-body (ast)
  (do ((result nil)
       (rest (get-list (get-body ast)) (cdr rest)))
      ((not rest)
        result)
    (let ((defs (multiple-value-list (rewrite (car rest)))))
      (setf result (append defs result)))))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-features-definition))
  (rewrite-body ast))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-namespace-definition))
  (fl-namespace (identifier-to-string (get-identifier ast))
                :ast ast
                :body (rewrite-body ast)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-program-definition))
  ;;////TODO: make sure we're at toplevel
  (fl-program (identifier-to-string (get-identifier ast))
              :body (rewrite-body ast)))

;; -----------------------------------------------------------------------------
(defmethod rewrite (compilation-unit)
  (mapcar #'rewrite
    (get-list (get-definitions compilation-unit))))


