
(in-package :fl)

;; Doesn't do a whole lot other than desugaring. Right thing would be to generate
;; this stuff directly from the parser rather than an AST.

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmethod rewrite ((modifier ast-import-modifier))
  'import)

;; -----------------------------------------------------------------------------
(defmethod rewrite ((modifier ast-abstract-modifier))
  'abstract)

;; -----------------------------------------------------------------------------
(defun rewrite-modifiers (ast)
  (let ((modifiers (get-modifiers ast)))
    (if modifiers
      (mapcar #'rewrite (get-list modifiers)))))

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
(defmethod rewrite ((ast ast-name-expression))
  (fl-name-expression (identifier-to-string (get-identifier ast))))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-return-statement))
  (fl-return-statement (rewrite (get-return-expression ast))))

;; -----------------------------------------------------------------------------
(defun rewrite-body (ast)
  (let ((body (get-body ast)))
    (if body
      (do ((result nil)
           (rest (get-list body) (cdr rest)))
          ((not rest)
           result)
        (let ((defs (multiple-value-list (rewrite (car rest)))))
          (setf result (append defs result)))))))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-type-definition))
  (let ((type (get-type ast)))
    (fl-type (identifier-to-string (get-identifier ast))
      :ast ast
      :modifiers (rewrite-modifiers ast)
      :type (if type (rewrite type)))))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-object-definition))
  ;;////TODO: if inside namespace, need to take into account for name used for binding
  ;;////TODO: proper base type
  (fl-singleton (identifier-to-string (get-identifier ast))
    :ast ast
    :modifiers (rewrite-modifiers ast)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-function-definition))
  (fl-function (identifier-to-string (get-identifier ast))
               :type (rewrite (get-type ast))
               :ast ast
               :modifiers (rewrite-modifiers ast)
               :body (rewrite-body ast)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-features-definition))
  ;;////TODO: need to apply modifiers to definitions in body (like 'import' and 'private')
  (values-list (rewrite-body ast)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-namespace-definition))
  (fl-namespace (identifier-to-string (get-identifier ast))
                :ast ast
                :modifiers (rewrite-modifiers ast)
                :body (rewrite-body ast)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-module-definition))
  ;;////TODO: make sure we're at toplevel
  (fl-module (identifier-to-string (get-identifier ast))
              :ast ast
              :modifiers (rewrite-modifiers ast)
              :body (rewrite-body ast)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-library-definition))
  ;;////TODO: make sure we're at toplevel
  (fl-library (identifier-to-string (get-identifier ast))
              :ast ast
              :modifiers (rewrite-modifiers ast)
              :body (rewrite-body ast)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-program-definition))
  ;;////TODO: make sure we're at toplevel
  (fl-program (identifier-to-string (get-identifier ast))
              :ast ast
              :modifiers (rewrite-modifiers ast)
              :body (rewrite-body ast)))

;; -----------------------------------------------------------------------------
(defmethod rewrite (compilation-unit)
  (mapcar #'rewrite
    (get-list (get-definitions compilation-unit))))


