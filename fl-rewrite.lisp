
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
(defmethod rewrite ((ast ast-tuple-type))
  (let ((component-types (mapcar #'rewrite (get-component-types ast))))
    (fl-tuple-type-from-list-of-types component-types)))

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
               :type (rewrite (get-type ast)) ;;////TODO: need to capture names from tuples
               :ast ast
               :modifiers (rewrite-modifiers ast)
               :body (rewrite-body ast)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-field-definition))
  (let* ((modifiers (rewrite-modifiers ast))
         (name (identifier-to-string (get-identifier ast)))
         (body (get-value ast))
         ;;////TODO: need to check form of value parameters
         (object-type (rewrite (get-type (first (get-list (get-value-parameters ast))))))
         (value-type (rewrite (get-type ast)))
         (default-type (fl-function-type object-type value-type))
         (reader-type default-type)
         (writer-type (fl-function-type (fl-tuple-type object-type value-type) *nothing-type*))
         definitions)
    ;;////TODO: check which actually apply to the field
    (setf definitions (list (fl-function name :ast ast :modifiers (list 'default modifiers) :type default-type :body body) definitions))
    (setf definitions (list (fl-function name :ast ast :modifiers (list 'read modifiers) :type reader-type :body body) definitions))
    (setf definitions (list (fl-function name :ast ast :modifiers (list 'write modifiers) :type writer-type :body body) definitions))
    (values-list definitions)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-method-definition))
  (let* ((value-parameters (get-list (get-value-parameters ast)))
         (value-parameter-types
           (mapcar (lambda (parameter)
                     (rewrite (get-type parameter)))
                   value-parameters))
         (argument-type (fl-tuple-type-from-list-of-types value-parameter-types))
         (result-type (rewrite (get-type ast))))

    ;;////TODO: type parameters
    (fl-function (identifier-to-string (get-identifier ast))
                 :type (fl-function-type argument-type  result-type)
                 :ast ast
                 :modifiers (rewrite-modifiers ast)
                 :body (rewrite-body ast))))

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


