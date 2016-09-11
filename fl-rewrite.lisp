
(in-package :fl)

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-nothing-type))
  *nothing-type*)

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-function-type))
  (let ((left-type (rewrite (get-left-type ast)))
        (right-type (rewrite (get-right-type ast))))
    (fl-function-type left-type right-type)))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-function-definition))
  (fl-function (identifier-to-string (get-identifier ast))
               :type (rewrite (get-type ast))))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-program-definition))
  ;;////TODO: make sure we're at toplevel
  (fl-program (identifier-to-string (get-identifier ast))
              :body (mapcar #'rewrite (get-list (get-body ast)))))

;; -----------------------------------------------------------------------------
(defmethod rewrite ((ast ast-compilation-unit))
  (mapcar #'rewrite
    (get-list (get-definitions ast))))


