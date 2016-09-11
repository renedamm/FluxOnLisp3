
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; All side-effects of program execution are performed through the host interface.
(defclass host ()
  ())

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun run-flux (code &key program verb path argument host)
  (with-new-program-state ()
    (with-new-parser-state ()
      (let* ((scanner (make-string-scanner code))
             (ast (parse-compilation-unit scanner)))
        (rewrite ast)
        (if (eq 0 (hash-table-count *programs*))
          (not-implemented "error: no programs found in code"))
        (let ((program-to-execute nil))
          (if (not program)
            (maphash (lambda (key value) (setf program-to-execute value)) *programs*)
            (not-implemented "choosing one out of multiple programs"))
          (run program-to-execute :verb verb :path path :argument argument))))))

