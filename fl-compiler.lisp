
(in-package :fl)

;;////REVIEW: allow compiler to stick around for multiple runs; cache stuff like sources (with their ASTs)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;;////TODO: move
;; Representation of a compiled program.  Lists the entry points and contains
;; the compiled code.
(defclass compiled-program ()
  (name
    entry-points
    code))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;;////TODO: only parse sources that have changed or have not been parsed yet
(defun parse-source (source)
  (if (get-name source)
    (log:info "Parsing ~a" (get-name source)))
  (with-new-parser-state ()
    (let* ((scanner (make-string-scanner (get-text source)))
           (result (parse-compilation-unit scanner))
           (ast (parse-result-value result)))
      (set-ast ast source)
      ast)))

(deftest test-parse-source ()
  (let* ((source (make-source "module Test { type Foobar; }"))
         (result (parse-source source)))
    (test-type 'ast-compilation-unit (get-ast source))
    (test-same result (get-ast source))))

;; -----------------------------------------------------------------------------
;; Compiles one or more files and returns a list of the resulting COMPILED-PROGRAMs.
(defun compile-flux (inputs)
  (let* ((sources (collect-sources inputs))
         (ir (make-root sources)))
    ;;////TODO: create program
    ;; ---- Read Input ----
    ;; Parse sources.
    (mapc #'parse-source sources)
    ;;////TODO: diagnostics

    ;; ---- Translate ----
    (translate-sources ir)

    ;; ---- Optimize ----
    
    ;; --- Emit Code ----

    ))

;; -----------------------------------------------------------------------------
(defsuite test-compiler ()
  (test-parse-source))

