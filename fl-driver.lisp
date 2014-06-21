
(in-package :fl)

;;;;============================================================================
;;;;    Entry Points.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; Parse one or more units of Flux code.  Returns a list of AST-COMPILATION-UNITs.
;; A unit of code can be represented as a string (parsed directly as Flux code), a pathname
;; (if pointing to a file, contents of file are parsed; if pointing to a directory, all Flux
;; source files in the directory and any of its subdirectories are parsed), a scanner (fed
;; directly into the parser), or a character stream (parsed as Flux code).
(defgeneric parse (code &key))

(defsuite test-parse ()
  (test-parse-code-source)
  (test-parse-code-string))

;; -----------------------------------------------------------------------------
(defmethod parse ((code source) &key package-for-symbols)
  (let* ((state (make-instance 'parser-state
                               :line-break-table (source-line-breaks code)
                               :diagnostics (source-diagnostics code)
                               :package-for-symbols (if package-for-symbols package-for-symbols *flux-default-package*)))
         (scanner (make-string-scanner (source-text code)))
         (result (parse-compilation-unit scanner state))
         (ast (parse-result-value result)))
    (setf (source-ast code) ast)
    (list ast)))

(deftest test-parse-code-source ()
  (let ((source (make-source "type Foobar;")))
    (destructuring-bind (ast)
        (parse source)
      (test (typep ast 'ast-compilation-unit)))))

;; -----------------------------------------------------------------------------
(defmethod parse ((code string) &key package-for-symbols)
  (parse (make-source code) :package-for-symbols package-for-symbols))

(deftest test-parse-code-string ()
  (destructuring-bind (ast)
      (parse "type Foobar;")
    (test (typep ast 'ast-compilation-unit))))

;; -----------------------------------------------------------------------------
(defmethod parse ((code pathname) &key package-for-symbols)
  (if (directory-pathname-p code)
      (not-implemented "Compiling entire directories")
      (parse (make-source code) :package-for-symbols package-for-symbols)))

;; -----------------------------------------------------------------------------
(defun translate (source &key package-name)
  (let ((emitter-state (make-instance 'emitter-state :package-name (if package-name package-name :flux-program))))
    (emit (source-ast source) emitter-state)))

;; -----------------------------------------------------------------------------
(defun flux-to-lisp (code &key package-name)
  "Parses one or more units of Flux code and then translates them to Lisp.  Returns \
the resulting Lisp expression."
  (let ((emitter-state (make-instance 'emitter-state :package-name (if package-name package-name :flux-program)))
        (asts (parse code)))
    ;////TODO: need to do a pre-pass to gather all types
    (mapc (lambda (ast) (emit ast emitter-state)) asts)
    (values
     (get-emitted-code emitter-state)
     asts)))
