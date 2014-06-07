; This code is pretty much the opposite of idiomatic Lisp...

(in-package :cl-user)

(defpackage :flux
  (:use :common-lisp)
  (:export
   :run-tests))

(in-package :flux)

;;;;============================================================================
;;;;    Configuration.
;;;;============================================================================

;; Valid extensions for Flux source files.
(defparameter *flux-file-extensions* '("flux"))

;; Location of regression test suite.
(defparameter *regression-suite-directory*
  #+darwin
  #P"/Users/rene/Dropbox/Workspaces/FluxOnLisp/RegressionTests"
  #-darwin
  #P"C:/Users/Rene Damm/Dropbox/Workspaces/FluxOnLisp/RegressionTests")

;;;;============================================================================
;;;;    Test Framework.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;////TODO: turn this into a stack and print suites/tests as we enter them (instead of for each single test)
(defvar *test-name* nil)

;; -----------------------------------------------------------------------------
;; List of test suites.
(defparameter *test-suites* nil)

;; -----------------------------------------------------------------------------
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;; -----------------------------------------------------------------------------
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; -----------------------------------------------------------------------------
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

;; -----------------------------------------------------------------------------
(defmacro test (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

;; -----------------------------------------------------------------------------
(defmacro test-equal (expected actual)
  (with-gensyms (expected-value actual-value)
    `(let ((,expected-value ,expected)
           (,actual-value ,actual))
       (if (equal ,expected-value ,actual-value)
           (report-result t '(equal ,expected ,actual))
           (report-result nil (format nil "Expected ~a from ~a but got ~a" ,expected-value ',actual ,actual))))))

;; -----------------------------------------------------------------------------
(defmacro test-sequence-equal (expected actual)
  (with-gensyms (expected-value actual-value index left right length-expected length-actual)
    `(let* ((,expected-value ,expected)
            (,actual-value ,actual)
            (,length-expected (length ,expected-value))
            (,length-actual (length ,actual-value))
            (,left nil)
            (,right nil))
       (if
        (and
         ;; Check whether length matches.
         (if (not (equal ,length-expected ,length-actual))
             (report-result nil (format nil "Expected sequence of length ~a from ~a but got sequence of length ~a instead" ,length-expected ',actual ,length-actual))
             t)

         ;; Check whether elements match.
         (dotimes (,index (min ,length-expected ,length-actual) t)
           (setf ,left (elt ,expected-value ,index))
           (setf ,right (elt ,actual-value ,index))
           (if (not (equal ,left ,right))
               (progn
                 (report-result nil (format nil "Expected ~a from ~a but found ~a instead of ~a at index ~a" ,expected-value ',actual ,right ,left ,index))
                 (return nil)))))

        (report-result t '(equal ,expected ,actual))))))

;; -----------------------------------------------------------------------------
(defmacro test-type (expected-type expr)
  (with-gensyms (expected-type-value expr-value)
    `(let ((,expected-type-value ,expected-type)
           (,expr-value ,expr))
       (if (typep ,expr-value ,expected-type-value)
           (report-result t '(typep ,expr ,expected-type))
           (report-result nil (format nil "Expected value of type ~a but got ~a from ~a instead" ,expected-type ,expr-value ',expr))))))

;; -----------------------------------------------------------------------------
(defmacro with-test-name (name &body body)
  `(let ((*test-name* (append *test-name* (list ',name))))
     ,@body))

;; -----------------------------------------------------------------------------
(defmacro deftest-internal (name parameters &body body)
  `(defun ,name ,parameters
     (with-test-name ,name
       ,@body)))

;; -----------------------------------------------------------------------------
(defmacro deftest (name parameters &body body)
  `(deftest-internal ,name ,parameters
     ,@body))

;; -----------------------------------------------------------------------------
(defmacro defsuite (name parameters &body body)
  `(progn
     (deftest-internal ,name ,parameters
       (combine-results
         ,@body))
     (setf *test-suites* (cons #',name *test-suites*))))

;; -----------------------------------------------------------------------------
;////TODO: print test summary at end
(defun run-unit-tests (&key terminate-on-failure)
  (if terminate-on-failure
      (every #'funcall *test-suites*)
      (reduce (lambda (result suite) (and (funcall suite) result)) *test-suites* :initial-value t)))

;;;;============================================================================
;;;;    File Utilities.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

;; -----------------------------------------------------------------------------
(defun directory-pathname-p (pathname)
  (and
   (not (component-present-p (pathname-name pathname)))
   (not (component-present-p (pathname-type pathname)))
   pathname))

;; -----------------------------------------------------------------------------
(defun pathname-as-directory (namestring)
  (let ((pathname (pathname namestring)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p namestring))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
        pathname)))

;; -----------------------------------------------------------------------------
(defun pathname-as-file (namestring)
  (let ((pathname (pathname namestring)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p namestring)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory)
           :name (pathname-name name-and-type)
           :type (pathname-type name-and-type)
           :defaults pathname))
        pathname)))

;; -----------------------------------------------------------------------------
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

;; -----------------------------------------------------------------------------
#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

;; -----------------------------------------------------------------------------
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    
    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-as-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks allow clisp openmcl)
    (error "LIST-DIRECTORY not implemented for current platform")))

;; -----------------------------------------------------------------------------
(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))
  #-(or sbcl lispworks openmcl allegro cmu clisp)
  (error "FILE-EXISTS-P not implemented for current platform"))

;; -----------------------------------------------------------------------------
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

;; -----------------------------------------------------------------------------
(defun read-string (stream-or-pathname)
  (if (not (streamp stream-or-pathname))
      (with-open-file (file stream-or-pathname)
        (read-string file))
      (let* ((length-remaining (- (file-length stream-or-pathname)
                                  (file-position stream-or-pathname)))
             (data (make-string length-remaining)))
        (read-sequence data stream-or-pathname)
        data)))

;;;;============================================================================
;;;;    Utilities.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun not-implemented (&optional message)
  (if message
      (error (format nil "Not implemented: ~a" message))
      (error "Not implemented!")))

;; -----------------------------------------------------------------------------
(defun insert-into-array (vector value position)
  (assert (<= position (length vector)))
  (if (equal position (length vector))
      (vector-push-extend value vector)
      (progn
        (replace vector vector :start2 position :start1 (1+ position)
                 :end2 (vector-push-extend value vector))
        (setf (aref vector position) value)))
  vector)

(deftest test-insert-into-array ()
  (let ((vector (make-array 10 :adjustable t :fill-pointer 0)))
    (insert-into-array vector 10 0)
    (test (equal (length vector) 1))))

;; -----------------------------------------------------------------------------
(defun whitespace-char-p (char)
  (or (equal char #\Return)
      (equal char #\Tab)
      (equal char #\Newline)
      (equal char #\Space)))

;; -----------------------------------------------------------------------------
(defun digit-char-to-integer (char)
  (cond ((and (char>= char #\0)
              (char<= char #\9))
         (- (char-code char) (char-code #\0)))
        ((and (char>= char #\a)
              (char<= char #\f))
         (- (char-code char) (char-code #\a)))
        ((and (char>= char #\A)
              (char<= char #\F))
         (- (char-code char) (char-code #\A)))
        (t (error (format "Character ~@C is not a valid digit character" char)))))

(deftest test-digit-char-to-integer ()
  (test-equal 0 (digit-char-to-integer #\0))
  (test-equal 9 (digit-char-to-integer #\9))
  (test-equal 10 (digit-char-to-integer #\A))
  (test-equal 11 (digit-char-to-integer #\b))
  (test-equal 15 (digit-char-to-integer #\f)))

;; -----------------------------------------------------------------------------
(defsuite test-utilities ()
  (test-insert-into-array))

;;;;============================================================================
;;;;    Sources.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass source ()
  ((name
    :reader source-name
    :initarg :name)
   (path
    :reader source-path
    :initarg :path)
   (line-breaks
    :reader source-line-breaks
    :initarg :line-breaks
    :initform (make-line-break-table))
   (text
    :reader source-text
    :initarg :text)
   (diagnostics
    :reader source-diagnostics
    :initform (make-instance 'diagnostic-collection))
   (ast
    :accessor source-ast)))

;; -----------------------------------------------------------------------------
(defun make-source (code &key name path)
  (if (pathnamep code)
      (progn
        (if (not path)
            (setf path code))
        (setf code (read-string code))))
  (if (and (not name) path)
      (setf name (pathname-name path)))
  (make-instance 'source
                 :name name
                 :path path
                 :text code))

;; -----------------------------------------------------------------------------
(defun make-source-region (left right)
  (assert (<= left right))
  (cons left right))

;; -----------------------------------------------------------------------------
(defun source-region-empty (region)
  (= (car region)
     (cdr region)))

;; -----------------------------------------------------------------------------
(defun source-region-null (region)
  (and (zerop (car region))
       (zerop (cdr region))))

;; -----------------------------------------------------------------------------
(defun make-line-break-table ()
  "Create a new line break table.  A line break table records the character indices of new lines in a text."
  (let ((table (make-array 100 :fill-pointer 0 :adjustable t :element-type 'fixnum)))
    (vector-push-extend 0 table) ; First line has no explicit line break so always record a line break at index 0 for it.
    table))

(deftest test-make-line-break-table-adds-first-line ()
  (test
    (equal (line-count (make-line-break-table)) 1)))

;; -----------------------------------------------------------------------------
(defun line-count (table)
  (length table))

;; -----------------------------------------------------------------------------
(defun find-line-break-index (table position)
  "Return the index of the line break that corresponds to the line of the given position.
Note that a line break occurs *after* newlines, i.e. newline characters themselves still belong \
to the previous line."
  (assert (>= position 0))
  (let ((upper-limit (1- (length table)))
        (lower-limit 0))
    (if (> position (elt table upper-limit))
        upper-limit
        (loop
           (if (equal upper-limit lower-limit)
               (return lower-limit))
           (let* ((current-index (+ lower-limit (floor (/ (- upper-limit lower-limit) 2))))
                  (current-value (elt table current-index))
                  (next-value (elt table (1+ current-index))))
             (if (and (< current-value position)
                      (>= next-value position))
                 (return current-index))
             (if (< current-value position)
                 (setf lower-limit current-index)
                 (setf upper-limit current-index)))))))

(deftest test-find-line-break-index ()
  (let ((table (make-line-break-table)))
    (add-line-break table 1)
    (add-line-break table 10)
    (add-line-break table 50)
    (test-equal (find-line-break-index table 0) 0)
    (test-equal (find-line-break-index table 5) 1)
    (test-equal (find-line-break-index table 10) 1)
    (test-equal (find-line-break-index table 15) 2)
    (test-equal (find-line-break-index table 100) 3)))

;; -----------------------------------------------------------------------------
(defun add-line-break (table position)
  (assert (>= position 0))
  (if (not (zerop position))
      (let ((index (find-line-break-index table position))
            (upper-limit (1- (length table))))
        (if (not (equal (elt table (if (< index upper-limit)
                                       (1+ index)
                                       index)) position))
            (insert-into-array table position (1+ index)))
        (1+ index))))

(deftest test-add-line-break-appends-line-at-end ()
  (let ((table (make-line-break-table)))
    (add-line-break table 10)
    (test-equal (line-count table) 2)
    (test-equal (elt table 1) 10)))

(deftest test-add-line-break-does-not-add-duplicates ()
  (let ((table (make-line-break-table)))
    (add-line-break table 10)
    (add-line-break table 10)
    (add-line-break table 0)
    (test-equal (line-count table) 2)))

(deftest test-add-line-break-independent-of-insertion-order ()
  (let ((table (make-line-break-table)))
    (add-line-break table 10)
    (add-line-break table 5)
    (test-equal (line-count table) 3)
    (test-equal (elt table 1) 5)
    (test-equal (elt table 2) 10)))

;; -----------------------------------------------------------------------------
(defun line-break-p (table position)
  (assert (>= position 0))
  (if (zerop position)
      t
      (let ((index (find-line-break-index table position)))
        (cond ((< index (1- (length table)))
               (equal (elt table (1+ index)) position))
              (t
               (equal (elt table index) position))))))

(deftest test-line-break-p ()
  (let ((table (make-line-break-table)))
    (add-line-break table 10)
    (test (line-break-p table 0))
    (test (line-break-p table 10))
    (test (not (line-break-p table 5)))
    (test (not (line-break-p table 15)))))

;; -----------------------------------------------------------------------------
(defsuite test-text-utilities ()
  (test-make-line-break-table-adds-first-line)
  (test-add-line-break-appends-line-at-end)
  (test-add-line-break-does-not-add-duplicates)
  (test-add-line-break-independent-of-insertion-order)
  (test-find-line-break-index)
  (test-line-break-p))

;;;;============================================================================
;;;;    Diagnostics.
;;;;============================================================================

; 1xxxx are syntax errors
; 2xxxx are syntax warnings
; 3xxxx are semantic errors
; 4xxxx are semantic warnings
; 5xxxx are implementation-specific errors
; 6xxxx are implementation-specific warnings
; 7xxxx are implementation-specific infos

;; -----------------------------------------------------------------------------
(defparameter *diagnostic-types-by-name*
  (make-hash-table))

;; -----------------------------------------------------------------------------
(defparameter *diagnostic-types-by-code*
  (make-hash-table))

;; -----------------------------------------------------------------------------
(defclass diagnostic-type ()
  ((code
    :reader diagnostic-code
    :initarg :code)
   (name
    :reader diagnostic-name
    :initarg :name)
   (format
    :reader diagnostic-format
    :initarg :format)))

;; -----------------------------------------------------------------------------
(defclass diagnostic ()
  ((type
    :reader diagnostic-type
    :initarg :type)
   (format-args
    :reader diagnostic-args
    :initarg :args)
   (source
    :reader diagnostic-source
    :initarg :source)
   (source-region
    :reader diagnostic-source-region
    :initarg :source-region)
   (ast
    :reader diagnostic-arg
    :initarg :ast)))

;; -----------------------------------------------------------------------------
(defclass diagnostic-collection ()
  ((diagnostics
    :reader diagnostics
    :initform (make-array 10 :adjustable t :fill-pointer 0))
   (error-count
    :reader diagnostics-error-count
    :initform 0)
   (warning-count
    :reader diagnostics-warning-count
    :initform 0)))

;; -----------------------------------------------------------------------------
(defmacro defdiagnostic-type (code name format)
  (with-gensyms (instance)
  `(let ((,instance (make-instance 'diagnostic-type
                                   :code ,code
                                   :name ',name
                                   :format ,format)))
     (setf (gethash ,code *diagnostic-types-by-code*) ,instance)
     (setf (gethash ',name *diagnostic-types-by-name*) ,instance))))

;; -----------------------------------------------------------------------------
(defdiagnostic-type 10001 block-not-closed "Expecting '}'")
(defdiagnostic-type 10002 list-not-closed "Expecting ')'")
(defdiagnostic-type 10003 separator-missing "Expecting ','")
(defdiagnostic-type 10004 name-missing "Expecting identifier")

;; -----------------------------------------------------------------------------
(defun get-diagnostic-type-by-code (code)
  (gethash code *diagnostic-types-by-code*))

;; -----------------------------------------------------------------------------
(defun get-diagnostic-type-by-name (name)
  (gethash name *diagnostic-types-by-name*))

;; -----------------------------------------------------------------------------
(defun get-diagnostic-type-for-expecting (expecting)
  (cond ((equal expecting #\})
         (get-diagnostic-type-by-name 'block-not-closed))
        ((equal expecting #\))
         (get-diagnostic-type-by-name 'list-not-closed))
        ((equal expecting #\,)
         (get-diagnostic-type-by-name 'separator-missing))
        ((equal expecting 'parse-identifier)
         (get-diagnostic-type-by-name 'identifier-missing))
        ((symbolp expecting)
         (let ((code (get expecting :diagnostic-code))
               (name (get expecting :diagnostic-name))
               (type (get expecting :diagnostic-type)))
           (if type
               type
               (if code
                   (get-diagnostic-type-by-code code)
                   (if name
                       (get-diagnostic-type-by-name name)
                       (error (format nil "Neither :diagnostic-code nor :diagnostic-name nor :diagnostic-type is set for ~a" expecting)))))))
        (t
         (error (format nil "No diagnostic for expecting '~a'" expecting)))))

;; -----------------------------------------------------------------------------
(defun make-diagnostic (diagnostic-type &key ast source source-region)
  (cond ((numberp diagnostic-type)
         (setf diagnostic-type (get-diagnostic-type-by-code diagnostic-type)))
        ((stringp diagnostic-type)
         (setf diagnostic-type (get-diagnostic-type-by-name diagnostic-type))))
  (if (and (not source-region) ast)
      (setf source-region (ast-source-region ast)))
  (assert (typep diagnostic-type 'diagnostic-type))
  (make-instance 'diagnostic
                 :type diagnostic-type
                 :source source
                 :source-region source-region))

;;;;============================================================================
;;;;    ASTs.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; Abstract base class for AST nodes.
(defclass ast-node ()
  ((region
    :reader ast-source-region
    :initarg :source-region)))

;; -----------------------------------------------------------------------------
;; Node representing a parse error in the syntax tree.
(defclass ast-error (ast-node)
  ((diagnostic
    :reader ast-error-diagnostic
    :initarg :diagnostic)))

;; -----------------------------------------------------------------------------
(defclass ast-list (ast-node)
  ((nodes
    :reader ast-list-nodes
    :initarg :nodes
    :initform nil)))

;; -----------------------------------------------------------------------------
;; Representation of "::" denoting the global namespace.
(defclass ast-global-qualifier (ast-node)
  ())

;; -----------------------------------------------------------------------------
;; A (potentially qualified) identifier.
(defclass ast-identifier (ast-node)
  ((qualifier
    :reader ast-id-qualifier
    :initarg qualifier
    :initform nil)
   (name
    :reader ast-id-name
    :initarg :name)))

;; -----------------------------------------------------------------------------
(defclass ast-modifier (ast-node)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-immutable-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-mutable-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-abstract-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-include-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-import-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-extend-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-read-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-write-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-default-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-final-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-sealed-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-instantiable-modifier (ast-modifier)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-clause (ast-node)
  (expression
   :reader ast-contract-expression
   :initarg :expression))

;; -----------------------------------------------------------------------------
(defclass ast-contract-clause (ast-clause)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-requires-clause (ast-contract-clause)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-ensures-clause (ast-contract-clause)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-invariant-clause (ast-contract-clause)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-when-clause (ast-clause)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-statement (ast-node)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-return-statement (ast-node)
  ((expression
    :reader ast-return-expression
    :initarg :expression)))

;; -----------------------------------------------------------------------------
(defclass ast-expression (ast-node)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-literal-expression (ast-expression)
  ((value
    :reader ast-literal-value
    :initarg :value)))

;; -----------------------------------------------------------------------------
(defclass ast-character-literal (ast-literal-expression)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-string-literal (ast-literal-expression)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-integer-literal (ast-literal-expression)
  ((type
    :reader ast-integer-type
    :initarg :type
    :initform 'integer)))

;; -----------------------------------------------------------------------------
(defclass ast-float-literal (ast-literal-expression)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-type (ast-node)
  ((modifiers
    :reader ast-type-modifiers
    :initarg :modifiers)))

;; -----------------------------------------------------------------------------
(defclass ast-special-type (ast-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-nothing-type (ast-special-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-named-type (ast-type)
  ((name
    :reader ast-type-name
    :initarg :name)
   type-arguments))

;; -----------------------------------------------------------------------------
(defclass ast-combination-type (ast-type)
  ((left-type
    :reader ast-type-left
    :initarg :left-type)
   (right-type
    :reader ast-type-right
    :initarg :right-type)))

;; -----------------------------------------------------------------------------
(defclass ast-function-type (ast-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-union-type (ast-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-intersection-type (ast-combination-type)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-definition (ast-node)
  ((attributes
    :reader ast-definition-attributes
    :initarg :attributes)
   (modifiers
    :reader ast-definition-modifiers
    :initarg :modifiers)
   (name
    :reader ast-definition-name
    :initarg :name)
   (type-parameters
    :reader ast-definition-type-parameters
    :initarg :type-parameters)
   (value-parameters
    :reader ast-definition-value-parameters
    :initarg :value-parameters)
   (clauses
    :reader ast-definition-clauses
    :initarg :clauses)
   (type
    :reader ast-definition-type
    :initarg :type)
   (value
    :reader ast-definition-value
    :initarg :value)
   (body
    :reader ast-definition-body
    :initarg :body)))

;; -----------------------------------------------------------------------------
(defclass ast-type-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-object-definition (ast-type-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-function-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-field-definition (ast-function-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-method-definition (ast-function-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-variable-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-module-definition (ast-definition)
  ())

;; -----------------------------------------------------------------------------
(defclass ast-compilation-unit (ast-node)
  ((definitions
    :reader ast-unit-definitions
    :initarg :definitions)))

;; -----------------------------------------------------------------------------
(defun make-ast-error (diagnostic)
  (make-instance 'ast-error
                 :source-region (diagnostic-source-region diagnostic)
                 :diagnostic diagnostic))

;; -----------------------------------------------------------------------------
(defmethod initialize-instance :after ((id ast-identifier) &key)
  ;; Convert 'name' slot to symbol, if it isn't one already.
  (let ((name (ast-id-name id)))
    (if (not (symbolp name))
        (setf (slot-value id 'name) (intern name)))))

(deftest test-ast-identifier-initialize ()
  (let ((id (make-instance 'ast-identifier :name "test")))
    (test (symbolp (ast-id-name id)))))

;; -----------------------------------------------------------------------------
(defun identifier-to-string (id)
  (let ((qualifier (ast-id-qualifier id)))
    (cond ((not qualifier)
           (symbol-name (ast-id-name id)))
          ((typep qualifier 'ast-global-qualifier)
           (concatenate 'string "::" (symbol-name (ast-id-name id))))
          (t
           (concatenate 'string (identifier-to-string qualifier) "::" (symbol-name (ast-id-name id)))))))

(deftest test-identifier-to-string ()
  (let ((id1 (make-instance 'ast-identifier :name "test")))
    (test-equal "test" (identifier-to-string id1))))

;; -----------------------------------------------------------------------------
(defun identifier-to-lisp (id)
  (intern (identifier-to-string id)))

;; -----------------------------------------------------------------------------
(defun definition-has-modifier-p (ast type)
  (let ((modifiers (ast-definition-modifiers ast)))
    (if (not modifiers)
        nil
        (find-if (lambda (modifier) (typep modifier type)) (ast-list-nodes modifiers)))))

(deftest test-definition-has-modifier-p ()
  (let ((abstract-ast (parse-result-value (parse-definition (make-string-scanner "abstract type Foobar;") (make-instance 'parser-state))))
        (plain-ast (parse-result-value (parse-definition (make-string-scanner "type Foobar;") (make-instance 'parser-state)))))
    (test (definition-has-modifier-p abstract-ast 'ast-abstract-modifier))
    (test (not (definition-has-modifier-p plain-ast 'ast-abstract-modifier)))))

;; -----------------------------------------------------------------------------
(defun definition-abstract-p (ast)
  (definition-has-modifier-p ast 'ast-abstract-modifier))

;; -----------------------------------------------------------------------------
(defsuite test-asts ()
  (test-ast-identifier-initialize)
  (test-identifier-to-string)
  (test-definition-has-modifier-p))

;;;;============================================================================
;;;;    Naming Conventions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass naming-convention ()
  (prefix
   suffix
   first-character-case
   middle-character-case
   last-character-case))

;; -----------------------------------------------------------------------------
(defgeneric normalize-name (name convention))

;;;;============================================================================
;;;;    Scanners.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass scanner ()
  ((position
    :initform 0
    :accessor scanner-position)))

;; -----------------------------------------------------------------------------
(defclass string-scanner (scanner)
  ((string
    :initarg :string
    :initform "")))

;; -----------------------------------------------------------------------------
(defclass stream-scanner (scanner)
  ((stream
    :initarg :stream)
   (stream-length
    :initarg :length)))

;; -----------------------------------------------------------------------------
(defgeneric scanner-at-end-p (scanner))

;; -----------------------------------------------------------------------------
(defgeneric scanner-peek-next (scanner))

;; -----------------------------------------------------------------------------
(defgeneric scanner-read-next (scanner))

;; -----------------------------------------------------------------------------
(defgeneric scanner-match (scanner token))

;; -----------------------------------------------------------------------------
(defgeneric scanner-match-if (scanner function))

;; -----------------------------------------------------------------------------
(defmethod scanner-match ((scanner scanner) token)
  (cond ((scanner-at-end-p scanner) nil)
        ((equal (scanner-peek-next scanner) token)
         (scanner-read-next scanner)
         t)
        (t nil)))

(deftest test-scanner-match ()
  (let ((scanner (make-stream-scanner "foo")))
    (test (scanner-match scanner #\f))
    (test (not (scanner-match scanner #\b)))))

;; -----------------------------------------------------------------------------
(defun make-stream-scanner (stream &key length)
  (if (stringp stream)
      (progn
        (setf length (length stream))
        (setf stream (make-string-input-stream stream))))
  (make-instance 'stream-scanner :stream stream :length length))

;; -----------------------------------------------------------------------------
(defmethod initialize-instance :after ((instance stream-scanner) &key)
  (if (not (slot-value instance 'stream-length))
      (setf (slot-value instance 'stream-length) (file-length (slot-value instance 'stream)))))

;; -----------------------------------------------------------------------------
(defmethod scanner-at-end-p ((scanner stream-scanner))
  (>= (slot-value scanner 'position)
      (slot-value scanner 'stream-length)))

(deftest test-stream-scanner-at-end-p ()
  (let ((scanner (make-stream-scanner "")))
    (test (scanner-at-end-p scanner))))

;; -----------------------------------------------------------------------------
(defmethod scanner-peek-next ((scanner stream-scanner))
  (peek-char nil (slot-value scanner 'stream)))

(deftest test-stream-scanner-peek-next ()
  (let ((scanner (make-stream-scanner "foo")))
    (test-equal (scanner-peek-next scanner) #\f)))

;; -----------------------------------------------------------------------------
(defmethod scanner-read-next ((scanner stream-scanner))
  (let ((char (read-char (slot-value scanner 'stream))))
    (incf (slot-value scanner 'position))
    char))

(deftest test-stream-scanner-read-next ()
  (let ((scanner (make-stream-scanner "foo")))
    (test-equal (scanner-read-next scanner) #\f)))

;; -----------------------------------------------------------------------------
(defun make-string-scanner (string)
  (make-instance 'string-scanner :string string))

;; -----------------------------------------------------------------------------
(defmethod scanner-at-end-p ((scanner string-scanner))
  (equal (slot-value scanner 'position)
         (length (slot-value scanner 'string))))

(deftest test-string-scanner-at-end-p ()
  (test (not (scanner-at-end-p (make-string-scanner "foo"))))
  (test (scanner-at-end-p (make-string-scanner ""))))

;; -----------------------------------------------------------------------------
(defmethod scanner-peek-next ((scanner string-scanner))
  (elt (slot-value scanner 'string)
       (slot-value scanner 'position)))

(deftest test-string-scanner-peek-next ()
  (let ((scanner (make-string-scanner "foo")))
    (test (equal (scanner-peek-next scanner) #\f))
    (test (equal (scanner-position scanner) 0))))

;; -----------------------------------------------------------------------------
(defmethod scanner-read-next ((scanner string-scanner))
  (let ((token (scanner-peek-next scanner)))
    (incf (slot-value scanner 'position))
    token))

(deftest test-string-scanner-read-next ()
  (let ((scanner (make-string-scanner "foo")))
    (test (equal (scanner-read-next scanner) #\f))
    (test (equal (scanner-position scanner) 1))))

;; -----------------------------------------------------------------------------
(defmethod scanner-match ((scanner string-scanner) char)
  (cond
    ((scanner-at-end-p scanner) nil)
    ((equal (scanner-peek-next scanner) char)
     (incf (slot-value scanner 'position))
     t)
    (t nil)))

(deftest test-string-scanner-match ()
  (test (scanner-match (make-string-scanner "foo") #\f))
  (test (not (scanner-match (make-string-scanner "foo") #\b)))
  (test (not (scanner-match (make-string-scanner "") #\f))))

;; -----------------------------------------------------------------------------
(defmethod scanner-match-if ((scanner string-scanner) function)
  (cond
    ((scanner-at-end-p scanner) nil)
    ((funcall function (scanner-peek-next scanner))
     (incf (slot-value scanner 'position)))
    (t nil)))

(deftest test-string-scanner-match-if ()
  (test (scanner-match-if (make-string-scanner "foo" ) (lambda (char) t)))
  (test (not (scanner-match-if (make-string-scanner "foo") (lambda (char) nil)))))

;; -----------------------------------------------------------------------------
(defun scanner-match-sequence (scanner token-list)
  (let ((saved-position (scanner-position scanner)))
    (if (every (lambda (token) (scanner-match scanner token)) token-list)
        t
        (progn
          (setf (scanner-position scanner) saved-position)
          nil))))

(deftest test-scanner-match-sequence ()
  (let ((scanner (make-string-scanner "foo")))
    (test (not (scanner-match-sequence scanner "bar")))
    (test-equal 0 (scanner-position scanner)))
  (let ((scanner (make-string-scanner "foobar")))
    (test (scanner-match-sequence scanner "foo"))
    (test-equal 3 (scanner-position scanner))))

;; -----------------------------------------------------------------------------
(defun scanner-match-keyword (scanner token-list)
  (let ((saved-position (scanner-position scanner)))
    (if (and (scanner-match-sequence scanner token-list)
             (or (scanner-at-end-p scanner)
                 (let ((next-char (scanner-peek-next scanner)))
                   (and (not (alphanumericp next-char))
                        (not (equal next-char #\_))))))
        t
        (progn
          (setf (scanner-position scanner) saved-position)
          nil))))

(deftest test-scanner-match-keyword ()
  (let ((scanner (make-string-scanner "foobar")))
    (test (not (scanner-match-keyword scanner "foo")))
    (test-equal 0 (scanner-position scanner)))
  (let ((scanner (make-string-scanner "foobar ")))
    (test (scanner-match-keyword scanner "foobar"))
    (test-equal 6 (scanner-position scanner)))
  (let ((scanner (make-string-scanner "foo")))
    (test (scanner-match-keyword scanner "foo"))
    (test-equal 3 (scanner-position scanner)))
  (let ((scanner (make-string-scanner "foo_")))
    (test (not (scanner-match-keyword scanner "foo")))))

;; -----------------------------------------------------------------------------
(defmethod (setf scanner-position) :before (position (scanner string-scanner))
  (if (or (< position 0)
          (> position (length (slot-value scanner 'string))))
      (error (format nil "Position ~a out of range for string ~a used by string-scanner!" position (slot-value scanner 'string)))))

(deftest test-string-scanner-setf-position ()
  (let ((scanner (make-string-scanner "foo")))
    (setf (scanner-position scanner) 0)
    (test (scanner-position scanner) 0)
    (setf (scanner-position scanner) 3)
    (test (scanner-position scanner) 3)))

;; -----------------------------------------------------------------------------
(defsuite test-scanners ()
  (test-scanner-match)
  (test-scanner-match-sequence)
  (test-scanner-match-keyword)
  (test-stream-scanner-at-end-p)
  (test-stream-scanner-peek-next)
  (test-string-scanner-at-end-p)
  (test-string-scanner-peek-next)
  (test-string-scanner-read-next)
  (test-string-scanner-match)
  (test-string-scanner-match-if)
  (test-string-scanner-setf-position))

;;;;============================================================================
;;;;    Parsers.
;;;;============================================================================

;; The parsers here are a mixture of plain recursive descent and
;; combinators.  Each parser implements a specific pattern that it
;; will recognize and from which it will derive an optional value.  As
;; a side-effect of recognizing a pattern, it will advance the
;; position in the input sequence to the first position after the
;; match.
;;
;; All parsers have to either produce a full match or produce no match
;; at all.
;;
;; If a parser runs into a situation where it can partially recognize
;; a prefix of the input but then comes across input that doesn't
;; match what is expected, it can
;;
;;  a) either abort, reset the input position, and return a no-match, or
;;  b) apply error repair and return a match.
;;
;; Error repair is usually implemented by falling back to parsers that
;; recognize a superset of valid input but produce diagnostics and
;; error nodes as side- effects.  In that sense, there are two
;; languages implemented by the parsers: one that is Flux and one that
;; is a superset of Flux trying to recognize as much of Flux as
;; possible in otherwise invalid input.
;;
;; Diagnostics produced by error handling will be stored on the parser
;; state.  Error nodes are normal AST nodes that refer to the
;; diagnostics and that are made part of the AST like any other type
;; of node.
;;
;; Parsers follow a longest-prefix matching strategy, i.e. as long as
;; a prefix matches their expected pattern, they will produce a match.
;; They will, however, go for the longest possible prefix they can
;; match.

;; -----------------------------------------------------------------------------
(defmacro test-parser (function string &key (is-match-p :dont-test) end-position state expected-type line-breaks-at checks)
  (with-gensyms (res scanner end-position-value expected-type-value state-value line-breaks-at-value)
    `(let* ((,scanner (make-string-scanner ,string))
            (,state-value ,state))

       (if (not ,state-value)
           (setf ,state-value (make-instance 'parser-state)))

       (let*
           ((,res (,function ,scanner ,state-value))
            (,end-position-value ,end-position)
            (,expected-type-value ,expected-type)
            (,line-breaks-at-value ,line-breaks-at))

         ;////TODO: automatically check region of parsed AST

         (combine-results

           ;; Check match, if requested.
           ,(cond
             ((eq is-match-p :dont-test)
              t)
             (is-match-p
              `(test (parse-result-match-p ,res)))
             (t
              `(test (parse-result-no-match-p ,res))))

           ;; Check end position, if requested.
           (if ,end-position-value
               (test-equal ,end-position-value (scanner-position ,scanner))
               (if ,(and is-match-p (not (eq is-match-p :dont-test)))
                   (test-equal (length ,string) (scanner-position ,scanner))
                   t))

           ;; Check value type, if requested.
           (if ,expected-type-value
               (test-type ,expected-type-value (if (parse-result-match-p ,res) (parse-result-value ,res) ,res))
               t)
        
           ;; Check line breaks, if requested.
           (if ,line-breaks-at-value
               (test-sequence-equal ,line-breaks-at-value (line-break-table ,state-value))
               t)

           ;; Check AST.
           ,(if (> (length checks) 0)
                `(let ((ast (parse-result-value ,res)))
                   (combine-results ,@checks))))))))

;; -----------------------------------------------------------------------------
(defparameter *parse-result-no-match* :no-match)

;; -----------------------------------------------------------------------------
(defparameter *parse-result-no-value* :no-value)

;; -----------------------------------------------------------------------------
(defclass parser-state ()
  ((line-break-table
    :reader line-break-table
    :initform (make-line-break-table)
    :initarg :line-break-table)
   (diagnostics
    :reader parser-diagnostics
    :initarg :diagnostics)))

;; -----------------------------------------------------------------------------
(defun parse-result-match-p (result)
  (not (eq result *parse-result-no-match*)))

;; -----------------------------------------------------------------------------
(defun parse-result-no-match-p (result)
  (eq result *parse-result-no-match*))

;; -----------------------------------------------------------------------------
(defun parse-result-value (result)
  (assert (parse-result-match-p result))
  (if (eq result *parse-result-no-value*)
      nil
      result))

;; -----------------------------------------------------------------------------
(defun parse-result-match (&optional value)
  (if value
      value
      *parse-result-no-value*))

;; -----------------------------------------------------------------------------
(defun parse-result-no-match ()
  *parse-result-no-match*)

;; -----------------------------------------------------------------------------
(defun parse-whitespace (scanner state)
  (let ((initial-position (scanner-position scanner)))
    (flet ((line-break () (add-line-break (line-break-table state) (scanner-position scanner)))
           (consume () (scanner-read-next scanner)))
      (loop
         (if (scanner-at-end-p scanner)
             (return))
         (let ((char (scanner-peek-next scanner)))
           (cond ((equal char #\Newline)
                  (consume)
                  (line-break))
                 ((equal char #\Return)
                  (consume)
                  (scanner-match scanner #\Newline)
                  (line-break))
                 ((or (equal char #\Tab)
                      (equal char #\Space))
                  (consume))
                 ((equal char #\/)
                  (scanner-read-next scanner)
                  (cond ((scanner-match scanner #\/)
                         (loop
                            (if (or (scanner-at-end-p scanner)
                                    (not (scanner-match-if scanner
                                                           (lambda (char) (not (or (equal char #\Newline)
                                                                                   (equal char #\Return)))))))
                                (return))))
                        ((scanner-match scanner #\*)
                         (let ((nesting-level 1))
                           (loop
                              (if (or (zerop nesting-level)
                                      (scanner-at-end-p scanner))
                                  (return))
                              (let ((next-char (scanner-read-next scanner)))
                                (cond ((and (equal next-char #\/)
                                            (scanner-match scanner #\*))
                                       (incf nesting-level))
                                      ((and (equal next-char #\*)
                                            (scanner-match scanner #\/))
                                       (decf nesting-level))
                                      ((equal next-char #\Return)
                                       (scanner-match scanner #\Newline)
                                       (line-break))
                                      ((equal next-char #\Newline)
                                       (line-break)))))))
                        (t
                         (decf (scanner-position scanner))
                         (return))))
                 (t
                  (return))))))
    (if (not (equal initial-position (scanner-position scanner)))
        (parse-result-match nil)
        (parse-result-no-match))))

(deftest test-parse-whitespace-does-not-consume-non-whitespace ()
  (test-parser parse-whitespace "foo" :end-position 0 :is-match-p nil))

(deftest test-parse-whitespace-consumes-whitespace ()
  (test-parser parse-whitespace (format nil " ~C~C~Cfoo" #\Return #\Newline #\Tab) :is-match-p t :end-position 4))

(deftest test-parse-whitespace-consumes-single-line-comments ()
  (test-parser parse-whitespace (format nil " // foo~Cbar" #\Newline) :is-match-p t :end-position 8 :line-breaks-at '(0 8)))

(deftest test-parse-whitespace-consume-multi-line-comments ()
  (test-parser parse-whitespace (format nil " /* foo /* ~C bar */ */foo" #\Newline) :is-match-p t :end-position 22 :line-breaks-at '(0 12)))

(deftest test-parse-whitespace ()
  (test-parse-whitespace-does-not-consume-non-whitespace)
  (test-parse-whitespace-consumes-whitespace)
  (test-parse-whitespace-consumes-single-line-comments)
  (test-parse-whitespace-consume-multi-line-comments))

;; -----------------------------------------------------------------------------
;; Combination that makes sure the given parser consumes all available input.
;; Otherwise produces a parse error.
(defmacro parse-all-input (parser-name scanner state)
  ())

;; -----------------------------------------------------------------------------
;; Combinator that turns another parser into a list parser.  This combinator can be broken
;; down into more atomic combinators that can be composed to create the same effect as this
;; combinator here yet in a more flexible manner.  However, it works nicely for the parsers
;; we have.
(defmacro parse-list (parser-name scanner state &key start-delimiter end-delimiter separator)
  ;; List must either be undelimited or have both a start and end
  ;; delimiter.
  (with-gensyms (scanner-value state-value list start-position element parse-list-block result)
    `(block ,parse-list-block
       (let ((,scanner-value ,scanner)
             (,state-value ,state))

         (parse-whitespace ,scanner-value ,state-value)
         (let ((,start-position (scanner-position ,scanner-value))
               ,element
               ,list)

           ;; Match start-delimiter.
           ,(if start-delimiter
                `(progn
                   (parse-whitespace ,scanner-value ,state-value)
                   (if (not (scanner-match ,scanner-value ,start-delimiter))
                       (return-from ,parse-list-block (parse-result-no-match)))))

           ;; Match elements.
           (loop

              ;; Try to parse another element.
              (parse-whitespace ,scanner-value ,state-value)
              (setf ,element (,parser-name ,scanner-value ,state-value))
              (parse-whitespace ,scanner-value ,state-value)

              ;; Handle parse result.
              (cond

                ;; Handle match.
                ((parse-result-match-p ,element)
                 (setf ,list (cons (parse-result-value ,element) ,list))
               
                 ;; Consume separator or terminate on end-delimiter.
                 ,(cond

                   ;; In a list with only separators and no end-delimiter,
                   ;; we stop as soon as an element isn't followed by a separator.
                   ((and separator (not end-delimiter))
                    `(if (not (scanner-match ,scanner-value ,separator))
                         (return)))

                   ;; In a list with both separators and an end-delimiter, we
                   ;; continue if we see a separator and we stop if we see an
                   ;; end-delimiter.
                   ((and separator end-delimiter)
                    `(if (scanner-match ,scanner-value ,separator)
                         t
                         (if (scanner-match ,scanner-value ,end-delimiter)
                             (return)
                             (not-implemented "expecting separator or terminator"))))))

                ;; Handle no match.
                ((parse-result-no-match-p ,element)
               
                 ,(if separator
                      `(cond

                         ;; If we don't have an end-delimiter and we've already read
                         ;; one or more elements, we're we missing an element.
                         ((and ,(not end-delimiter)
                               ,list)
                          (let* ((diagnostic (make-diagnostic (get-diagnostic-type-for-expecting ',parser-name)))
                                 (ast (make-ast-error diagnostic)))
                            (not-implemented "error handling")
                            (setf ,list (cons ast ,list))
                            (return)))

                         ;; If next up is a separator, we're missing an element.
                         ((scanner-match ,scanner-value ,separator)
                          (not-implemented "error; expecting element"))
                        
                         ;; Otherwise, if we haven't yet parsed any elements and we don't
                         ;; have an end-delimiter, we consider it a no-match rather than a match
                         ;; against an empty list.
                         ((and (not ,list)
                               ,(not end-delimiter))
                          (return-from ,parse-list-block (parse-result-no-match)))))

                 ,(if end-delimiter
                      `(if (not (scanner-match ,scanner-value ,end-delimiter))
                           (not-implemented "error; expecting element or terminator")))

                 (return))))
                  
           (setf ,list (nreverse ,list))
           (make-instance 'ast-list 
                          :source-region (make-source-region ,start-position (scanner-position ,scanner-value))
                          :nodes ,list))))))

(deftest test-parse-list ()
  (labels
      ;; Define a dummy parser that matches "a".
      ((parse-a (scanner state)
         (if (scanner-match scanner #\a)
             (parse-result-match (make-instance 'ast-node :source-region (make-source-region (1- (scanner-position scanner)) (scanner-position scanner))))
             (parse-result-no-match)))

       ;; Parser that matches "a, a, a...".
       (parse-comma-separated-as (scanner state)
         (parse-list parse-a scanner state :separator #\,)))
   
    (setf (get 'parse-a :diagnostic-type) (make-instance 'diagnostic-type :code 80000 :name "a"))

    (with-test-name test-no-match
      (test-parser parse-comma-separated-as "foo" :is-match-p nil :end-position 0))

    (with-test-name test-single-element
      (test-parser parse-comma-separated-as "a" :is-match-p t :expected-type 'ast-list :end-position 1))

    (with-test-name test-comma-separated
      (test-parser parse-comma-separated-as "a, a, a]" :is-match-p t :expected-type 'ast-list :end-position 7))))

;; -----------------------------------------------------------------------------
(defun parse-modifier (scanner state)
  (let ((saved-position (scanner-position scanner)))
    (macrolet ((match (modifier ast-type)
                 `(if (scanner-match-keyword scanner ,modifier)
                      (return-from parse-modifier (parse-result-match
                                                   (make-instance ,ast-type
                                                                  :source-region (make-source-region saved-position (scanner-position scanner))))))))
      (match "abstract" 'ast-abstract-modifier)
      (match "immutable" 'ast-immutable-modifier)
      (match "mutable" 'ast-mutable-modifier)
      (match "instantiable" 'ast-instantiable-modifier)
      (match "extend" 'ast-extend-modifier)
      (match "import" 'ast-import-modifier)
      (match "include" 'ast-include-modifier)
      (match "final" 'ast-final-modifier)
      (match "sealed" 'ast-sealed-modifier)
      (parse-result-no-match))))

(deftest test-parse-modifier ()
  (test-parser parse-modifier "importA" :is-match-p nil)
  (test-parser parse-modifier "abstract[" :end-position 8 :is-match-p t :expected-type 'ast-abstract-modifier)
  (test-parser parse-modifier "immutable " :end-position 9 :is-match-p t :expected-type 'ast-immutable-modifier)
  (test-parser parse-modifier "mutable" :end-position 7 :is-match-p t :expected-type 'ast-mutable-modifier)
  (test-parser parse-modifier "import?" :end-position 6 :is-match-p t :expected-type 'ast-import-modifier)
  (test-parser parse-modifier "include%" :end-position 7 :is-match-p t :expected-type 'ast-include-modifier)
  (test-parser parse-modifier "extend+" :end-position 6 :is-match-p t :expected-type 'ast-extend-modifier)
  (test-parser parse-modifier "instantiable!" :end-position 12 :is-match-p t :expected-type 'ast-instantiable-modifier)
  (test-parser parse-modifier "final " :end-position 5 :is-match-p t :expected-type 'ast-final-modifier)
  (test-parser parse-modifier "sealed_" :is-match-p nil)
  (test-parser parse-modifier "sealed#" :end-position 6 :is-match-p t :expected-type 'ast-sealed-modifier))

;; -----------------------------------------------------------------------------
(defun parse-modifier-list (scanner state)
  (parse-list parse-modifier scanner state))

;; -----------------------------------------------------------------------------
(defun parse-identifier (scanner state)
  (let ((start-position (scanner-position scanner))
        (next-char (scanner-peek-next scanner))
        (buffer (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character)))
    (if (or (alpha-char-p next-char)
            (equal next-char #\_))
        (progn
          (scanner-read-next scanner)
          (vector-push-extend next-char buffer)
          (loop
             (if (scanner-at-end-p scanner)
                 (return))
             (setf next-char (scanner-peek-next scanner))
             (if (and (not (alphanumericp next-char))
                      (not (equal next-char #\_)))
                 (return))
             (vector-push-extend next-char buffer)
             (scanner-read-next scanner))
          (parse-result-match (make-instance 'ast-identifier
                                             :source-region (make-source-region start-position (scanner-position scanner))
                                             :name (intern buffer))))
        (parse-result-no-match))))

(deftest test-parse-identifier-simple-name ()
  (test-parser parse-identifier "test"
               :end-position 4 :is-match-p t :expected-type 'ast-identifier
               :checks ((test-equal (intern "test") (ast-id-name ast))
                        (test-equal nil (ast-id-qualifier ast)))))

(deftest test-parse-identifier ()
  (test-parse-identifier-simple-name))

;; -----------------------------------------------------------------------------
(defun parse-clause (scanner state)
  (let ((saved-position (scanner-position scanner))
        (clause-type (cond ((scanner-match-keyword scanner "when")
                            'ast-when-clause)
                           ((scanner-match-keyword scanner "requires")
                            'ast-requires-clause)
                           ((scanner-match-keyword scanner "ensures")
                            'ast-ensures-clause)
                           ((scanner-match-keyword scanner "invariant")
                            'ast-invariant-clause)
                           (t
                            (return-from parse-clause (parse-result-no-match))))))
    (parse-whitespace scanner state)
    (let ((expression (parse-expression scanner state)))
      (if (parse-result-no-match-p expression)
          (not-implemented "parse error; expecting expression"))
      (parse-result-match (make-instance clause-type
                                         :source (make-source-region saved-position (scanner-position scanner))
                                         :expression (parse-result-value expression))))))

;; -----------------------------------------------------------------------------
(defun parse-clause-list (scanner state)
  (parse-list parse-clause scanner state))

;; -----------------------------------------------------------------------------
(defun parse-attribute (scanner state)
  (parse-result-no-match))

;; -----------------------------------------------------------------------------
(defun parse-attribute-list (scanner state)
  (parse-list parse-attribute scanner state :start-delimiter #\[ :separator #\, :end-delimiter #\]))

;; -----------------------------------------------------------------------------
(defun parse-type (scanner state)
  (let* ((saved-position (scanner-position scanner))
         (left-type (cond ((scanner-match scanner #\()
                           (parse-whitespace scanner state)
                           (if (scanner-match scanner #\))
                               (parse-result-match (make-instance 'ast-nothing-type
                                                                  :source-region (make-source-region saved-position (scanner-position scanner))))
                               (not-implemented "parenthesized type expressions")))
                          (t
                           (let (modifiers name)
                             (setf modifiers (parse-modifier-list scanner state))
                             (parse-whitespace scanner state)
                             (setf name (parse-identifier scanner state))
                             (if (parse-result-no-match-p name)
                                 (not-implemented "parse error; expecting type name"))
                             (parse-result-match (make-instance 'ast-named-type
                                                                :source-region (make-source-region saved-position (scanner-position scanner))
                                                                :name (parse-result-value name)
                                                                :modifiers (parse-result-value modifiers))))))))
    (parse-whitespace scanner state)

    (let ((combination-type (cond ((scanner-match-sequence scanner "->")
                                   'ast-function-type)
                                  ((scanner-match scanner #\&)
                                   'ast-union-type)
                                  ((scanner-match scanner #\|)
                                   'ast-intersection-type)
                                  (t nil))))
      (if (not combination-type)
          (return-from parse-type left-type))

      (parse-whitespace scanner state)
      (let ((right-type (parse-type scanner state)))
        (if (parse-result-no-match-p right-type)
            (not-implemented "parse error; expecting right type"))
        (parse-result-match (make-instance combination-type
                                           :source-region (make-source-region saved-position (scanner-position scanner))
                                           :left-type (parse-result-value left-type)
                                           :right-type (parse-result-value right-type)))))))

(deftest test-parse-simple-named-type ()
  (test-parser parse-type "Foobar" :is-match-p t :expected-type 'ast-named-type))

(deftest test-parse-nothing-type ()
  (test-parser parse-type "()" :is-match-p t :expected-type 'ast-nothing-type))

(deftest test-parse-simple-function-type ()
  (test-parser parse-type "() -> ()" :is-match-p t :expected-type 'ast-function-type))

(deftest test-parse-simple-union-type ()
  (test-parser parse-type "Foobar & Foobar" :is-match-p t :expected-type 'ast-union-type
               :checks ((test-type 'ast-named-type (ast-type-left ast))
                        (test-type 'ast-named-type (ast-type-right ast)))))

(deftest test-parse-type ()
  (test-parse-simple-named-type)
  (test-parse-nothing-type)
  (test-parse-simple-function-type)
  (test-parse-simple-union-type))

;; -----------------------------------------------------------------------------
(defun parse-type-parameter (scanner state)
  (parse-result-no-match))

;; -----------------------------------------------------------------------------
(defun parse-type-parameter-list (scanner state)
  "Parse a list of type parameters surrounded by '<' and '>'."
  (parse-list parse-type-parameter scanner state :start-delimiter #\< :separator #\, :end-delimiter #\>))

;; -----------------------------------------------------------------------------
(defun parse-value-parameter (scanner state)
  (parse-result-no-match))

;; -----------------------------------------------------------------------------
(defun parse-value-parameter-list (scanner state)
  "Parse a list of value parameters surrounded by '(' and ')'."
  (parse-list parse-value-parameter scanner state :start-delimiter #\( :separator #\, :end-delimiter #\)))

;; -----------------------------------------------------------------------------
(defun parse-statement (scanner state)
  (let ((saved-position (scanner-position scanner)))
    (cond ((scanner-match-keyword scanner "return")
           (parse-whitespace scanner state)
           (let (expression)
             (if (not (scanner-match scanner #\;))
                 (progn
                   (setf expression (parse-expression scanner state))
                   (if (parse-result-no-match-p expression)
                       (not-implemented "parse error; expecting expression after 'return'"))
                   (if (not (scanner-match scanner #\;))
                       (not-implemented "parse error; expecting ';'"))))
             (parse-result-match (make-instance 'ast-return-statement
                                                :source-region (make-source-region saved-position (scanner-position scanner))
                                                :expression (parse-result-value expression)))))
          (t
           (parse-result-no-match)))))

(deftest test-parse-return-statement ()
  (test-parser parse-statement "return ;" :is-match-p t :expected-type 'ast-return-statement))

(deftest test-parse-statement ()
  (test-parse-return-statement))

;; -----------------------------------------------------------------------------
(defun parse-statement-list (scanner state)
  "Parse a list of statements surrounded by '{' and '}'."
  (parse-list parse-statement scanner state :start-delimiter #\{ :end-delimiter #\}))

;; -----------------------------------------------------------------------------
(defun parse-literal (scanner state)
  (if (scanner-at-end-p scanner)
      (parse-result-no-match)
      (let ((saved-position (scanner-position scanner))
            (next-char (scanner-read-next scanner)))
        (cond ((digit-char-p next-char)
               (if (and (char-equal #\0 next-char)
                        (scanner-match scanner #\x))
                   (not-implemented "hex literals"))
               (let ((value (digit-char-to-integer next-char)))
                 (loop
                    (if (scanner-at-end-p scanner)
                        (return))
                    (setf next-char (scanner-peek-next scanner))
                    (if (not (digit-char-p next-char))
                        (return))
                    (scanner-read-next scanner)
                    (setf value (+ (digit-char-to-integer next-char) (* value 10))))
                 (parse-result-match (make-instance 'ast-integer-literal
                                                    :source-region (make-source-region saved-position (scanner-position scanner))
                                                    :value value))))
              (t (not-implemented "literal"))))))

(deftest test-parse-integer-literal ()
  (test-parser parse-literal "12" :is-match-p t :expected-type 'ast-integer-literal
               :checks ((test-equal 12 (ast-literal-value ast)))))

(deftest test-parse-literal ()
  (test-parser parse-literal "" :is-match-p nil)
  (test-parse-integer-literal))

;; -----------------------------------------------------------------------------
(defun parse-expression (scanner state)
  (if (scanner-at-end-p scanner)
      (parse-result-no-match)
      (let ((next-char (scanner-peek-next scanner)))
        (cond ((or (digit-char-p next-char)
                   (char-equal #\" next-char))
               (parse-literal scanner state))
              (t
               (parse-result-no-match))))))

(deftest test-parse-literal-expression ()
  (test-parser parse-expression "512" :is-match-p t :expected-type 'ast-integer-literal
               :checks ((test-equal 512 (ast-literal-value ast)))))

(deftest test-parse-expression ()
  (test-parse-literal-expression))

;; -----------------------------------------------------------------------------
(defun parse-definition (scanner state)
  (let (start-position
        attributes
        modifiers
        definition-class
        name
        value-parameters
        type-parameters
        type
        clauses
        value
        body
        ast)

    (parse-whitespace scanner state)
    (setf start-position (scanner-position scanner))

    ;; Parse attributes.
    (setf attributes (parse-attribute-list scanner state))

    ;; Parse modifiers.
    (setf modifiers (parse-modifier-list scanner state))

    ;;////FIXME: this needs to match keyword+whitespace, not just keyword
    ;; Parse definition kind.
    (parse-whitespace scanner state)
    (setf definition-class (cond ((scanner-match-keyword scanner "method")
                                 'ast-method-definition)
                                ((scanner-match-keyword scanner "field")
                                 'ast-field-definition)
                                ((scanner-match-keyword scanner "type")
                                 'ast-type-definition)
                                ((scanner-match-keyword scanner "object")
                                 'ast-object-definition)
                                ((scanner-match-keyword scanner "local")
                                 'ast-variable-definition)
                                ((scanner-match-keyword scanner "function")
                                 'ast-function-definition)
                                ((scanner-match-keyword scanner "features")
                                 'ast-features-definition)
                                ((scanner-match-keyword scanner "module")
                                 'ast-module-definition)
                                (t
                                 (return-from parse-definition (parse-result-no-match)))))

    ;; Parse name.
    (parse-whitespace scanner state)
    (setf name (parse-identifier scanner state))

    ;; Parse type parameters.
    (setf type-parameters (parse-type-parameter-list scanner state))
    
    ;; Parse value parameters.
    (setf value-parameters (parse-value-parameter-list scanner state))

    ;; Parse type.
    (parse-whitespace scanner state)
    (if (scanner-match scanner #\:)
        (progn
          (parse-whitespace scanner state)
          (setf type (parse-type scanner state))))

    ;; Parse clauses.
    (setf clauses (parse-clause-list scanner state))

    ;; Parse body/value.
    (parse-whitespace scanner state)
    (cond ((scanner-match scanner #\;)
           t)
          ;;////TODO: if it's a type definition, we need to parse a type here when hitting '='
          ((scanner-match scanner #\=)
           (parse-whitespace scanner state)
           (setf value (parse-expression scanner state))
           (parse-whitespace scanner state)
           (if (not (scanner-match scanner #\;))
               (not-implemented "parse error; expecting semicolon")))
          (t
           (setf body
                 (if (eq 'ast-module-definition definition-class)
                     (parse-definition-list scanner state)
                     (parse-statement-list scanner state)))))

    ;; Create AST node.
    (setf ast (make-instance definition-class
                             :source-region (make-source-region start-position (scanner-position scanner))
                             :attributes attributes
                             :modifiers modifiers
                             :name name
                             :type-parameters type-parameters
                             :value-parameters value-parameters
                             :type type
                             :clauses clauses
                             :value value
                             :body body))

    (parse-result-match ast)))

(deftest test-parse-definition-simple-type ()
  (test-parser parse-definition "type Foobar;"
               :is-match-p t :expected-type 'ast-type-definition
               :checks ((test-equal "Foobar" (identifier-to-string (ast-definition-name ast))))))

(deftest test-parse-definition-rejects-non-definition ()
  (test-parser parse-definition "Foobar" :is-match-p nil))

(deftest test-parse-definition ()
  (test-parse-definition-simple-type)
  (test-parse-definition-rejects-non-definition))

;; -----------------------------------------------------------------------------
(defun parse-definition-list (scanner state)
  (parse-list parse-definition scanner state :start-delimiter #\{ :end-delimiter #\}))

;; -----------------------------------------------------------------------------
(defun parse-compilation-unit (scanner state)
  (let* ((start-position (scanner-position scanner))
         (definitions (parse-list parse-definition scanner state))
         (result (parse-result-match (make-instance 'ast-compilation-unit
                                                    :source-region (make-source-region start-position (scanner-position scanner))
                                                    :definitions (if (parse-result-no-match-p definitions)
                                                                     (make-instance 'ast-list :source-region (make-source-region start-position start-position))
                                                                     (parse-result-value definitions))))))
    (if (not (scanner-at-end-p scanner))
        (not-implemented "parse error; unrecognized input"))
    result))

(deftest test-parse-compilation-unit-empty ()
  (test-parser parse-compilation-unit "" :is-match-p t :expected-type 'ast-compilation-unit
               :checks
               ((test-type 'ast-list (ast-unit-definitions ast))
                (test-equal nil (ast-list-nodes (ast-unit-definitions ast))))))

(deftest test-parse-compilation-unit-simple ()
  (test-parser parse-compilation-unit "type Foobar; type Barfoo;" :is-match-p t :expected-type 'ast-compilation-unit
               :checks
               ((test-type 'ast-list (ast-unit-definitions ast))
                (test-equal 2 (length (ast-list-nodes (ast-unit-definitions ast)))))))

(deftest test-parse-compilation-unit-consumes-all-input ()
  (test-parser parse-compilation-unit "type Foobar; fdklajskfl$#@%@@#% FDSF" :is-match-p nil))

(deftest test-parse-compilation-unit ()
  (test-parse-compilation-unit-empty)
  (test-parse-compilation-unit-simple)
  (test-parse-compilation-unit-consumes-all-input))

;; -----------------------------------------------------------------------------
(defsuite test-parsers ()
  (test-parse-whitespace)
  (test-parse-list)
  (test-parse-modifier)
  (test-parse-identifier)
  (test-parse-literal)
  (test-parse-type)
  (test-parse-expression)
  (test-parse-statement)
  (test-parse-definition)
  (test-parse-compilation-unit-simple))

;;;;============================================================================
;;;;    Emitter.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; A block of emitted Lisp code.
(defclass emitter-block ()
  ((head
    :initform nil)
   (tail
    :initform nil)
   (name
    :initform nil)))

;; -----------------------------------------------------------------------------
(defclass emitter-state ()
  ((blocks
    :reader emitter-blocks
    :initform (progn
                (let ((array (make-array 10 :adjustable t :fill-pointer t :element-type 'emitter-block)))
                  (vector-push-extend (make-instance 'emitter-block) array)
                  array)))
   (package-name
    :reader emitter-package-name
    :initarg :package-name
    :initform :flux-program)
   functions
   types))

;; -----------------------------------------------------------------------------
(defparameter *object-class-name* (intern "Object"))

;; -----------------------------------------------------------------------------
(defun current-emitter-block (state)
  (let ((blocks (slot-value state 'blocks)))
    (elt blocks (1- (length blocks)))))

(deftest test-current-emitter-block ()
  (test (current-emitter-block (make-instance 'emitter-state))))

;; -----------------------------------------------------------------------------
(defun current-emitter-block-head (state)
  (slot-value (current-emitter-block state) 'head))
  
;; -----------------------------------------------------------------------------
(defun current-emitter-block-tail (state)
  (slot-value (current-emitter-block state) 'tail))
  
;; -----------------------------------------------------------------------------
(defun set-current-block-name (state name)
  (setf (slot-value (current-emitter-block state) 'name) name))

;; -----------------------------------------------------------------------------
(defun get-current-block-name (state)
  (slot-value (current-emitter-block state) 'name))

;; -----------------------------------------------------------------------------
(defun get-emitted-code (state)
  (current-emitter-block-head state))

;; -----------------------------------------------------------------------------
(defun push-emitter-block (state &key name)
  (let ((block (make-instance 'emitter-block)))
    (vector-push-extend block (slot-value state 'blocks))
    (if name
        (set-current-block-name state name))
    block))

;; -----------------------------------------------------------------------------
(defun pop-emitter-block (state)
  (vector-pop (slot-value state 'blocks)))

;; -----------------------------------------------------------------------------
(defun append-code-to-current-emitter-block (state list)
  (let* ((block (current-emitter-block state))
         (head (slot-value block 'head)))
    (if (not head)
        (progn
          (setf (slot-value block 'head) list)
          (setf (slot-value block 'tail) (last list)))
        (progn
          (setf (cdr (slot-value block 'tail)) list)
          (setf (slot-value block 'tail) (last list)))))
  list)

(deftest test-append-code-to-current-emitter-block ()
  (let ((state (make-instance 'emitter-state))
        (list1 (list 1 2 3))
        (list2 (list 4 5 6)))
    (append-code-to-current-emitter-block state list1)
    (test (eq (current-emitter-block-head state) list1))
    (test (eq (current-emitter-block-tail state) (last list1)))

    (append-code-to-current-emitter-block state list2)
    (test (eq (current-emitter-block-head state) list1))
    (test (eq (current-emitter-block-tail state) (last list2)))
    (test-equal (list 1 2 3 4 5 6) (current-emitter-block-head state))))

;; -----------------------------------------------------------------------------
(defmacro test-emitter (code parser lambda-list &body checks)
  (with-gensyms (ast parser-value state result)
    `(let* ((,parser-value ,parser)
            (,ast (funcall ,parser-value
                           (make-string-scanner ,code)
                           (make-instance 'parser-state)))
            (,state (make-instance 'emitter-state))
            (,result (emit ,ast ,state)))
       ,(if (not (listp lambda-list))
            `(let ((,lambda-list ,result))
               ,@checks)
            `(destructuring-bind ,lambda-list
                 ,result
               ,@checks)))))

;; -----------------------------------------------------------------------------
(defgeneric emit (ast state))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-nothing-type) state)
  (intern "Nothing"))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-named-type) state)
  (identifier-to-lisp (ast-type-name ast)))

(deftest test-emit-named-type-simple ()
  (test-emitter
      "Foobar"
      #'parse-type
      name
    (test-equal "Foobar" (symbol-name name))))

(defsuite test-emit-named-type ()
  (test-emit-named-type-simple))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-integer-literal) state)
  (ast-literal-value ast))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-return-statement) state)
  (let* ((value-expression (ast-return-expression ast))
         (value (if value-expression
                  (emit value-expression state)
                  nil))
         (code `(return-from ,(get-current-block-name state) ,value)))
    (append-code-to-current-emitter-block state code)))

(deftest test-emit-return-statement-with-simple-value ()
  (test-emitter
    "return 0;"
    #'parse-statement
    (operator name value)
    ()))

(defsuite test-emit-return-statement ()
  (test-emit-return-statement-with-simple-value))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-type-definition) state)
  (let* ((name (identifier-to-lisp (ast-definition-name ast)))
         ;;////TODO: need to properly look up names
         (superclasses (if (not (ast-definition-type ast))
                           (if (not (eq name *object-class-name*))
                               (list *object-class-name*)
                               ())
                           (list (emit (ast-definition-type ast) state))))
         (class `(defclass ,name ,superclasses ())))
    (append-code-to-current-emitter-block state (list class))
    (if (definition-abstract-p ast)
        (let ((error-message (format nil "Cannot instantiate abstract class ~a!"
                                     (identifier-to-string (ast-definition-name ast)))))
          (append-code-to-current-emitter-block state
                        (list `(defmethod make-instance :before ((instance ,name) &key)
                                 (error ,error-message))))))
    class))

(deftest test-emit-type-definition-simple ()
  (test-emitter
      "type Foobar;"
      #'parse-definition
      (operator name (superclass) slots)
    (test-equal 'defclass operator)
    (test-equal "Foobar" (symbol-name name))
    (test-equal *object-class-name* superclass)))

(deftest test-emit-type-definition-single-supertype ()
  (test-emitter
      "type Foobar : Barfoo;"
      #'parse-definition
      (operator name (superclass) slots)
    (test-equal "Barfoo" (symbol-name superclass))))

(defsuite test-emit-type-definition ()
  (test-emit-type-definition-simple)
  (test-emit-type-definition-single-supertype))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-function-definition) state)
  (let ((name (identifier-to-lisp (ast-definition-name ast))))
    (push-emitter-block state :name name)
    (let* ((body-ast  (ast-definition-body ast))
           (body (if body-ast
                   (mapcan (lambda (statement) (emit statement state)) (ast-list-nodes body-ast))
                   nil))
           (method `(defmethod ,name () ,body)))
    (pop-emitter-block state)
    (append-code-to-current-emitter-block state (list method))
    method)))

(deftest test-emit-function-definition-simple ()
  (test-emitter
      "method Foobar() {}"
      #'parse-definition
      (operator name () body)
    (test-equal 'defmethod operator)
    (test-equal "Foobar" (symbol-name name))))

(defsuite test-emit-function-definition ()
  (test-emit-function-definition-simple))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-field-definition) state)
  (not-implemented "emitting fields"))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-module-definition) state)
  (let ((body (ast-definition-body ast)))
    (if body
        (mapcar (lambda (definition) (emit definition state)) (ast-list-nodes body)))))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-compilation-unit) state)
  "Emit code for compilation unit."

  ;; Emit prologue.
  (let ((package-name (emitter-package-name state)))
    (append-code-to-current-emitter-block state
                  `((in-package :cl-user)
                    (defpackage ,package-name
                      (:use :common-lisp)
                      (:export :__main))
                    (in-package ,package-name))))

  ;; Emit definitions.
  (mapc (lambda (definition) (emit definition state))
        (ast-list-nodes (ast-unit-definitions ast)))

  (get-emitted-code state))

(deftest test-emit-compilation-unit-prologue ()
  (test-emitter
      ""
      #'parse-compilation-unit
      (&rest code)
    (test (find `(in-package :flux-program) code :test #'equal))))

(defsuite test-emit-compilation-unit ()
  (test-emit-compilation-unit-prologue))

;; -----------------------------------------------------------------------------
(defsuite test-emitters ()
  (test-current-emitter-block)
  (test-append-code-to-current-emitter-block)
  (test-emit-type-definition)
  (test-emit-function-definition)
  (test-emit-compilation-unit))

;;;;============================================================================
;;;;    Regression Testing.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass regression-spec ()
  ((type
    :reader regression-spec-type
    :initarg :type)
   (data
    :reader regression-spec-data
    :initarg :data)))

;; -----------------------------------------------------------------------------
(defgeneric verify-no-regression (regression-spec-type regression-spec source
                                                       emitter-state generated-code))

;; -----------------------------------------------------------------------------
(defmethod verify-no-regression ((regression-spec-type (eql :type)) regression-spec source
                                                                    emitter-state generated-code)
  ())

;; -----------------------------------------------------------------------------
(defmethod verify-no-regression ((regression-spec-type (eql :function)) regression-spec source
                                                                        emitter-state generated-code)
  ())

;; -----------------------------------------------------------------------------
(defmethod verify-no-regression ((regression-spec-type (eql :result)) regression-spec source
                                                                      emitter-state generated-code)
  ())

;; -----------------------------------------------------------------------------
(defun parse-spec-ignored-text (scanner state)
  (let ((saved-position (scanner-position scanner))
        char)
    (loop
       (if (or (scanner-at-end-p scanner)
               (scanner-match-sequence scanner "/*#"))
           (return))
       (let ((char (scanner-read-next scanner)))
         (if (or (equal char #\Return)
                 (equal char #\Newline))
             (progn
               (if (equal char #\Return)
                   (scanner-match scanner #\Newline))
               (add-line-break (line-break-table state) (scanner-position scanner))))))
    (if (equal (scanner-position scanner)
               saved-position)
        (parse-result-no-match)
        (parse-result-match))))

(deftest test-parse-spec-ignored-text ()
  (test-parser parse-spec-ignored-text (format nil "fooi /* fo~Co */ ;[] /*#FOO" #\Newline)
               :is-match-p t :end-position 23 :line-breaks-at '(0 11)))

;; -----------------------------------------------------------------------------
(defun parse-spec-key (scanner state)
  (let ((string (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character)))
    (loop
       (if (or (scanner-at-end-p scanner)
               (not (alpha-char-p (scanner-peek-next scanner))))
           (return))
       (vector-push-extend (scanner-read-next scanner) string))
    (if (zerop (length string))
        (parse-result-no-match)
        (parse-result-match (intern (string-upcase string) :keyword)))))

(deftest test-parse-spec-key ()
  (test-parser parse-spec-key "foo" :is-match-p t :end-position 3 :expected-type 'symbol
               :checks ((test-equal :foo ast))))

;; -----------------------------------------------------------------------------
(defun parse-spec-value (scanner state)
  (let ((string (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character)))
    (loop
       (if (or (scanner-at-end-p scanner)
               (whitespace-char-p (scanner-peek-next scanner)))
           (return))
       (vector-push-extend (scanner-read-next scanner) string))
    (if (zerop (length string))
        (parse-result-no-match)
        (parse-result-match string))))

(deftest test-parse-spec-value ()
  (test-parser parse-spec-value "10" :is-match-p t :end-position 2 :expected-type 'string))

;; -----------------------------------------------------------------------------
(defun parse-spec-key-value-pair (scanner state)
  (let (key value)
    (setf key (parse-spec-key scanner state))
    (if (not (parse-result-match-p key))
        (return-from parse-spec-key-value-pair (parse-result-no-match)))
    (if (scanner-match scanner #\=)
        (progn
          (setf value (parse-spec-value scanner state))
          (if (not (parse-result-match-p value))
              (not-implemented "expecting value"))))
    (parse-result-match (cons (parse-result-value key)
                              (parse-result-value value)))))

(deftest test-parse-spec-key-value-pair ()
  (test-parser parse-spec-key-value-pair "foo=bar" :is-match-p t :end-position 7
               :checks ((test (listp ast))
                        (test-equal :foo (car ast))
                        (test-equal "bar" (cdr ast)))))

;; -----------------------------------------------------------------------------
(defun parse-spec-key-value-pair-list (scanner state)
  (parse-list parse-spec-key-value-pair scanner state))

(deftest test-parse-spec-key-value-pair-list ()
  (test-parser parse-spec-key-value-pair-list "foo=1 bar=2" :is-match-p t :end-position 11))

;; -----------------------------------------------------------------------------
(defun parse-spec-list (scanner state)
  (let (list spec spec-type values)
    (loop
       (parse-spec-ignored-text scanner state)
       (if (scanner-at-end-p scanner)
           (return))

       (setf spec-type (cond ((scanner-match-sequence scanner "TYPE:") :type)
                             ((scanner-match-sequence scanner "FUNCTION:") :function)
                             ((scanner-match-sequence scanner "RESULT:") :result)
                             (t (not-implemented "Unrecognized regression spec type"))))

       (setf values (parse-spec-key-value-pair-list scanner state))
       (if (not (parse-result-match values))
           (not-implemented "parse error; expecting list of key/value pairs"))

       (setf spec (make-instance 'regression-spec
                                 :type spec-type
                                 :data (parse-result-value values)))

       (setf list (cons spec list))
       (setf spec nil)

       (parse-whitespace scanner state)
       (if (not (scanner-match-sequence scanner "*/"))
           (not-implemented "Unterminated regression spec")))
    (parse-result-match list)))

(deftest test-parse-spec-list ()
  (test-parser parse-spec-list "/*#TYPE: Foobar */" :is-match-p t :expected-type 'list
               :checks ((test-type 'regression-spec (car ast)))))

;; -----------------------------------------------------------------------------
(defsuite test-spec-parsers ()
  (test-parse-spec-ignored-text)
  (test-parse-spec-key)
  (test-parse-spec-value)
  (test-parse-spec-key-value-pair)
  (test-parse-spec-key-value-pair-list)
  (test-parse-spec-list))

;; -----------------------------------------------------------------------------
(defun run-regression-tests ()
  (flet ((test-file (pathname)
           (if (find (pathname-type pathname) *flux-file-extensions* :test #'equal)
               (progn
                 (print (format nil "--- ~a ---" pathname))
                 (let* ((test-package-name :flux-regression-tests)
                        (source (make-source pathname))
                        (regression-specs (parse-result-value (parse-spec-list (make-string-scanner (source-text source)) (make-instance 'parser-state))))
                        (emitter-state (make-instance 'emitter-state
                                                      :package-name test-package-name))
                        ast
                        code)
                   (parse source)
                   (setf code (emit (source-ast source) emitter-state))
                   (pprint code)
                   (mapc (lambda (spec) (verify-no-regression (regression-spec-type spec) spec source emitter-state code)) regression-specs)
                   (mapc #'eval code)
                   (in-package :flux)
                   (delete-package test-package-name))))))
    (walk-directory *regression-suite-directory* #'test-file :directories nil))
  (in-package :flux))

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
(defmethod parse ((code source) &key)
  (let* ((state (make-instance 'parser-state
                               :line-break-table (source-line-breaks code)
                               :diagnostics (source-diagnostics code)))
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
(defmethod parse ((code string) &key)
  (parse (make-source code)))

(deftest test-parse-code-string ()
  (destructuring-bind (ast)
      (parse "type Foobar;")
    (test (typep ast 'ast-compilation-unit))))

;; -----------------------------------------------------------------------------
(defmethod parse ((code pathname) &key)
  (if (directory-pathname-p code)
      (not-implemented "Compiling entire directories")
      (parse (make-source code))))

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
