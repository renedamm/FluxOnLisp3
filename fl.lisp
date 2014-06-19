; This code is pretty much the opposite of idiomatic Lisp...

; Turn off optimizations to help debugging code.
(declaim (optimize (speed 0)
                   (safety 3)
                   (debug 3)))

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

(defparameter *flux-default-package* (defpackage :flux-program
                                       (:use :common-lisp)))

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
(defmacro test-equal (expected actual &key (equal 'equal))
  (with-gensyms (expected-value actual-value)
    `(let ((,expected-value ,expected)
           (,actual-value ,actual))
       (if (,equal ,expected-value ,actual-value)
           (report-result t '(equal ,expected ,actual))
           (report-result nil (format nil "Expected ~a from ~a but got ~a" ,expected-value ',actual ,actual))))))

;; -----------------------------------------------------------------------------
(defmacro test-same (expected actual)
  `(test-equal ,expected ,actual :equal eq))

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
(defmacro lazy-initialize-slot (instance slot-name &body init-form)
  (with-gensyms (instance-value slot-name-value slot-value)
    `(let* ((,instance-value ,instance)
            (,slot-name-value ,slot-name)
            (,slot-value (slot-value ,instance-value ,slot-name-value)))
       (if (not ,slot-value)
         (setf ,slot-value
               (setf (slot-value ,instance-value ,slot-name-value)
                     (progn
                       ,@init-form))))
       ,slot-value)))
     
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
  (if (typep right 'scanner)
    (setf right (scanner-position right)))
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
    :initform nil)
   (local-scope
     :reader ast-local-scope)))

;; -----------------------------------------------------------------------------
;; Representation of "::" denoting the global namespace.
(defclass ast-global-qualifier (ast-node)
  ())

;; -----------------------------------------------------------------------------
;; A (potentially qualified) identifier.
(defclass ast-identifier (ast-node)
  ((qualifier
    :reader ast-id-qualifier
    :initarg :qualifier
    :initform nil)
   (name
    :reader ast-id-name
    :initarg :name)))

(defmethod initialize-instance :after ((id ast-identifier) &key)
  (let ((name (ast-id-name id)))
    (if (not (symbolp name))
      (error (format nil "Expecting symbol for name of identifier but got '~a' instead." name)))))

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
(defclass ast-external-modifier (ast-modifier)
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
(defparameter *global-qualifier* '|::|)

;; -----------------------------------------------------------------------------
(defun make-ast-error (diagnostic)
  (make-instance 'ast-error
                 :source-region (diagnostic-source-region diagnostic)
                 :diagnostic diagnostic))

;; -----------------------------------------------------------------------------
(defun make-identifier (&rest names)
  (assert (>= (length names) 1))
  (cond
    ((eq (car names) *global-qualifier*)
     (let ((id (apply #'make-identifier (cdr names))))
       (setf (slot-value id 'qualifier) (make-instance 'ast-global-qualifier))
       id))
    ((cdr names)
     (let ((id (apply #'make-identifier (cdr names))))
       (setf (slot-value id 'qualifier) (make-instance 'ast-identifier :name (car names)))
       id))
    (t
     (make-instance 'ast-identifier :name (car names)))))

(deftest test-make-identifier-simple ()
  (let ((id (make-identifier 'test)))
    (test-equal "TEST" (symbol-name (ast-id-name id)))
    (test-equal nil (ast-id-qualifier id))))

(deftest test-make-identifier-with-global-qualifier ()
  (let ((id (make-identifier *global-qualifier* 'test)))
    (test-equal "TEST" (symbol-name (ast-id-name id)))
    (test-type 'ast-global-qualifier (ast-id-qualifier id))))

(deftest test-make-identifier-with-namespace ()
  (let ((id (make-identifier 'outer 'inner)))
    (test-equal "INNER" (symbol-name (ast-id-name id)))
    (test-type 'ast-identifier (ast-id-qualifier id))
    (test-equal "OUTER" (symbol-name (ast-id-name (ast-id-qualifier id))))))

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
  (let ((id1 (make-instance 'ast-identifier :name (intern "test"))))
    (test-equal "test" (identifier-to-string id1))))

;; -----------------------------------------------------------------------------
;;////TODO: need to do proper symbol lookups and generate fully qualified IDs for every use
(defun identifier-to-lisp (id)
  (intern (identifier-to-string id)))

;; -----------------------------------------------------------------------------
(defun definition-has-modifier-p (ast type)
  (let ((modifiers (ast-definition-modifiers ast)))
    (if (not modifiers)
        nil
        (find-if (lambda (modifier) (typep modifier type)) (ast-list-nodes modifiers)))))

(deftest test-definition-has-modifier-p ()
  (let ((abstract-ast (parse-result-value
                        (parse-definition (make-string-scanner "abstract type Foobar;")
                                          (make-instance 'parser-state))))
        (plain-ast (parse-result-value
                     (parse-definition (make-string-scanner "type Foobar;")
                                       (make-instance 'parser-state)))))
    (test (definition-has-modifier-p abstract-ast 'ast-abstract-modifier))
    (test (not (definition-has-modifier-p plain-ast 'ast-abstract-modifier)))))

;; -----------------------------------------------------------------------------
(defun definition-abstract-p (ast)
  (definition-has-modifier-p ast 'ast-abstract-modifier))

;; -----------------------------------------------------------------------------
(defsuite test-asts ()
  (test-make-identifier-simple)
  (test-make-identifier-with-global-qualifier)
  (test-make-identifier-with-namespace)
  (test-identifier-to-string)
  (test-definition-has-modifier-p))

;;;;============================================================================
;;;;    Symbol Tables.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *declaration-kind-function* 'functions)
(defparameter *declaration-kind-variable* 'variables)
(defparameter *declaration-kind-module* 'modules)
(defparameter *declaration-kind-type* 'types)

;; -----------------------------------------------------------------------------
(defclass declaration ()
  ((namespace
     :reader declaration-namespace
     :initarg :namespace)
   (name
     :reader declaration-name
     :initarg :name)
   (definitions
     :reader declaration-definitions
     :initform nil)))

;; -----------------------------------------------------------------------------
(defclass scope ()
  ((modules
     :initform nil)
   (types
     :initform nil)
   (functions
     :initform nil)
   (variables
     :initform nil)))

;; -----------------------------------------------------------------------------
(defclass namespace ()
  ((name
     :reader namespace-name
     :initarg :name
     :initform nil)
   (parent-namespace
     :reader namespace-parent
     :initarg :parent
     :initform nil)
   (scope
     :reader namespace-scope
     :initarg :scope
     :initform nil)
   (children
     :reader namespace-children
     :initform nil)
   (declarations
     :reader namespace-declarations
     :initform nil)))

;; -----------------------------------------------------------------------------
(defparameter *namespace-separator* "::")

;; -----------------------------------------------------------------------------
(defun namespace-qualified-name (namespace)
  (let ((parent (namespace-parent namespace)))
    (cond
      ((not parent)
       "")
      
      ((not (namespace-parent parent))
       (symbol-name (namespace-name namespace)))
      
      (t
       (concatenate 'string
                    (namespace-qualified-name parent)
                    *namespace-separator*
                    (symbol-name (namespace-name namespace)))))))

(deftest test-namespace-qualified-name ()
  (let* ((global (make-instance 'namespace))
         (outer (make-instance 'namespace :name 'outer :parent global))
         (inner (make-instance 'namespace :name 'inner :parent outer)))
    (test-equal "" (namespace-qualified-name global))
    (test-equal "OUTER" (namespace-qualified-name outer))
    (test-equal "OUTER::INNER" (namespace-qualified-name inner))))

;; -----------------------------------------------------------------------------
(defun declaration-qualified-name (declaration)
  (let ((namespace (namespace-qualified-name (declaration-namespace declaration)))
        (name (symbol-name (declaration-name declaration))))
    (if (zerop (length namespace))
      name
      (concatenate 'string namespace *namespace-separator* name))))

(deftest test-declaration-qualified-name ()
  (let* ((global (make-instance 'namespace))
         (namespace (make-instance 'namespace :name 'namespace :parent global))
         (declaration (make-instance 'declaration :name 'test :namespace namespace)))
    (test-equal "NAMESPACE::TEST" (declaration-qualified-name declaration))))

;; -----------------------------------------------------------------------------
(defun make-scope-stack ()
  (make-array 10 :element-type 'scope :adjustable t :fill-pointer 0))

;; -----------------------------------------------------------------------------
(defun push-scope (scope-stack &optional scope)
  (if (not scope)
    (setf scope (make-instance 'scope)))
  (vector-push-extend scope scope-stack)
  scope)

;; -----------------------------------------------------------------------------
(defun pop-scope (scope-stack)
  (assert (>= (length scope-stack) 1))
  (vector-pop scope-stack))

;; -----------------------------------------------------------------------------
(defun current-scope (scope-stack &key (index 0))
  (let* ((scope-index (- (1- (length scope-stack)) index)))
    (assert (>= scope-index 0))
    (elt scope-stack scope-index)))

(deftest test-current-scope ()
  (let* ((stack (make-scope-stack))
         (global-scope (make-instance 'scope))
         (local-scope (make-instance 'scope)))
    (push-scope stack global-scope)
    (push-scope stack local-scope)
    (test-same local-scope (current-scope stack))
    (test-same global-scope (current-scope stack :index 1))))

;; -----------------------------------------------------------------------------
(defun add-definition (declaration definition)
   (setf (slot-value declaration 'definitions)
         (cons definition
               (slot-value declaration 'definitions))))

;; -----------------------------------------------------------------------------
(defun find-declaration (namespace name)
  (let ((declarations (namespace-declarations namespace)))
    (if declarations
      (gethash name declarations)
      nil)))

;; -----------------------------------------------------------------------------
(defun find-or-insert-declaration (namespace name)
  (let* ((declarations (lazy-initialize-slot namespace 'declarations
                         (make-hash-table)))
         (declaration (gethash name declarations)))
    (if (not declaration)
      (setf declaration
            (setf (gethash name declarations)
                  (make-instance 'declaration
                                 :name name
                                 :namespace namespace))))
    declaration))

(deftest test-find-or-insert-declaration ()
  (let* ((namespace (make-instance 'namespace))
         (declaration1 (find-or-insert-declaration namespace 'test-function))
         (declaration2 (find-or-insert-declaration namespace 'test-function)))
    (test-type 'declaration declaration1)
    (test-type 'declaration declaration2)
    (test-same declaration1 declaration2)
    (test-equal declaration1 (find-declaration namespace 'test-function))))

;; -----------------------------------------------------------------------------
(defun create-child-namespace (parent-namespace name)
  (let ((child-namespace (make-instance 'namespace
                                        :name name
                                        :parent parent-namespace
                                        :scope (namespace-scope parent-namespace))))
    (setf (slot-value parent-namespace 'children)
          (cons child-namespace
                (slot-value parent-namespace 'children)))
    child-namespace))

(deftest test-create-child-namespace ()
  (let* ((parent (make-instance 'namespace
                                :name 'outer))
         (child (create-child-namespace parent 'inner)))
    (test-type 'namespace child)
    (test-equal 'inner (namespace-name child))
    (test-sequence-equal (list child) (namespace-children parent))))

;; -----------------------------------------------------------------------------
(defun find-child-namespace (parent-namespace name)
  (find-if (lambda (child-namespace)
             (equal (namespace-name child-namespace) name))
           (namespace-children parent-namespace)))

;; -----------------------------------------------------------------------------
(defun find-nested-child-namespace (parent-namespace identifier &key if-does-not-exist)
  (let ((qualifier (ast-id-qualifier identifier))
        (inner-namespace parent-namespace))
    (if qualifier
      (setf inner-namespace (find-nested-child-namespace parent-namespace
                                                         qualifier
                                                         :if-does-not-exist if-does-not-exist)))
    (if (not inner-namespace)
      nil
      (let* ((name (ast-id-name identifier))
             (child-namespace (find-child-namespace inner-namespace name)))
        (if child-namespace
          child-namespace
          (cond
            ((eq if-does-not-exist :create)
             (create-child-namespace inner-namespace name))
            (t nil)))))))

(deftest test-find-nested-child-namespace-returns-nil ()
  (let ((parent (make-instance 'namespace
                               :name 'outer)))
    (test-equal nil (find-nested-child-namespace parent
                                                 (make-instance 'ast-identifier
                                                                :name 'inner)))))

(deftest test-find-nested-child-namespace-create-if-does-not-exist ()
  (let* ((parent (make-instance 'namespace
                                :name 'outermost))
         (child (find-nested-child-namespace parent
                                             (make-instance 'ast-identifier
                                                            :name 'innermost
                                                            :qualifier (make-instance 'ast-identifier
                                                                                      :name 'inner))
                                             :if-does-not-exist :create)))
    (test-type 'namespace child)
    (test-equal 'innermost (namespace-name child))
    (test-equal 'inner (namespace-name (namespace-parent child)))
    (test-equal 'outermost (namespace-name (namespace-parent (namespace-parent child))))))

;; -----------------------------------------------------------------------------
(defun get-namespace-for-declarations (scope declaration-kind)
  (macrolet
    ((lazy-initialize (slot-name)
       `(lazy-initialize-slot scope ,slot-name
                             (make-instance 'namespace
                                             :scope scope))))
  (cond
    ((eq declaration-kind *declaration-kind-function*)
     (lazy-initialize 'functions))
    ((eq declaration-kind *declaration-kind-variable*)
     (lazy-initialize 'variables))
    ((eq declaration-kind *declaration-kind-module*)
     (lazy-initialize 'modules))
    ((eq declaration-kind *declaration-kind-type*)
     (lazy-initialize 'types))
    (t
     (error (format nil "Unknown declaration kind '~a'" declaration-kind))))))

(deftest test-get-namespace-for-declarations ()
  (let ((scope (make-instance 'scope)))
    (test-type 'namespace (get-namespace-for-declarations scope *declaration-kind-function*))
    (test-type 'namespace (get-namespace-for-declarations scope *declaration-kind-variable*))
    (test-type 'namespace (get-namespace-for-declarations scope *declaration-kind-module*))
    (test-type 'namespace (get-namespace-for-declarations scope *declaration-kind-type*))))

;; -----------------------------------------------------------------------------
(defun lookup-namespace (scope declaration-kind identifier &key if-does-not-exist)
  (find-nested-child-namespace (get-namespace-for-declarations scope declaration-kind)
                               identifier
                               :if-does-not-exist if-does-not-exist))

(deftest test-lookup-namespace-simple ()
  (let* ((scope (make-instance 'scope))
         (namespace (lookup-namespace scope
                                      *declaration-kind-function*
                                      (make-instance 'ast-identifier
                                                     :name 'inner
                                                     :qualifier (make-instance 'ast-identifier
                                                                               :name 'outer))
                                      :if-does-not-exist :create)))
    (test-type 'namespace namespace)
    (test-equal 'inner (namespace-name namespace))
    (test-equal (get-namespace-for-declarations scope *declaration-kind-function*)
                (namespace-parent (namespace-parent namespace)))))

;; -----------------------------------------------------------------------------
(defun find-or-create-declaration (scope declaration-kind identifier)
  (let* ((top-namespace (get-namespace-for-declarations scope declaration-kind))
         (qualifier (ast-id-qualifier identifier))
         (name (ast-id-name identifier))
         (namespace (if qualifier
                      (find-nested-child-namespace top-namespace qualifier :if-does-not-exist :create)
                      top-namespace)))
    (find-or-insert-declaration namespace name)))

(deftest test-find-or-create-declaration ()
  (let* ((scope (make-instance 'scope))
         (declaration (find-or-create-declaration scope *declaration-kind-function* (make-identifier 'my-function))))
    (test-type 'declaration declaration)
    (test-equal 'my-function (declaration-name declaration))))

;; -----------------------------------------------------------------------------
(defun lookup-declaration (scope-stack declaration-kind identifier &key current-namespace)
  (let* ((qualifier (ast-id-qualifier identifier))
         (name (ast-id-name identifier))
         (is-globally-qualified (typep qualifier 'ast-global-qualifier)))

    (if is-globally-qualified
      (setf qualifier (ast-id-qualifier qualifier)))

    (flet ((lookup-in-scope (scope)
             (let* ((starting-namespace
                      (cond
                        (is-globally-qualified
                          (get-namespace-for-declarations scope declaration-kind))
                        ((and current-namespace
                              (eq (namespace-scope current-namespace) scope))
                         current-namespace)
                        (current-namespace
                          (not-implemented "finding corresponding namespace in different scope"))
                        (t
                          (get-namespace-for-declarations scope declaration-kind))))
                    (target-namespace
                      (cond
                        (qualifier
                          (find-nested-child-namespace starting-namespace qualifier))
                        (t
                          starting-namespace))))
               (if target-namespace
                 (find-declaration target-namespace name)))))

    (dotimes (scope-index (length scope-stack))
      (let* ((current-scope (current-scope scope-stack :index scope-index))
             (declaration (lookup-in-scope current-scope)))
        (if declaration
          (return declaration)))))))

(deftest test-lookup-declaration-in-current-scope ()
  (let* ((id (make-identifier 'my-function))
         (scope-stack (make-scope-stack))
         (scope (push-scope scope-stack))
         (declaration (find-or-create-declaration scope *declaration-kind-function* id)))
    (test-same declaration (lookup-declaration scope-stack *declaration-kind-function* id))))

;; -----------------------------------------------------------------------------
(defsuite test-symbol-table ()
  (test-namespace-qualified-name)
  (test-declaration-qualified-name)
  (test-find-or-insert-declaration)
  (test-create-child-namespace)
  (test-find-nested-child-namespace-returns-nil)
  (test-find-nested-child-namespace-create-if-does-not-exist)
  (test-get-namespace-for-declarations)
  (test-lookup-namespace-simple)
  (test-find-or-create-declaration)
  (test-lookup-declaration-in-current-scope))

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
;;;;    Name Mangling.
;;;;============================================================================

;; Once we have resolved a name to the declaration it refers to, we need to
;; emit a Lisp identifier that uniquely refers to that declaration.

;; -----------------------------------------------------------------------------
(defun mangled-name (declaration &key in-package)
  (let ((qualified-name (declaration-qualified-name declaration)))
    (if in-package
      (intern qualified-name in-package)
      (intern qualified-name))))

(deftest test-mangled-name-simple ()
  (let* ((scope (make-instance 'scope))
         (namespace (get-namespace-for-declarations scope *declaration-kind-function*))
         (declaration (find-or-create-declaration scope *declaration-kind-function* (make-identifier 'test))))
    (declare (ignore namespace))
    (test-equal '|TEST| (mangled-name declaration))))

(defsuite test-mangled-name ()
  (test-mangled-name-simple))

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
  (test (scanner-match-if (make-string-scanner "foo" ) (lambda (char) (declare (ignore char)) t)))
  (test (not (scanner-match-if (make-string-scanner "foo") (lambda (char) (declare (ignore char)) nil)))))

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
      (error (format nil "Position ~a out of range for string ~a used by string-scanner!"
                     position
                     (slot-value scanner 'string)))))

(deftest test-string-scanner-setf-position ()
  (let ((scanner (make-string-scanner "foo")))
    (setf (scanner-position scanner) 0)
    (test-equal 0 (scanner-position scanner))
    (setf (scanner-position scanner) 3)
    (test-equal 3 (scanner-position scanner))))

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
;;;;    Parser Actions
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun insert-declaration-into-symbol-table (declaration-kind ast parser-state &key (scope-index 0))
  (let ((declaration (find-or-create-declaration
                      (current-scope (scope-stack parser-state) :index scope-index)
                      declaration-kind
                      (ast-definition-name ast))))
    (add-definition declaration ast)))
  
;; -----------------------------------------------------------------------------
;; The default parse action is to just create an AST node whose type corresponds
;; to the name of the production and which is initialized from the given arguments.
(defmethod parse-action ((production symbol) region state &rest rest)
  (declare (ignore state))
  (apply 'make-instance (nconc (list production
                                     :source-region region)
                               rest)))

;; -----------------------------------------------------------------------------
(defmethod parse-action ((production (eql 'ast-function-definition)) region state &rest rest)
  (declare (ignore rest region))
  (let* ((ast (call-next-method)))
    (insert-declaration-into-symbol-table *declaration-kind-function* ast state)
    ast))
 
;; -----------------------------------------------------------------------------
(defmethod parse-action ((production (eql 'ast-module-definition)) region state &rest rest)
  (declare (ignore rest region))
  (let ((ast (call-next-method)))
    (insert-declaration-into-symbol-table *declaration-kind-module* ast state)
    ast))

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
     :initarg :diagnostics)
   (package-for-symbols
     :reader package-for-symbols
     :initarg :package-for-symbols
     :initform *flux-default-package*)
   (scope-stack
     :reader scope-stack
     :initform (make-scope-stack))))

(defmethod initialize-instance :after ((instance parser-state) &key)

  ;; Make sure that package-for-symbols is an existing package.
  (let* ((symbol-package-name (package-for-symbols instance))
         (symbol-package (find-package symbol-package-name)))
    (if (not symbol-package)
      (progn
        (setf symbol-package (defpackage symbol-package-name (:use :common-lisp)))
        (setf (slot-value instance 'package-for-symbols) symbol-package))))
  
  ;; Set current scope to global scope.
  (push-scope (scope-stack instance)))

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
  (test-parser parse-whitespace
               (format nil " ~C~C~Cfoo" #\Return #\Newline #\Tab)
               :is-match-p t :end-position 4))

(deftest test-parse-whitespace-consumes-single-line-comments ()
  (test-parser parse-whitespace
               (format nil " // foo~Cbar" #\Newline)
               :is-match-p t :end-position 8 :line-breaks-at '(0 8)))

(deftest test-parse-whitespace-consume-multi-line-comments ()
  (test-parser parse-whitespace
               (format nil " /* foo /* ~C bar */ */foo" #\Newline)
               :is-match-p t :end-position 22 :line-breaks-at '(0 12)))

(deftest test-parse-whitespace ()
  (test-parse-whitespace-does-not-consume-non-whitespace)
  (test-parse-whitespace-consumes-whitespace)
  (test-parse-whitespace-consumes-single-line-comments)
  (test-parse-whitespace-consume-multi-line-comments))

;; -----------------------------------------------------------------------------
;; Combinator that turns another parser into a list parser.  This combinator can be broken
;; down into more atomic combinators that can be composed to create the same effect as this
;; combinator here yet in a more flexible manner.  However, it works nicely for the parsers
;; we have.
(defmacro parse-list (parser-name scanner state &key start-delimiter end-delimiter separator)
  ;; List must either be undelimited or have both a start and end
  ;; delimiter.
  (with-gensyms (scanner-value state-value list start-position element parse-list-block)
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
           (parse-action 'ast-list
                         (make-source-region ,start-position ,scanner-value)
                         state
                         :nodes ,list))))))

(deftest test-parse-list ()
  (labels
      ;; Define a dummy parser that matches "a".
      ((parse-a (scanner state)
         (declare (ignore state))
         (if (scanner-match scanner #\a)
             (parse-result-match (make-instance 'ast-node
                                                :source-region (make-source-region
                                                                 (1- (scanner-position scanner))
                                                                 (scanner-position scanner))))
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
                                                    (parse-action ,ast-type
                                                                  (make-source-region saved-position scanner)
                                                                  state))))))
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
          (parse-result-match (parse-action 'ast-identifier
                                            (make-source-region start-position scanner)
                                            state
                                            :name (intern buffer (package-for-symbols state)))))
        (parse-result-no-match))))

(deftest test-parse-identifier-simple-name ()
  (test-parser parse-identifier "test"
               :end-position 4 :is-match-p t :expected-type 'ast-identifier
               :checks ((test-equal (intern "test" *flux-default-package*) (ast-id-name ast))
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
      (parse-result-match (parse-action clause-type
                                        (make-source-region saved-position scanner)
                                        state
                                        :expression (parse-result-value expression))))))

;; -----------------------------------------------------------------------------
(defun parse-clause-list (scanner state)
  (parse-list parse-clause scanner state))

;; -----------------------------------------------------------------------------
(defun parse-attribute (scanner state)
  (declare (ignore scanner state))
  (parse-result-no-match));////TODO

;; -----------------------------------------------------------------------------
(defun parse-attribute-list (scanner state)
  (parse-list parse-attribute scanner state :start-delimiter #\[ :separator #\, :end-delimiter #\]))

;; -----------------------------------------------------------------------------
(defun parse-type (scanner state)
  (let* ((saved-position (scanner-position scanner))
         (left-type (cond ((scanner-match scanner #\()
                           (parse-whitespace scanner state)
                           (if (scanner-match scanner #\))
                               (parse-result-match (parse-action 'ast-nothing-type
                                                                 (make-source-region saved-position scanner)
                                                                 state))
                               (not-implemented "parenthesized type expressions")))
                          (t
                           (let (modifiers name)
                             (setf modifiers (parse-modifier-list scanner state))
                             (parse-whitespace scanner state)
                             (setf name (parse-identifier scanner state))
                             (if (parse-result-no-match-p name)
                                 (not-implemented "parse error; expecting type name"))
                             (parse-result-match (parse-action 'ast-named-type
                                                               (make-source-region saved-position scanner)
                                                               state
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
        (parse-result-match (parse-action combination-type
                                          (make-source-region saved-position scanner)
                                          state
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
  (declare (ignore scanner state))
  (parse-result-no-match));////TODO

;; -----------------------------------------------------------------------------
(defun parse-type-parameter-list (scanner state)
  "Parse a list of type parameters surrounded by '<' and '>'."
  (parse-list parse-type-parameter scanner state :start-delimiter #\< :separator #\, :end-delimiter #\>))

;; -----------------------------------------------------------------------------
(defun parse-value-parameter (scanner state)
  (declare (ignore scanner state))
  (parse-result-no-match));////TODO

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
             (parse-result-match (parse-action 'ast-return-statement
                                               (make-source-region saved-position scanner)
                                               state
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
                 (parse-result-match (parse-action 'ast-integer-literal
                                                   (make-source-region saved-position scanner)
                                                   state
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
                 ;;////FIXME: this should be unified into one single parsing path for both module and other definitions
                 (if (eq 'ast-module-definition definition-class)
                     (parse-definition-list scanner state)
                     (parse-statement-list scanner state)))))

    ;; Create AST node.
    (setf ast (parse-action definition-class
                            (make-source-region start-position scanner)
                            state
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

(deftest test-parse-definition-simple-function ()
  (test-parser parse-definition "function Foobar : () -> () {}"
               :is-match-p t
               :expected-type 'ast-function-definition
               :checks ((test-equal "Foobar" (identifier-to-string (ast-definition-name ast))))))

(deftest test-parse-definition-simple-module ()
  (test-parser parse-definition "module Foobar {}"
               :is-match-p t
               :expected-type 'ast-module-definition
               :checks ((test-equal "Foobar" (identifier-to-string (ast-definition-name ast))))))

(deftest test-parse-definition-rejects-non-definition ()
  (test-parser parse-definition "Foobar" :is-match-p nil))

(deftest test-parse-definition ()
  (test-parse-definition-simple-type)
  (test-parse-definition-simple-function)
  (test-parse-definition-rejects-non-definition))

;; -----------------------------------------------------------------------------
(defun parse-definition-list (scanner state)
  (push-scope (scope-stack state))
  (let ((ast (parse-list parse-definition scanner state :start-delimiter #\{ :end-delimiter #\}))
        (scope (pop-scope (scope-stack state))))
    (setf (slot-value ast 'local-scope) scope)
    ast))

;; -----------------------------------------------------------------------------
(defun parse-compilation-unit (scanner state)
  (let* ((start-position (scanner-position scanner))
         (definitions (parse-list parse-definition scanner state))
         (result (parse-result-match
                   (parse-action 'ast-compilation-unit
                                 (make-source-region start-position scanner)
                                 state
                                 :definitions (if (parse-result-no-match-p definitions)
                                                (parse-action 'ast-list
                                                              (make-source-region start-position start-position)
                                                              state)
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
  (test-current-scope)
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
  ((block-stack
     :reader emitter-blocks
     :initform (progn
                 (let ((array (make-array 10 :adjustable t :fill-pointer 0 :element-type 'emitter-block)))
                   (vector-push-extend (make-instance 'emitter-block) array)
                   array)))
   (package-name
     :reader emitter-package-name
     :initarg :package-name
     :initform :flux-program)
   (scope-stack
     :reader scope-stack
     :initform (make-scope-stack))
   (current-namespace
     :accessor current-namespace
     :initform nil)
   functions
   types))

;; -----------------------------------------------------------------------------
;;////TODO: this has to go
(defparameter *object-class-name* (intern "Object"))

;; -----------------------------------------------------------------------------
(defun current-emitter-block (state)
  (let ((block-stack (slot-value state 'block-stack)))
    (elt block-stack (1- (length block-stack)))))

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
    (vector-push-extend block (slot-value state 'block-stack))
    (if name
        (set-current-block-name state name))
    block))

;; -----------------------------------------------------------------------------
(defun pop-emitter-block (state)
  (vector-pop (slot-value state 'block-stack)))

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
(defmacro test-emitter (code parser declarations lambda-list &body checks)
  (with-gensyms (ast parser-value state result)
    `(let* ((,parser-value ,parser)
            (,ast (funcall ,parser-value
                           (make-string-scanner ,code)
                           (make-instance 'parser-state)))
            (,state (let* ((state (make-instance 'emitter-state)))
                      (push-scope (scope-stack state))
                      ,@(loop for decl in declarations
                              collect `(find-or-create-declaration (current-scope (scope-stack state))
                                                                   ,(car decl)
                                                                   (make-identifier ',@(mapcar
                                                                                        (lambda (id)
                                                                                          (intern id :flux-program))
                                                                                        (cdr decl)))))
                      state))
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
  (declare (ignore state))
  (intern "Nothing"));////TODO: user proper lookup

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-named-type) state)
  (let ((declaration (lookup-declaration (scope-stack state)
                                         *declaration-kind-type*
                                         (ast-type-name ast)
                                         :current-namespace (current-namespace state))))
    (if (not declaration)
      (not-implemented "declaration not found"))
    (mangled-name declaration)))

(deftest test-emit-named-type-simple ()
  (test-emitter
      "Foobar"
      #'parse-type
      ((*declaration-kind-type* "Foobar"))
      name
    (test-equal "Foobar" (symbol-name name))))

(defsuite test-emit-named-type ()
  (test-emit-named-type-simple))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-integer-literal) state)
  (declare (ignore state))
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
    ()
    (operator name value)
    (declare (ignore operator name value))));////TODO

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
      ()
      (operator name (superclass) slots)
    (declare (ignore slots))
    (test-equal 'defclass operator)
    (test-equal "Foobar" (symbol-name name))
    (test-equal *object-class-name* superclass)))

(deftest test-emit-type-definition-single-supertype ()
  (test-emitter
      "type Foobar : Barfoo;"
      #'parse-definition
      ((*declaration-kind-type* "Barfoo"))
      (operator name (superclass) slots)
    (declare (ignore operator name slots))
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
      ()
      (operator name () body)
    (declare (ignore body))
    (test-equal 'defmethod operator)
    (test-equal "Foobar" (symbol-name name))))

(defsuite test-emit-function-definition ()
  (test-emit-function-definition-simple))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-field-definition) state)
  (declare (ignore state))
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
      ()
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
(defun get-regression-spec-argument (regression-spec key)
  (let ((key-value-pairs (regression-spec-data regression-spec)))
    (if key-value-pairs
      (cdr (assoc key (ast-list-nodes key-value-pairs)))
      nil)))

;; -----------------------------------------------------------------------------
(defgeneric verify-no-regression (regression-spec-type regression-spec source
                                                       emitter-state generated-code))

;; -----------------------------------------------------------------------------
(defmethod verify-no-regression ((regression-spec-type (eql :type)) regression-spec source
                                                                    emitter-state generated-code)
  (declare (ignore regression-spec source emitter-state generated-code))
  ())

;; -----------------------------------------------------------------------------
(defmethod verify-no-regression ((regression-spec-type (eql :function)) regression-spec source
                                                                        emitter-state generated-code)
  (declare (ignore regression-spec source emitter-state generated-code))
  ())

;; -----------------------------------------------------------------------------
(defmethod verify-no-regression ((regression-spec-type (eql :call)) regression-spec source
                                                                    emitter-state generated-code)
  (declare (ignore source emitter-state generated-code))
  (let ((function (get-regression-spec-argument regression-spec :function))
        (result (get-regression-spec-argument regression-spec :result)))
    (test-equal result (funcall (intern function)))))

;; -----------------------------------------------------------------------------
(defun parse-spec-ignored-text (scanner state)
  (let ((saved-position (scanner-position scanner)))
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
  (declare (ignore state))
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
  (declare (ignore state))
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
                             ((scanner-match-sequence scanner "CALL:") :call)
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
(deftest run-regression-tests ()
  (flet ((test-file (pathname)
           (if (find (pathname-type pathname) *flux-file-extensions* :test #'equal)
               (with-test-name (pathname)
                 (print (format nil "--- ~a ---" pathname))
                 (let* ((test-package-name :flux-regression-tests)
                        (source (make-source pathname))
                        (regression-specs (parse-result-value
                                            (parse-spec-list
                                              (make-string-scanner (source-text source))
                                              (make-instance 'parser-state))))
                        (emitter-state (make-instance 'emitter-state
                                                      :package-name test-package-name))
                        code)
                   (parse source :package-for-symbols test-package-name)
                   (setf code (emit (source-ast source) emitter-state))
                   (pprint code)
                   (mapc (lambda (x) (print x) (eval x)) code)
                   (mapc (lambda (spec)
                           (verify-no-regression (regression-spec-type spec) spec source emitter-state code))
                         regression-specs)
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
