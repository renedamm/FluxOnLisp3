; This code is pretty much the opposite of idiomatic Lisp...

(in-package :cl-user)

(defpackage :flux
  (:use :common-lisp)
  (:export
   :run-tests))

(in-package :flux)

;;;;============================================================================
;;;;	Test Framework.
;;;;============================================================================

;////TODO: turn this into a stack and print suites/tests as we enter them (instead of for each single test)
(defvar *test-name* nil)

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
(defmacro deftest-internal (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;; -----------------------------------------------------------------------------
(defmacro deftest (name parameters &body body)
  `(deftest-internal ,name ,parameters
     ,@body))

;; -----------------------------------------------------------------------------
(defmacro defsuite (name parameters &body body)
  `(deftest-internal ,name ,parameters
     (combine-results
       ,@body)))

;;;;============================================================================
;;;;	Utilities.
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
(defsuite test-utilities ()
  (test-insert-into-array))

;;;;============================================================================
;;;;	Sources.
;;;;============================================================================

(defclass source ()
  ())

;; -----------------------------------------------------------------------------
(defun make-source-region (left right)
  (assert (<= left right))
  (cons left right))

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
;;;;	ASTs.
;;;;============================================================================

(defclass ast-node ()
  ((region
    :reader source-region
    :initarg :source-region)))

(defclass ast-error (ast-node)
  ())

(defclass ast-list (ast-node)
  ((nodes
    :reader list-nodes
    :initarg :nodes)))

(defclass ast-global-qualifier (ast-node)
  ())

(defclass ast-identifier (ast-node)
  ((qualifier
    :reader id-qualifier
    :initarg qualifier
    :initform nil)
   (name
    :reader id-name
    :initarg :name)))

(defclass ast-modifier (ast-node)
  ())

(defclass ast-immutable-modifier (ast-modifier)
  ())

(defclass ast-mutable-modifier (ast-modifier)
  ())

(defclass ast-abstract-modifier (ast-modifier)
  ())

(defclass ast-include-modifier (ast-modifier)
  ())

(defclass ast-import-modifier (ast-modifier)
  ())

(defclass ast-instantiable-modifier (ast-modifier)
  ())

(defclass ast-clause (ast-node)
  ())

(defclass ast-statement (ast-node)
  ())

(defclass ast-expression (ast-node)
  ())

(defclass ast-definition (ast-node)
  ((attributes
    :reader definition-attributes
    :initarg :attributes)
   (modifiers
    :reader definition-modifiers
    :initarg :modifiers)
   (name
    :reader definition-name
    :initarg :name)
   (type-parameters
    :reader definition-type-parameters
    :initarg :type-parameters)
   (value-parameters
    :reader definition-value-parameters
    :initarg :value-parameters)
   (clauses
    :reader definition-clauses
    :initarg :clauses)
   (type
    :reader definition-type
    :initarg :type)
   (value
    :reader definition-value
    :initarg :value)
   (body
    :reader definition-body
    :initarg :body)))

(defclass ast-type-definition (ast-definition)
  ())

(defclass ast-object-definition (ast-type-definition)
  ())

(defclass ast-function-definition (ast-definition)
  ())

(defclass ast-field-definition (ast-function-definition)
  ())

(defclass ast-method-definition (ast-function-definition)
  ())

(defclass ast-compilation-unit (ast-node)
  ((definitions
    :reader unit-definitions
    :initarg :definitions)))

;; -----------------------------------------------------------------------------
(defmethod initialize-instance :after ((id ast-identifier) &key)
  ;; Convert 'name' slot to symbol, if it isn't one already.
  (let ((name (id-name id)))
    (if (not (symbolp name))
	(setf (slot-value id 'name) (intern name)))))

(deftest test-ast-identifier-initialize ()
  (let ((id (make-instance 'ast-identifier :name "test")))
    (test (symbolp (id-name id)))))

;; -----------------------------------------------------------------------------
(defun identifier-to-string (id)
  (let ((qualifier (id-qualifier id)))
    (cond ((not qualifier)
	   (symbol-name (id-name id)))
	  ((typep qualifier 'ast-global-qualifier)
	   (concatenate 'string "::" (symbol-name (id-name id))))
	  (t
	   (concatenate 'string (identifier-to-string qualifier) "::" (symbol-name (id-name id)))))))

(deftest test-identifier-to-string ()
  (let ((id1 (make-instance 'ast-identifier :name "test")))
    (test-equal "test" (identifier-to-string id1))))

;; -----------------------------------------------------------------------------
(defsuite test-asts ()
  (test-ast-identifier-initialize)
  (test-identifier-to-string))

;;;;============================================================================
;;;;	Naming Conventions.
;;;;============================================================================

(defclass naming-convention ()
  (prefix
   suffix
   first-character-case
   middle-character-case
   last-character-case))

(defgeneric normalize-name (name convention))

;;;;============================================================================
;;;;	Scanners.
;;;;============================================================================

(defclass scanner ()
  ((position
    :initform 0
    :accessor scanner-position)))

(defclass string-scanner (scanner)
  ((string
    :initarg :string
    :initform "")))

(defclass stream-scanner (scanner)
  ())

(defgeneric scanner-at-end? (scanner))
(defgeneric scanner-peek-next (scanner))
(defgeneric scanner-read-next (scanner))
(defgeneric scanner-match (scanner token))
(defgeneric scanner-match-if (scanner function))

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
  (every (lambda (token) (scanner-match scanner token)) token-list))

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
  (test-string-scanner-at-end-p)
  (test-string-scanner-peek-next)
  (test-string-scanner-read-next)
  (test-string-scanner-match)
  (test-string-scanner-match-if)
  (test-string-scanner-setf-position))

;;;;============================================================================
;;;;	Parsers.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmacro test-parser (function string &key (is-match-p :dont-test) end-position state expected-type line-breaks-at checks)
  (with-gensyms (res scanner is-match-value end-position-value expected-type-value state-value line-breaks-at-value)
    `(let* ((,scanner (make-string-scanner ,string))
	    (,state-value ,state))

       (if (not ,state-value)
	   (setf ,state-value (make-instance 'parser-state)))

       (let*
	   ((,res (funcall ,function ,scanner ,state-value))
	    (,is-match-value ,is-match-p)
	    (,end-position-value ,end-position)
	    (,expected-type-value ,expected-type)
	    (,line-breaks-at-value ,line-breaks-at))

	 ;////TODO: automatically check region of parsed AST

	 ;; Check match, if requested.
	 (if (not (eq ,is-match-value :dont-test))
	     (if ,is-match-value
		 (test (parse-result-match-p ,res))
		 (test (parse-result-no-match-p ,res))))

	 ;; Check end position, if requested.
	 (if ,end-position-value
	     (test-equal ,end-position-value (scanner-position ,scanner)))

	 ;; Check value type, if requested.
	 (if ,expected-type-value
	     (test-type ,expected-type-value (parse-result-value ,res)))
	
	 ;; Check line breaks, if requested.
	 (if ,line-breaks-at-value
	     (test-sequence-equal ,line-breaks-at-value (line-break-table ,state-value)))

	 ;; Check AST.
	 (let ((ast (parse-result-value ,res)))
	   (setf ast ast) ; Silence warning if check-ast is nil.
	   ,@checks)))))

;; -----------------------------------------------------------------------------
(defparameter *parse-result-no-match* (cons nil nil))

;; -----------------------------------------------------------------------------
(defclass parser-state ()
  ((line-break-table
    :initform (make-line-break-table)
    :reader line-break-table)))

;; -----------------------------------------------------------------------------
(defmacro parse-result-match-p (result)
  `(not (eq ,result *parse-result-no-match*)))

;; -----------------------------------------------------------------------------
(defmacro parse-result-no-match-p (result)
  `(eq ,result *parse-result-no-match*))

;; -----------------------------------------------------------------------------
(defmacro parse-result-value (result)
  result)

;; -----------------------------------------------------------------------------
(defmacro parse-result-match (value)
  value)

;; -----------------------------------------------------------------------------
(defmacro parse-result-no-match ()
  '*parse-result-no-match*)

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
  (test-parser #'parse-whitespace "foo" :end-position 0 :is-match-p nil))

(deftest test-parse-whitespace-consumes-whitespace ()
  (test-parser #'parse-whitespace (format nil " ~C~C~Cfoo" #\Return #\Newline #\Tab) :is-match-p t :end-position 4))

(deftest test-parse-whitespace-consumes-single-line-comments ()
  (test-parser #'parse-whitespace (format nil " // foo~Cbar" #\Newline) :is-match-p t :end-position 8 :line-breaks-at '(0 8)))

(deftest test-parse-whitespace-consume-multi-line-comments ()
  (test-parser #'parse-whitespace (format nil " /* foo /* ~C bar */ */foo" #\Newline) :is-match-p t :end-position 22 :line-breaks-at '(0 12)))

(defsuite test-parse-whitespace ()
  (test-parse-whitespace-does-not-consume-non-whitespace)
  (test-parse-whitespace-consumes-whitespace)
  (test-parse-whitespace-consumes-single-line-comments)
  (test-parse-whitespace-consume-multi-line-comments))

;; -----------------------------------------------------------------------------
(defun parse-list (parser scanner state &key terminator separator opener)
  (parse-whitespace scanner state)
  (let ((start-position (scanner-position scanner)))
    (if opener
	(progn
	  (parse-whitespace scanner state)
	  (if (not (scanner-match scanner opener))
	      (return-from parse-list (parse-result-no-match)))))
    (let (list)
      (loop
	 (parse-whitespace scanner state)
	 (let ((element (funcall parser scanner state)))
	   (if (parse-result-no-match-p element)
	       (return))
	   (setf list (cons element list))
	   (cond (separator
		  (if (not (scanner-match scanner separator))
		      (if (and terminator (scanner-match scanner terminator))
			  (return))
		      (not-implemented "parse error; expecting separator (or terminator)")))
		 (terminator
		  (if (scanner-match scanner terminator)
		      (return))))))
      (setf list (nreverse list))
      (if (equal (scanner-position scanner) start-position)
	  (parse-result-no-match)
	  (parse-result-match (make-instance 'ast-list :source-region (make-source-region start-position (scanner-position scanner)) :nodes list))))))

;; -----------------------------------------------------------------------------
(defun parse-string-literal ()
  ())

;; -----------------------------------------------------------------------------
(defun parse-numeric-literal ()
  ())

;; -----------------------------------------------------------------------------
(defun parse-literal ()
  ())

;; -----------------------------------------------------------------------------
(defun parse-modifier (scanner state)
  (let ((saved-position (scanner-position scanner)))
    (flet ((parse-region () (make-source-region saved-position (scanner-position scanner))))
      (cond ((scanner-match-sequence scanner "abstract")
	     (parse-result-match
	      (make-instance 'ast-abstract-modifier :source-region (parse-region))))
	    ((scanner-match-sequence scanner "immutable")
	     (parse-result-match
	      (make-instance 'ast-immutable-modifier :source-region (parse-region))))
	    (t (parse-result-no-match))))))

(defsuite test-parse-modifier ()
  (test-parser 'parse-modifier "abstract[" :end-position 8 :is-match-p t :expected-type 'ast-abstract-modifier)
  (test-parser 'parse-modifier "immutable " :end-position 9 :is-match-p t :expected-type 'ast-immutable-modifier))

;; -----------------------------------------------------------------------------
(defun parse-modifier-list (scanner state)
  (parse-list #'parse-modifier scanner state))

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
  (test-parser #'parse-identifier "test"
	       :end-position 4 :is-match-p t :expected-type 'ast-identifier
	       :checks ((test-equal (intern "test") (id-name ast))
			(test-equal nil (id-qualifier ast)))))

(defsuite test-parse-identifier ()
  (test-parse-identifier-simple-name))

;; -----------------------------------------------------------------------------
(defun parse-clause (scanner state)
  (parse-result-no-match))

;; -----------------------------------------------------------------------------
(defun parse-clause-list (scanner state)
  (parse-list #'parse-clause scanner state))

;; -----------------------------------------------------------------------------
(defun parse-attribute (scanner state)
  (parse-result-no-match))

;; -----------------------------------------------------------------------------
(defun parse-attribute-list (scanner state)
  (parse-list #'parse-attribute scanner state :opener #\[ :separator #\, :terminator #\]))

;; -----------------------------------------------------------------------------
(defun parse-type (scanner state)
  (parse-result-no-match))

;; -----------------------------------------------------------------------------
(defun parse-type-parameter (scanner state)
  (parse-result-no-match))

;; -----------------------------------------------------------------------------
(defun parse-type-parameter-list (scanner state)
  "Parse a list of type parameters surrounded by '<' and '>'."
  (parse-list #'parse-type-parameter scanner state :opener #\< :separator #\, :terminator #\>))

;; -----------------------------------------------------------------------------
(defun parse-value-parameter (scanner state)
  (parse-result-no-match))

;; -----------------------------------------------------------------------------
(defun parse-value-parameter-list (scanner state)
  "Parse a list of value parameters surrounded by '(' and ')'."
  (parse-list #'parse-value-parameter scanner state :opener #\( :separator #\, :terminator #\)))

;; -----------------------------------------------------------------------------
(defun parse-statement (scanner state)
  (parse-result-no-match))

;; -----------------------------------------------------------------------------
(defun parse-statement-list (scanner state)
  "Parse a list of statements surrounded by '{' and '}'."
  (parse-list #'parse-statement scanner state :opener #\{ :terminator #\}))

;; -----------------------------------------------------------------------------
(defun parse-expression (scanner state)
  (parse-result-no-match))

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

    ;; Parse definition kind.
    (parse-whitespace scanner state)
    (setf definition-class (cond ((scanner-match-sequence scanner "method")
				 'ast-method-definition)
				((scanner-match-sequence scanner "field")
				 'ast-field-definition)
				((scanner-match-sequence scanner "type")
				 'ast-type-definition)
				((scanner-match-sequence scanner "object")
				 'ast-object-definition)
				((scanner-match-sequence scanner "function")
				 'ast-function-definition)
				((scanner-match-sequence scanner "features")
				 'ast-features-definition)
				((scanner-match-sequence scanner "module")
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
	  ((scanner-match scanner #\=)
	   (parse-whitespace scanner state)
	   (setf value (parse-expression scanner state))
	   (parse-whitespace scanner state)
	   (if (not (scanner-match scanner #\;))
	       (not-implemented "parse error; expecting semicolon")))
	  (t
	   (setf body (parse-statement-list scanner state))))

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
  (test-parser #'parse-definition "type Foobar;"
	       :is-match-p t :expected-type 'ast-type-definition
	       :checks ((test-equal "Foobar" (identifier-to-string (definition-name ast))))))

(deftest test-parse-definition-rejects-non-definition ()
  (test-parser #'parse-definition "Foobar" :is-match-p nil))

(defsuite test-parse-definition ()
  (test-parse-definition-simple-type)
  (test-parse-definition-rejects-non-definition))

;; -----------------------------------------------------------------------------
(defun parse-compilation-unit (scanner state)
  (let ((start-position (scanner-position scanner))
	(definitions (parse-list #'parse-definition scanner state)))
    (parse-result-match (make-instance 'ast-compilation-unit
				       :source-region (make-source-region start-position (scanner-position scanner))
				       :definitions definitions))))

(deftest test-parse-compilation-unit-empty ()
  (test-parser #'parse-compilation-unit "" :is-match-p t :expected-type 'ast-compilation-unit))

(deftest test-parse-compilation-unit-simple ()
  (test-parser #'parse-compilation-unit "type Foobar; type Barfoo;" :is-match-p t :expected-type 'ast-compilation-unit))

(defsuite test-parse-compilation-unit ()
  (test-parse-compilation-unit-empty)
  (test-parse-compilation-unit-simple))

;; -----------------------------------------------------------------------------
(defsuite test-parsers ()
  (test-parse-whitespace)
  (test-parse-modifier)
  (test-parse-identifier)
  (test-parse-definition)
  (test-parse-compilation-unit-simple))

;;;;============================================================================
;;;;	Emitter.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass emitter-state ()
  ((root
    :reader emitter-result
    :initform ())))

;; -----------------------------------------------------------------------------
(defmacro test-emitter (code parser lambda-list &body checks)
  (with-gensyms (ast parser-value state result)
    `(let* ((,parser-value ,parser)
	    (,ast (funcall ,parser-value
			   (make-string-scanner ,code)
			   (make-instance 'parser-state)))
	    (,state (make-instance 'emitter-state))
	    (,result (emit ,ast ,state)))
       (destructuring-bind ,lambda-list
	   ,result
	 ,@checks))))

;; -----------------------------------------------------------------------------
(defgeneric emit (ast state))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-type-definition) state)
  `(defclass foobar ()))

(deftest test-emit-type-definition-simple ()
  (test-emitter
      "type Foobar;"
      #'parse-definition
      (operator name &rest rest)
    (test (equal operator 'defclass))
    (test (equal "FOOBAR" (symbol-name name)))))

;; -----------------------------------------------------------------------------
(defmethod emit ((ast ast-compilation-unit) state)
  ())

;; -----------------------------------------------------------------------------
(defsuite test-emitters ()
  (test-emit-type-definition-simple))

;;;;============================================================================
;;;;	Entry Points.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun parse (code)
  "Parse one or more units of Flux code.  Returns a list of AST-COMPILATION-UNITs.
A unit of code can be represented as a string (parsed directly as Flux code), a pathname \
(if pointing to a file, contents of file are parsed; if pointing to a directory, all Flux \
source files in the directory and any of its subdirectories are parsed), a scanner (fed \
directly into the parser), or a character stream (parsed as Flux code)."
  (cond ((typep code 'scanner)
	 (let ((state (make-instance 'parser-state)))
	   (list (parse-compilation-unit code state))))
	((stringp code)
	 (parse (make-string-scanner code)))
	((listp code)
	 (mapcan #'parse code))
	(t
	 (not-implemented "parsing files, streams, etc."))))

(deftest test-parse-code-string ()
  (destructuring-bind (ast)
      (parse "type Foobar;")
    (test (typep ast 'ast-compilation-unit))))

(defsuite test-parse ()
  (test-parse-code-string))

;; -----------------------------------------------------------------------------
(defun flux-to-lisp (code)
  "Parses one or more units of Flux code and then translates them to Lisp.  Returns \
the resulting Lisp expression."
  (let ((emitter-state (make-instance 'emitter-state))
	(asts (parse code)))
    (mapc #'emit asts)
    (emitter-result emitter-state)))

;; -----------------------------------------------------------------------------
;////TODO: print test summary at end
(defun run-tests ()
  (test-asts)
  (test-scanners)
  (test-parsers)
  (test-emitters)
  (test-utilities)
  (test-text-utilities)
  (test-parse))
