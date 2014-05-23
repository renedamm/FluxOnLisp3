
(in-package :cl-user)

(defpackage :fl
  (:use :common-lisp)
  (:export
   :run-tests))

(in-package :fl)

;;;;------------------------- Unit Test Framework ---------------------------------

(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest-internal (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro deftest (name parameters &body body)
  `(deftest-internal ,name ,parameters
     (check
       ,@body)))

(defmacro defsuite (name parameters &body body)
  `(deftest-internal ,name ,parameters
     (combine-results
       ,@body)))

;;;;------------------------------ Sources --------------------------------------

(defclass source ()
  ())

(defun make-source-region (left right)
  (assert (<= left right))
  (cons left right))

;;;;------------------------------- ASTs ----------------------------------------

(defclass ast-node ()
  ((region
    :reader :source-region
    :initarg :source-region)))

(defclass ast-error (ast-node)
  ())

(defclass ast-definition (ast-node)
  ())

(defclass ast-clause (ast-node)
  ())

(defclass ast-modifier (ast-node)
  ())

(defclass ast-immutable-modifier (ast-modifier)
  ())

(defclass ast-mutable-modifier (ast-modifier)
  ())

(defclass ast-abstract-modifier (ast-modifier)
  ())

(defclass ast-statement (ast-node)
  ())

(defclass ast-expression (ast-node)
  ())

(defclass ast-compilation-unit (ast-node)
  ())

;;;;------------------------------- Scanners ----------------------------------------

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

(defun make-string-scanner (string)
  (make-instance 'string-scanner :string string))

(defmethod scanner-at-end-p ((scanner string-scanner))
  (equal (slot-value scanner 'position)
	 (length (slot-value scanner 'string))))

(defmethod scanner-peek-next ((scanner string-scanner))
  (elt (slot-value scanner 'string)
       (slot-value scanner 'position)))

(defmethod scanner-read-next ((scanner string-scanner))
  (let ((token (scanner-peek-next scanner)))
    (incf (slot-value scanner 'position))
    token))

(defmethod scanner-match ((scanner string-scanner) char)
  (cond
    ((scanner-at-end-p scanner) nil)
    ((equal (scanner-peek-next scanner) char)
     (incf (slot-value scanner 'position))
     t)
    (t nil)))

(defun scanner-match-sequence (scanner token-list)
  (every (lambda (token) (scanner-match scanner token)) token-list))

(defmethod (setf scanner-position) :before (position (scanner string-scanner))
  (if (or (<= position 0)
	  (>= (length (slot-value scanner 'string))))
      (error (format nil "Position ~a out of range for string ~a used by string-scanner!" position (slot-value scanner 'string)))))

(deftest test-string-scanner ()
  (check

    (not (scanner-at-end-p (make-string-scanner "foo")))
    (scanner-at-end-p (make-string-scanner ""))

    (let ((scanner (make-string-scanner "foo")))
      (and (equal (scanner-peek-next scanner) #\f)
	   (equal (scanner-position scanner) 0)))

    (let ((scanner (make-string-scanner "foo")))
      (and (equal (scanner-read-next scanner) #\f)
	   (equal (scanner-position scanner) 1)))
    
    (scanner-match (make-string-scanner "foo") #\f)
    (not (scanner-match (make-string-scanner "foo") #\b))
    (not (scanner-match (make-string-scanner "") #\f))))

(defsuite test-scanners ()
  (test-string-scanner))

;;;;------------------------------- Parsers ----------------------------------------

(defmacro test-parser (function string &key (is-match-p :dont-test) end-position state expected-type)
  (with-gensyms (res scanner is-match-value end-position-value expected-type-value)
    `(let* ((,scanner (make-string-scanner ,string))
	    (,res (funcall ,function ,scanner ,state))
	    (,is-match-value ,is-match-p)
	    (,end-position-value ,end-position)
	    (,expected-type-value ,expected-type))
       (and

	; Check match, if requested.
	(if (not (eq ,is-match-value :dont-test))
	    (if ,is-match-value
		(parse-result-match-p ,res)
		(parse-result-no-match-p ,res))
	    t)
	
	; Check end position, if requested.
	(if ,end-position-value
	    (equal (scanner-position ,scanner) ,end-position-value)
	    t)

	; Check value type, if requested.
	(if ,expected-type-value
	    (typep (parse-result-value ,res) ,expected-type-value)
	    t)))))

(defparameter *parse-result-no-match* (cons nil nil))

(defclass parser-state ()
  ())

(defmacro parse-result-match-p (result)
  `(not (eq ,result *parse-result-no-match*)))

(defmacro parse-result-no-match-p (result)
  `(eq ,result *parse-result-no-match*))

(defmacro parse-result-value (result)
  result)

(defmacro parse-result-match (value)
  value)

(defmacro parse-result-no-match ()
  '*parse-result-no-match*)

(defun parse-whitespace (scanner state)
  (let ((saved-position (scanner-position scanner)))
    (loop
       (if (scanner-at-end-p scanner)
	   (return))
       (let ((char (scanner-peek-next scanner)))
	 (if (or (equal char #\Newline)
		 (equal char #\Tab)
		 (equal char #\Space)
		 (equal char #\Return))
	     (scanner-read-next scanner)
	     (return))))
    (if (not (equal saved-position (scanner-position scanner)))
	(parse-result-match nil)
	(parse-result-no-match))))

(deftest test-parse-whitespace ()
  (check
    (test-parser #'parse-whitespace "foo" :end-position 0 :is-match-p nil)
    (test-parser #'parse-whitespace (format nil " ~C~C~Cfoo" #\Return #\Newline #\Tab) :is-match-p t :end-position 4)))

(defun parse-definition ()
  ())

(defun parse-statement ()
  ())

(defun parse-expresssion ()
  ())

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

(deftest test-parse-modifier ()
  (check
    (test-parser 'parse-modifier "abstract[" :end-position 8 :is-match-p t :expected-type 'ast-abstract-modifier)
    (test-parser 'parse-modifier "immutable " :end-position 9 :is-match-p t :expected-type 'ast-immutable-modifier)))

(defsuite test-parsers ()
  (test-parse-whitespace)
  (test-parse-modifier))

(defun run-tests ()
  (test-scanners)
  (test-parsers))
