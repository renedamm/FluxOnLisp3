
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
    :reader scanner-position)))

(defclass string-scanner (scanner)
  ((string
    :initarg :string
    :initform "")))

(defclass stream-scanner (scanner)
  ())

(defgeneric scanner-at-end? (scanner))
(defgeneric scanner-peek-next (scanner))
(defgeneric scanner-read-next (scanner))

(defun make-string-scanner (string)
  (make-instance 'string-scanner :string string))

(defmethod scanner-at-end? ((scanner string-scanner))
  (equal (slot-value scanner 'position)
	 (length (slot-value scanner 'string))))

(defmethod scanner-peek-next ((scanner string-scanner))
  (elt (slot-value scanner 'string)
       (slot-value scanner 'position)))

(defmethod scanner-read-next ((scanner string-scanner))
  (let ((token (scanner-peek-next scanner)))
    (incf (slot-value scanner 'position))
    token))

(deftest test-string-scanner ()
  (check
    (not (scanner-at-end? (make-string-scanner "foo")))
    (scanner-at-end? (make-string-scanner ""))
    (let ((scanner (make-string-scanner "foo")))
      (and (equal (scanner-peek-next scanner) #\f)
	   (equal (scanner-position scanner) 0)))
    (let ((scanner (make-string-scanner "foo")))
      (and (equal (scanner-read-next scanner) #\f)
	   (equal (scanner-position scanner) 1)))))

(defsuite test-scanners ()
  (test-string-scanner))

;;;;------------------------------- Parsers ----------------------------------------

;; Every parser takes a

(defclass parser-state ()
  ())

(defun parse-whitespace (scanner state)
  ())

(deftest test-parse-whitespace ()
  t)

(defun parse-definition ()
  ())

(defun parse-statement ()
  ())

(defun parse-expresssion ()
  ())

(defun parse-modifier ()
  ())

(defsuite test-parsers ()
  (test-parse-whitespace))

(defun run-tests ()
  (test-scanners))
