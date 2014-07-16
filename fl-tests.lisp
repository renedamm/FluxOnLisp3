
(in-package :fl)

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
     ;;////FIXME: this sucks for reloading
     (if (not (member #',name *test-suites*))
       (setf *test-suites* (cons #',name *test-suites*)))))

;; -----------------------------------------------------------------------------
;////TODO: print test summary at end
(defun run-unit-tests (&key terminate-on-failure)
  (if terminate-on-failure
      (every #'funcall *test-suites*)
      (reduce (lambda (result suite) (and (funcall suite) result)) *test-suites* :initial-value t)))

