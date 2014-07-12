
(in-package :fl)

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
(defgeneric verify-no-regression (regression-spec-type regression-spec source generated-code))

;; -----------------------------------------------------------------------------
(defmethod verify-no-regression ((regression-spec-type (eql :type)) regression-spec source generated-code)
  (declare (ignore regression-spec source generated-code))
  ())

;; -----------------------------------------------------------------------------
(defmethod verify-no-regression ((regression-spec-type (eql :function)) regression-spec source generated-code)
  (declare (ignore regression-spec source generated-code))
  ())

;; -----------------------------------------------------------------------------
(defmethod verify-no-regression ((regression-spec-type (eql :call)) regression-spec source generated-code)
  (declare (ignore source generated-code))
  (with-test-name CALL
    (let ((function (get-regression-spec-argument regression-spec :function))
          (result (read (make-string-input-stream (get-regression-spec-argument regression-spec :result)))))
      (test-equal result (funcall (intern function))))))

;; -----------------------------------------------------------------------------
(defun parse-spec-ignored-text (scanner)
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
               (add-line-break *parser-line-break-table* (scanner-position scanner))))))
    (if (equal (scanner-position scanner)
               saved-position)
        (parse-result-no-match)
        (parse-result-match))))

(deftest test-parse-spec-ignored-text ()
  (test-parser parse-spec-ignored-text
               (format nil "fooi /* fo~Co */ ;[] /*#FOO" #\Newline)
               :is-match-p t :end-position 23 :line-breaks-at '(0 11)))

;; -----------------------------------------------------------------------------
(defun parse-spec-key (scanner)
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
(defun parse-spec-value (scanner)
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
(defun parse-spec-key-value-pair (scanner)
  (let (key value)
    (setf key (parse-spec-key scanner))
    (if (not (parse-result-match-p key))
        (return-from parse-spec-key-value-pair (parse-result-no-match)))
    (if (scanner-match scanner #\=)
        (progn
          (setf value (parse-spec-value scanner))
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
(defun parse-spec-key-value-pair-list (scanner)
  (parse-list parse-spec-key-value-pair scanner))

(deftest test-parse-spec-key-value-pair-list ()
  (test-parser parse-spec-key-value-pair-list "foo=1 bar=2" :is-match-p t :end-position 11))

;; -----------------------------------------------------------------------------
(defun parse-spec-list (scanner)
  (let (list spec spec-type values)
    (loop
       (parse-spec-ignored-text scanner)
       (if (scanner-at-end-p scanner)
           (return))

       (setf spec-type (cond ((scanner-match-sequence scanner "TYPE:") :type)
                             ((scanner-match-sequence scanner "FUNCTION:") :function)
                             ((scanner-match-sequence scanner "CALL:") :call)
                             (t (not-implemented "Unrecognized regression spec type"))))

       (setf values (parse-spec-key-value-pair-list scanner))
       (if (not (parse-result-match values))
           (not-implemented "parse error; expecting list of key/value pairs"))

       (setf spec (make-instance 'regression-spec
                                 :type spec-type
                                 :data (parse-result-value values)))

       (setf list (cons spec list))
       (setf spec nil)

       (parse-whitespace scanner)
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
           (if (find (pathname-type pathname) *config-source-file-extensions* :test #'equal)
             (progn
               (print (format nil "--- ~a ---" pathname))
               (let* ((test-package-name :flux-regression-tests)
                      (source (make-source pathname))
                      (regression-specs (parse-result-value
                                          (with-new-parser-state
                                            (parse-spec-list
                                              (make-string-scanner (source-text source))))))
                      code)
                 (parse source :package-for-symbols test-package-name)
                 (with-toplevel-scope (get-local-scope (source-ast source))
                   (with-new-emitter-state (:package-name test-package-name)
                     (setf code (emit (source-ast source)))
                     (print "=Code")
                     (pprint code)
                     (print "=Evaluation")
                     (mapc (lambda (x) (print x) (eval x)) code)
                     (print "=Specs")
                     (fresh-line)
                     (mapc (lambda (spec)
                             (verify-no-regression (regression-spec-type spec) spec source code))
                           regression-specs))
                   (in-package :fl)
                   (delete-package test-package-name)))))))
    (walk-directory *config-regression-suite-directory* #'test-file :directories nil)))

