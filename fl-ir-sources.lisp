
(in-package :fl)

;;////TODO: print-object support for ir-source

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass ir-source ()
  ((name
     :reader get-name
     :initarg :name)
   (path
     :reader get-path
     :initarg :path)
   (line-breaks
     :reader get-line-breaks
     :initarg :line-breaks
     :initform (make-line-break-table))
   (text
     :reader get-text
     :initarg :text)
   (diagnostics
     :reader get-diagnostics
     :initform (make-instance 'diagnostic-collection))
   (ast
     :reader get-ast
     :writer set-ast)))

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun make-source (code &key name path)
  (if (pathnamep code)
      (progn
        (if (not path)
            (setf path code))
        (setf code (read-string code))))
  (if (and (not name) path)
      (setf name (pathname-name path)))
  (make-instance 'ir-source
                 :name name
                 :path path
                 :text code))

(deftest test-make-source ()
  (let ((source (make-source "foo" :path #P"Test.flux")))
    (test-equal "Test" (get-name source))))

;; -----------------------------------------------------------------------------
(defun is-source-file-path-p (path)
  (member (string-downcase (pathname-type path))
          *config-source-file-extensions*
          :test #'equal))

(deftest test-is-source-file-path-p ()
  (test (is-source-file-path-p #P"test.flux"))
  (test (is-source-file-path-p #P"Test.Flux"))
  (test (not (is-source-file-path-p #P"Test")))
  (test (not (is-source-file-path-p #P"Test/"))))

;; -----------------------------------------------------------------------------
(defun collect-sources (&rest file-paths)
  ;;////REVIEW: in a "real" compiler, the loading should be asynchronous
  (mapcan (lambda (path)
    
            (cond

              ;; Use sources as is.
              ((typep path 'ir-source)
               (list path))

              ;; Recurse into lists.
              ((listp path)
               (mapcan #'collect-sources path))

              (t
               ;; Convert everything else to pathnames.
               (if (not (pathnamep path))
                 (setf path (pathname path)))

               (cond

                 ;; Scan directories.
                 ((directory-pathname-p path)
                  (remove-if #'not
                             (collect-sources (list-directory path))))

                 ;; Collect single source file.
                 ((is-source-file-path-p path)
                  (list (make-source path)))

                 ;; Ignore non-sources.
                 (t
                  nil)))))
          file-paths))

;; -----------------------------------------------------------------------------
(defun make-source-region (left right)
  (if (typep right 'scanner)
    (setf right (get-position right)))
  (assert (<= left right))
  (cons left right))

;; -----------------------------------------------------------------------------
(defun combine-source-region (first second)
  (make-source-region (min (car first) (car second))
                      (max (cdr first) (cdr second))))

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
(defun is-line-break-p (table position)
  (assert (>= position 0))
  (if (zerop position)
      t
      (let ((index (find-line-break-index table position)))
        (cond ((< index (1- (length table)))
               (equal (elt table (1+ index)) position))
              (t
               (equal (elt table index) position))))))

(deftest test-is-line-break-p ()
  (let ((table (make-line-break-table)))
    (add-line-break table 10)
    (test (is-line-break-p table 0))
    (test (is-line-break-p table 10))
    (test (not (is-line-break-p table 5)))
    (test (not (is-line-break-p table 15)))))

;; -----------------------------------------------------------------------------
(defsuite test-source ()
  (test-make-source)
  (test-is-source-file-path-p)
  (test-make-line-break-table-adds-first-line)
  (test-add-line-break-appends-line-at-end)
  (test-add-line-break-does-not-add-duplicates)
  (test-add-line-break-independent-of-insertion-order)
  (test-find-line-break-index)
  (test-is-line-break-p))

