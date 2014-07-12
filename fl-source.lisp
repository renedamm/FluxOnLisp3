
(in-package :fl)

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
(defmethod get-local-scope (source)
  (let ((ast (source-ast source)))
    (if (not ast)
      nil
      (get-local-scope ast))))

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

