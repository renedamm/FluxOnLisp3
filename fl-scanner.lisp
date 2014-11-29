
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass scanner ()
  ((position
     :initform 0
     :reader get-position
     :writer set-position)))

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

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defgeneric is-at-end-p (scanner))

;; -----------------------------------------------------------------------------
(defgeneric peek-next-token (scanner))

;; -----------------------------------------------------------------------------
(defgeneric read-next-token (scanner))

;; -----------------------------------------------------------------------------
(defgeneric match-next-token (scanner token))

;; -----------------------------------------------------------------------------
(defgeneric match-next-token-if (scanner function))

;; -----------------------------------------------------------------------------
(defmethod match-next-token ((scanner scanner) token)
  (cond ((is-at-end-p scanner) nil)
        ((equal (peek-next-token scanner) token)
         (read-next-token scanner)
         t)
        (t nil)))

(deftest test-match-next-token ()
  (let ((scanner (make-stream-scanner "foo")))
    (test (match-next-token scanner #\f))
    (test (not (match-next-token scanner #\b)))))

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
(defmethod is-at-end-p ((scanner stream-scanner))
  (>= (slot-value scanner 'position)
      (slot-value scanner 'stream-length)))

(deftest test-stream-scanner-is-at-end-p ()
  (let ((scanner (make-stream-scanner "")))
    (test (is-at-end-p scanner))))

;; -----------------------------------------------------------------------------
(defmethod peek-next-token ((scanner stream-scanner))
  (peek-char nil (slot-value scanner 'stream)))

(deftest test-stream-scanner-peek-next-token ()
  (let ((scanner (make-stream-scanner "foo")))
    (test-equal (peek-next-token scanner) #\f)))

;; -----------------------------------------------------------------------------
(defmethod read-next-token ((scanner stream-scanner))
  (let ((char (read-char (slot-value scanner 'stream))))
    (incf (slot-value scanner 'position))
    char))

(deftest test-stream-scanner-read-next-token ()
  (let ((scanner (make-stream-scanner "foo")))
    (test-equal (read-next-token scanner) #\f)))

;; -----------------------------------------------------------------------------
(defun make-string-scanner (string)
  (make-instance 'string-scanner :string string))

;; -----------------------------------------------------------------------------
(defmethod is-at-end-p ((scanner string-scanner))
  (equal (slot-value scanner 'position)
         (length (slot-value scanner 'string))))

(deftest test-string-scanner-is-at-end-p ()
  (test (not (is-at-end-p (make-string-scanner "foo"))))
  (test (is-at-end-p (make-string-scanner ""))))

;; -----------------------------------------------------------------------------
(defmethod peek-next-token ((scanner string-scanner))
  (elt (slot-value scanner 'string)
       (slot-value scanner 'position)))

(deftest test-string-scanner-peek-next-token ()
  (let ((scanner (make-string-scanner "foo")))
    (test (equal (peek-next-token scanner) #\f))
    (test (equal (get-position scanner) 0))))

;; -----------------------------------------------------------------------------
(defmethod read-next-token ((scanner string-scanner))
  (let ((token (peek-next-token scanner)))
    (incf (slot-value scanner 'position))
    token))

(deftest test-string-scanner-read-next-token ()
  (let ((scanner (make-string-scanner "foo")))
    (test (equal (read-next-token scanner) #\f))
    (test (equal (get-position scanner) 1))))

;; -----------------------------------------------------------------------------
(defmethod match-next-token ((scanner string-scanner) char)
  (cond
    ((is-at-end-p scanner) nil)
    ((equal (peek-next-token scanner) char)
     (incf (slot-value scanner 'position))
     t)
    (t nil)))

(deftest test-string-scanner-match-next-token ()
  (test (match-next-token (make-string-scanner "foo") #\f))
  (test (not (match-next-token (make-string-scanner "foo") #\b)))
  (test (not (match-next-token (make-string-scanner "") #\f))))

;; -----------------------------------------------------------------------------
(defmethod match-next-token-if ((scanner string-scanner) function)
  (cond
    ((is-at-end-p scanner) nil)
    ((funcall function (peek-next-token scanner))
     (incf (slot-value scanner 'position)))
    (t nil)))

(deftest test-string-scanner-match-next-token-if ()
  (test (match-next-token-if (make-string-scanner "foo" ) (lambda (char) (declare (ignore char)) t)))
  (test (not (match-next-token-if (make-string-scanner "foo") (lambda (char) (declare (ignore char)) nil)))))

;; -----------------------------------------------------------------------------
(defun match-token-sequence (scanner token-list)
  (let ((saved-position (get-position scanner)))
    (if (every (lambda (token) (match-next-token scanner token)) token-list)
        t
        (progn
          (set-position saved-position scanner)
          nil))))

(deftest test-match-token-sequence ()
  (let ((scanner (make-string-scanner "foo")))
    (test (not (match-token-sequence scanner "bar")))
    (test-equal 0 (get-position scanner)))
  (let ((scanner (make-string-scanner "foobar")))
    (test (match-token-sequence scanner "foo"))
    (test-equal 3 (get-position scanner))))

;; -----------------------------------------------------------------------------
(defun match-keyword (scanner token-list)
  (let ((saved-position (get-position scanner)))
    (if (and (match-token-sequence scanner token-list)
             (or (is-at-end-p scanner)
                 (let ((next-char (peek-next-token scanner)))
                   (and (not (alphanumericp next-char))
                        (not (equal next-char #\_))))))
        t
        (progn
          (set-position saved-position scanner)
          nil))))

(deftest test-match-keyword ()
  (let ((scanner (make-string-scanner "foobar")))
    (test (not (match-keyword scanner "foo")))
    (test-equal 0 (get-position scanner)))
  (let ((scanner (make-string-scanner "foobar ")))
    (test (match-keyword scanner "foobar"))
    (test-equal 6 (get-position scanner)))
  (let ((scanner (make-string-scanner "foo")))
    (test (match-keyword scanner "foo"))
    (test-equal 3 (get-position scanner)))
  (let ((scanner (make-string-scanner "foo_")))
    (test (not (match-keyword scanner "foo")))))

;; -----------------------------------------------------------------------------
(defmethod set-position :before (position (scanner string-scanner))
  (if (or (< position 0)
          (> position (length (slot-value scanner 'string))))
      (error (format nil "Position ~a out of range for string ~a used by string-scanner!"
                     position
                     (slot-value scanner 'string)))))

(deftest test-string-scanner-set-position ()
  (let ((scanner (make-string-scanner "foo")))
    (set-position 0 scanner)
    (test-equal 0 (get-position scanner))
    (set-position 3 scanner)
    (test-equal 3 (get-position scanner))))

;; -----------------------------------------------------------------------------
(defsuite test-scanners ()
  (test-match-next-token)
  (test-match-token-sequence)
  (test-match-keyword)
  (test-stream-scanner-is-at-end-p)
  (test-stream-scanner-peek-next-token)
  (test-string-scanner-is-at-end-p)
  (test-string-scanner-peek-next-token)
  (test-string-scanner-read-next-token)
  (test-string-scanner-match-next-token)
  (test-string-scanner-match-next-token-if)
  (test-string-scanner-set-position))

