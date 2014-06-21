
(in-package :fl)

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

