
(in-package :fl)

;;;;============================================================================
;;;;    Classes.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defclass namespace ()
  ())

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun canonicalize (name)
  (do ((result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
       (scanner (make-string-scanner name))
       (previous-char nil))
      ((is-at-end-p scanner)
       result)
    (let ((ch (read-next-token scanner)))
      (cond
        ((and (upper-case-p ch)
              previous-char
              (lower-case-p previous-char))
         (vector-push-extend #\_ result))
        ((and previous-char
              (upper-case-p ch)
              (upper-case-p previous-char)
              (not (is-at-end-p scanner))
              (lower-case-p (peek-next-token scanner)))
         (vector-push-extend #\_ result))
        ((eq #\- ch) (setf ch #\_)))
      (vector-push-extend (char-upcase ch) result)
      (setf previous-char ch))))

(deftest test-canonicalize ()
  (test-equal "TEST" (canonicalize "test"))
  (test-equal "TEST" (canonicalize "TEST"))
  (test-equal "TE_ST" (canonicalize "teST"))
  (test-equal "TEST_THIS" (canonicalize "testThis"))
  (test-equal "TEST_THIS" (canonicalize "test_this"))
  (test-equal "TEST_THIS" (canonicalize "test_THIS"))
  (test-equal "TEST_THIS" (canonicalize "test-this"))
  (test-equal "TEST_THIS" (canonicalize "TestThis"))
  (test-equal "TEST::THIS" (canonicalize "test::this"))
  (test-equal "TEST_THIS::TOO" (canonicalize "testThis::Too"))
  (test-equal "UI_PROGRAM" (canonicalize "UIProgram"))
  (test-equal "TEST__THIS" (canonicalize "test__this"))
  (test-equal "_TEST" (canonicalize "_test")))

;; -----------------------------------------------------------------------------
(defsuite test-names ()
  (test-canonicalize))


