
(in-package :fl)

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;;////TODO: function that checks for duplicate and conflicting modifiers

;; -----------------------------------------------------------------------------
(defun run-type-modifier-p (modifier)
  (case modifier
    ('before t)
    ('after t)
    ('around t)
    (otherwise nil)))

(deftest test-run-type-modifier-p ()
  (test-equal t (run-type-modifier-p 'before))
  (test-equal t (run-type-modifier-p 'after))
  (test-equal t (run-type-modifier-p 'around))
  (test-equal nil (run-type-modifier-p 'public)))

;; -----------------------------------------------------------------------------
(defun has-modifier-p (modifier modifiers)
  (not (not (member modifier modifiers))))

(deftest test-has-modifier-p ()
  (test-equal t (has-modifier-p 'before (list 'after 'before 'private)))
  (test-equal nil (has-modifier-p 'around (list 'before 'after 'private))))

;; -----------------------------------------------------------------------------
(defsuite test-modifiers ()
  (test-run-type-modifier-p)
  (test-has-modifier-p))

