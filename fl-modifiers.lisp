
(in-package :fl)

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

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
(defsuite test-modifiers ()
  (test-run-type-modifier-p))

