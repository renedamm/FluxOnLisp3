
(in-package :fl)

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(deftest test-can-execute-program-with-empty-main-function ()
  (test-equal *object-nothing* (run-flux "program Test { function Main : () -> () {} }")))

;; -----------------------------------------------------------------------------
(defsuite test-acceptance ()
  (test-can-execute-program-with-empty-main-function))

