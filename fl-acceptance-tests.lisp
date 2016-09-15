
(in-package :fl)

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(deftest test-can-execute-program-with-empty-main-function ()
  (test-equal *object-nothing*
    (run-flux "
      program Test
      {
          function Main : () -> ()
          {
          }
      }
    ")))

;; -----------------------------------------------------------------------------
(deftest test-ignores-comments ()
  (test-equal *object-nothing*
    (run-flux "
      program Test
      {
          /* multi
           line
           comment
           function Main : () -> () { fjkljfkljdslk f $#$#$ */
          function Main : () -> () // foo
          {
          }
      }
    ")))

;; -----------------------------------------------------------------------------
(deftest test-can-execute-program-returning-singleton ()
  (let ((result
          (run-flux "
            program Test
            {
                object Test;
                function Main : () -> Test
                {
                    return Test;
                }
            }
           ")))
    (test (fl-singleton-p result))
    (test-equal (canonicalize "Test") (get-name result))))

;; -----------------------------------------------------------------------------
(deftest test-introduces-all-definitions-in-a-scope-simultaneously ()
  (let ((result
          (run-flux "
            program Test
            {
                function Main : () -> Test
                {
                    return Test;
                }
                object Test;
            }
           ")))
    (test (fl-singleton-p result))
    (test-equal (canonicalize "Test") (get-name result))))

;; -----------------------------------------------------------------------------
(deftest test-puts-definitions-in-right-namespace ()
  (let ((result
          (run-flux "
            program Test
            {
                namespace Outer
                {
                    namespace Inner::InnerMost
                    {
                        object Test;
                    }
                }
                function Main : () -> Test
                {
                    return Outer::Inner::InnerMost::Test;
                }
            }
           ")))
    (test (fl-singleton-p result))
    (test-equal (canonicalize "Outer::Inner::InnerMost::Test") (get-qualified-name result))))

;; -----------------------------------------------------------------------------
;;/////TODO: need test to ensure modifiers on the feature groups are applied
(deftest test-ignores-feature-groups ()
  (let ((result
          (run-flux "
            program Test
            {
                features My::Stuff
                {
                    features More
                    {
                        object Test;
                    }
                }
                function Main : () -> Test
                {
                    return Test;
                }
            }
           ")))
    (test (fl-singleton-p result))
    (test-equal (canonicalize "Test") (get-name result))))

;; -----------------------------------------------------------------------------
(deftest test-can-execute-hello-world-program ()
  (let ((result
          (run-flux "
            program Test
            {
                type String;
                [ InternalCall ]
                function Print : String -> ();

                function Main : () -> ()
                {
                    Print( \"Hello, World!\" );
                }
            }
           ")))
    (test (fl-singleton-p result))
    (test-equal (canonicalize "Outer::Inner::InterMost::Test") (get-name result))))

;; -----------------------------------------------------------------------------
(defsuite test-acceptance ()
  (test-can-execute-program-with-empty-main-function)
  (test-ignores-comments)
  (test-can-execute-program-returning-singleton)
  (test-introduces-all-definitions-in-a-scope-simultaneously)
  (test-puts-definitions-in-right-namespace)
  (test-ignores-feature-groups))
  ;(test-can-execute-hello-world-program))

