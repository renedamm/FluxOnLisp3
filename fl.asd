
(defsystem "flux2lisp"
  :description "Flux2Lisp: A Flux to Lisp compiler."
  :version "0.0.1"
  :author "Rene Damm <rene.damm@gmx.net>"
  :license "Public Domain"
  :components ((:file "fl-common")
               (:file "fl-config" :depends-on ("fl-common"))
               (:file "fl-tests" :depends-on ("fl-common"))
               (:file "fl-regression-tests" :depends-on ("fl-common" "fl-tests" "fl-config" "fl-parser" "fl-emit"))
               (:file "fl-ast" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-symbols" :depends-on ("fl-common" "fl-config" "fl-tests" "fl-ast" "fl-utils"))
               (:file "fl-source" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-diagnostics" :depends-on ("fl-common"))
               (:file "fl-scanner" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-parser" :depends-on ("fl-common" "fl-tests" "fl-scanner" "fl-symbols" "fl-ast"))
               (:file "fl-emit" :depends-on ("fl-common" "fl-tests" "fl-symbols" "fl-parser"))
               (:file "fl-driver" :depends-on ("fl-common" "fl-tests" "fl-emit"))
               (:file "fl-utils" :depends-on ("fl-common" "fl-tests"))))

