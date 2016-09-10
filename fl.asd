
;; Build system.
(defsystem "FluxOnLisp"
  :description "FluxOnLisp: A Flux interpreter runngin on Lisp."
  :version "0.0.1"
  :author "Rene Damm <rene.damm@gmx.net>"
  :license "Public Domain"
  :components ((:file "fl-common")
               (:file "fl-config" :depends-on ("fl-common"))
               (:file "fl-tests" :depends-on ("fl-common"))
               (:file "fl-utils" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-source" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-scanner" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-diagnostics" :depends-on ("fl-common"))
               (:file "fl-ast" :depends-on ("fl-common" "fl-tests" "fl-source" "fl-diagnostics"))
               (:file "fl-parse" :depends-on ("fl-common" "fl-tests" "fl-scanner" "fl-ast" "fl-config"))
               (:file "fl-emit" :depends-on ("fl-common" "fl-tests"))))

