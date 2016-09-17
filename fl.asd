
;; Build system.
(defsystem "FluxOnLisp"
  :description "FluxOnLisp: A Flux interpreter runngin on Lisp."
  :version "0.0.1"
  :author "Rene Damm <rene.damm@gmx.net>"
  :license "Public Domain"
  :components ((:file "fl-common")
               (:file "fl-config" :depends-on ("fl-common"))
               (:file "fl-tests" :depends-on ("fl-common")) ;;////TODO: rename this to test-framework or something
               (:file "fl-utils" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-source" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-scanner" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-diagnostics" :depends-on ("fl-common"))
               (:file "fl-ast" :depends-on ("fl-common" "fl-tests" "fl-source" "fl-diagnostics"))
               (:file "fl-parse" :depends-on ("fl-common" "fl-tests" "fl-scanner" "fl-ast" "fl-config"))
               (:file "fl-objects" :depends-on ("fl-common" "fl-tests" "fl-types"))
               (:file "fl-programs" :depends-on ("fl-common" "fl-tests" "fl-objects" "fl-functions" "fl-names"))
               (:file "fl-functions" :depends-on ("fl-common" "fl-tests" "fl-names" "fl-modifiers"))
               (:file "fl-names" :depends-on ("fl-common" "fl-tests" "fl-scanner" "fl-modifiers"))
               (:file "fl-types" :depends-on ("fl-common" "fl-tests" "fl-names"))
               (:file "fl-rewrite" :depends-on ("fl-common" "fl-tests" "fl-names" "fl-ast" "fl-programs" "fl-types" "fl-functions" "fl-objects"))
               (:file "fl-modifiers" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-expressions" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-statements" :depends-on ("fl-common" "fl-tests" "fl-expressions"))
               (:file "fl-operations" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-attributes" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-stdlib" :depends-on ("fl-common" "fl-tests" "fl-config" "fl-utils" "fl-source" "fl-scanner" "fl-parse"))
               (:file "fl-interpreter" :depends-on ("fl-common" "fl-tests" "fl-programs" "fl-names"))
               (:file "fl-driver" :depends-on ("fl-common" "fl-tests" "fl-programs" "fl-parse" "fl-scanner" "fl-rewrite"))
               (:file "fl-acceptance-tests" :depends-on ("fl-common" "fl-tests" "fl-driver" "fl-objects" "fl-names" "fl-stdlib"))))

