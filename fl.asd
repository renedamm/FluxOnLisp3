
;; Load libraries.
(ql:quickload "log4cl")

;; Build system.
(defsystem "flux2lisp"
  :description "Flux2Lisp: A Flux to Lisp compiler."
  :version "0.0.1"
  :author "Rene Damm <rene.damm@gmx.net>"
  :license "Public Domain"
  :components ((:file "fl-common")
               (:file "fl-config" :depends-on ("fl-common"))
               (:file "fl-tests" :depends-on ("fl-common"))
               (:file "fl-utils" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-scanner" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-diagnostics" :depends-on ("fl-common"))
               (:file "fl-ast" :depends-on ("fl-common" "fl-tests" "fl-ir-sources" "fl-diagnostics"))
			   (:file "fl-ir-data" :depends-on ("fl-common" "fl-tests"))
			   (:file "fl-ir-types" :depends-on ("fl-common" "fl-tests"))
			   (:file "fl-ir-functions" :depends-on ("fl-common" "fl-tests"))
			   (:file "fl-ir-instructions" :depends-on ("fl-common" "fl-tests"))
			   (:file "fl-ir-modules" :depends-on ("fl-common" "fl-tests"))
			   (:file "fl-ir-programs" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-ir-sources" :depends-on ("fl-common" "fl-tests" "fl-scanner" "fl-utils" "fl-config"))
               (:file "fl-ir-root" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-parse" :depends-on ("fl-common" "fl-tests" "fl-scanner" "fl-ast" "fl-config"))
               (:file "fl-translate" :depends-on ("fl-common"
												  "fl-tests" "fl-ast"
												  "fl-ir-sources"
												  "fl-ir-modules"
												  "fl-ir-types"
												  "fl-ir-functions"
												  "fl-ir-root"
												  "fl-parse")) ;; Parsing functionality needed for testing.
               (:file "fl-optimize" :depends-on ("fl-common" "fl-tests"))
               (:file "fl-emit" :depends-on ("fl-common" "fl-tests"))
			   ;;////TODO: rename to fl-driver
               (:file "fl-compiler" :depends-on ("fl-common" "fl-tests" "fl-parse" "fl-ir-root"))))

