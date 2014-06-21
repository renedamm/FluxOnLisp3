
(in-package :fl)

;;;;============================================================================
;;;;    Parser Actions
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun insert-declaration-into-symbol-table (declaration-kind ast parser-state &key (scope-index 0))
  (let ((declaration (find-or-create-declaration
                      (current-scope (scope-stack parser-state) :index scope-index)
                      declaration-kind
                      (ast-definition-name ast))))
    (add-definition declaration ast)))
  
;; -----------------------------------------------------------------------------
;; The default parse action is to just create an AST node whose type corresponds
;; to the name of the production and which is initialized from the given arguments.
(defmethod parse-action ((production symbol) region state &rest rest)
  (declare (ignore state))
  (apply 'make-instance (nconc (list production
                                     :source-region region)
                               rest)))

;; -----------------------------------------------------------------------------
(defmacro parse-action-for-definition (ast-class declaration-kind)
  `(defmethod parse-action ((production (eql ',ast-class)) region state &rest rest)
     (declare (ignore rest region))
     (let* ((ast (call-next-method)))
       (insert-declaration-into-symbol-table ,declaration-kind ast state)
       ast)))
  
(parse-action-for-definition ast-type-definition *declaration-kind-type*)
(parse-action-for-definition ast-function-definition *declaration-kind-function*)
(parse-action-for-definition ast-method-definition *declaration-kind-function*)
(parse-action-for-definition ast-module-definition *declaration-kind-module*)
(parse-action-for-definition ast-variable-definition *declaration-kind-variable*)

;;;;============================================================================
;;;;    Parsers.
;;;;============================================================================

;; The parsers here are a mixture of plain recursive descent and
;; combinators.  Each parser implements a specific pattern that it
;; will recognize and from which it will derive an optional value.  As
;; a side-effect of recognizing a pattern, it will advance the
;; position in the input sequence to the first position after the
;; match.
;;
;; All parsers have to either produce a full match or produce no match
;; at all.
;;
;; If a parser runs into a situation where it can partially recognize
;; a prefix of the input but then comes across input that doesn't
;; match what is expected, it can
;;
;;  a) either abort, reset the input position, and return a no-match, or
;;  b) apply error repair and return a match.
;;
;; Error repair is usually implemented by falling back to parsers that
;; recognize a superset of valid input but produce diagnostics and
;; error nodes as side- effects.  In that sense, there are two
;; languages implemented by the parsers: one that is Flux and one that
;; is a superset of Flux trying to recognize as much of Flux as
;; possible in otherwise invalid input.
;;
;; Diagnostics produced by error handling will be stored on the parser
;; state.  Error nodes are normal AST nodes that refer to the
;; diagnostics and that are made part of the AST like any other type
;; of node.
;;
;; Parsers follow a longest-prefix matching strategy, i.e. as long as
;; a prefix matches their expected pattern, they will produce a match.
;; They will, however, go for the longest possible prefix they can
;; match.

;; -----------------------------------------------------------------------------
(defmacro test-parser (function string &key (is-match-p :dont-test) end-position state expected-type line-breaks-at checks)
  (with-gensyms (res scanner end-position-value expected-type-value state-value line-breaks-at-value)
    `(let* ((,scanner (make-string-scanner ,string))
            (,state-value ,state))

       (if (not ,state-value)
           (setf ,state-value (make-instance 'parser-state)))

       (let*
           ((,res (,function ,scanner ,state-value))
            (,end-position-value ,end-position)
            (,expected-type-value ,expected-type)
            (,line-breaks-at-value ,line-breaks-at))

         ;////TODO: automatically check region of parsed AST

         (combine-results

           ;; Check match, if requested.
           ,(cond
             ((eq is-match-p :dont-test)
              t)
             (is-match-p
              `(test (parse-result-match-p ,res)))
             (t
              `(test (parse-result-no-match-p ,res))))

           ;; Check end position, if requested.
           (if ,end-position-value
               (test-equal ,end-position-value (scanner-position ,scanner))
               (if ,(and is-match-p (not (eq is-match-p :dont-test)))
                   (test-equal (length ,string) (scanner-position ,scanner))
                   t))

           ;; Check value type, if requested.
           (if ,expected-type-value
               (test-type ,expected-type-value (if (parse-result-match-p ,res) (parse-result-value ,res) ,res))
               t)
        
           ;; Check line breaks, if requested.
           (if ,line-breaks-at-value
               (test-sequence-equal ,line-breaks-at-value (line-break-table ,state-value))
               t)

           ;; Check AST.
           ,(if (> (length checks) 0)
                `(let ((ast (parse-result-value ,res)))
                   (combine-results ,@checks))))))))

;; -----------------------------------------------------------------------------
(defparameter *parse-result-no-match* :no-match)

;; -----------------------------------------------------------------------------
(defparameter *parse-result-no-value* :no-value)

;; -----------------------------------------------------------------------------
(defclass parser-state ()
  ((line-break-table
     :reader line-break-table
     :initform (make-line-break-table)
     :initarg :line-break-table)
   (diagnostics
     :reader parser-diagnostics
     :initarg :diagnostics)
   (package-for-symbols
     :reader package-for-symbols
     :initarg :package-for-symbols
     :initform *flux-default-package*)
   (scope-stack
     :reader scope-stack
     :initform (make-scope-stack))))

(defmethod initialize-instance :after ((instance parser-state) &key)

  ;; Make sure that package-for-symbols is an existing package.
  (let* ((symbol-package-name (package-for-symbols instance))
         (symbol-package (find-package symbol-package-name)))
    (if (not symbol-package)
      (progn
        (setf symbol-package (make-package symbol-package-name :use :common-lisp))
        (setf (slot-value instance 'package-for-symbols) symbol-package))))
  
  ;; Set current scope to global scope.
  (push-scope (scope-stack instance)))

;; -----------------------------------------------------------------------------
(defun parse-result-match-p (result)
  (not (eq result *parse-result-no-match*)))

;; -----------------------------------------------------------------------------
(defun parse-result-no-match-p (result)
  (eq result *parse-result-no-match*))

;; -----------------------------------------------------------------------------
(defun parse-result-value (result)
  (assert (parse-result-match-p result))
  (if (eq result *parse-result-no-value*)
      nil
      result))

;; -----------------------------------------------------------------------------
(defun parse-result-match (&optional value)
  (if value
      value
      *parse-result-no-value*))

;; -----------------------------------------------------------------------------
(defun parse-result-no-match ()
  *parse-result-no-match*)

;; -----------------------------------------------------------------------------
(defun parse-whitespace (scanner state)
  (let ((initial-position (scanner-position scanner)))
    (flet ((line-break () (add-line-break (line-break-table state) (scanner-position scanner)))
           (consume () (scanner-read-next scanner)))
      (loop
         (if (scanner-at-end-p scanner)
             (return))
         (let ((char (scanner-peek-next scanner)))
           (cond ((equal char #\Newline)
                  (consume)
                  (line-break))
                 ((equal char #\Return)
                  (consume)
                  (scanner-match scanner #\Newline)
                  (line-break))
                 ((or (equal char #\Tab)
                      (equal char #\Space))
                  (consume))
                 ((equal char #\/)
                  (scanner-read-next scanner)
                  (cond ((scanner-match scanner #\/)
                         (loop
                            (if (or (scanner-at-end-p scanner)
                                    (not (scanner-match-if scanner
                                                           (lambda (char) (not (or (equal char #\Newline)
                                                                                   (equal char #\Return)))))))
                                (return))))
                        ((scanner-match scanner #\*)
                         (let ((nesting-level 1))
                           (loop
                              (if (or (zerop nesting-level)
                                      (scanner-at-end-p scanner))
                                  (return))
                              (let ((next-char (scanner-read-next scanner)))
                                (cond ((and (equal next-char #\/)
                                            (scanner-match scanner #\*))
                                       (incf nesting-level))
                                      ((and (equal next-char #\*)
                                            (scanner-match scanner #\/))
                                       (decf nesting-level))
                                      ((equal next-char #\Return)
                                       (scanner-match scanner #\Newline)
                                       (line-break))
                                      ((equal next-char #\Newline)
                                       (line-break)))))))
                        (t
                         (decf (scanner-position scanner))
                         (return))))
                 (t
                  (return))))))
    (if (not (equal initial-position (scanner-position scanner)))
        (parse-result-match nil)
        (parse-result-no-match))))

(deftest test-parse-whitespace-does-not-consume-non-whitespace ()
  (test-parser parse-whitespace "foo" :end-position 0 :is-match-p nil))

(deftest test-parse-whitespace-consumes-whitespace ()
  (test-parser parse-whitespace
               (format nil " ~C~C~Cfoo" #\Return #\Newline #\Tab)
               :is-match-p t :end-position 4))

(deftest test-parse-whitespace-consumes-single-line-comments ()
  (test-parser parse-whitespace
               (format nil " // foo~Cbar" #\Newline)
               :is-match-p t :end-position 8 :line-breaks-at '(0 8)))

(deftest test-parse-whitespace-consume-multi-line-comments ()
  (test-parser parse-whitespace
               (format nil " /* foo /* ~C bar */ */foo" #\Newline)
               :is-match-p t :end-position 22 :line-breaks-at '(0 12)))

(deftest test-parse-whitespace ()
  (test-parse-whitespace-does-not-consume-non-whitespace)
  (test-parse-whitespace-consumes-whitespace)
  (test-parse-whitespace-consumes-single-line-comments)
  (test-parse-whitespace-consume-multi-line-comments))

;; -----------------------------------------------------------------------------
;; Combinator that turns another parser into a list parser.  This combinator can be broken
;; down into more atomic combinators that can be composed to create the same effect as this
;; combinator here yet in a more flexible manner.  However, it works nicely for the parsers
;; we have.
(defmacro parse-list (parser-name scanner state &key start-delimiter end-delimiter separator)
  ;; List must either be undelimited or have both a start and end
  ;; delimiter.
  (with-gensyms (scanner-value state-value list start-position element parse-list-block)
    `(block ,parse-list-block
       (let ((,scanner-value ,scanner)
             (,state-value ,state))

         (parse-whitespace ,scanner-value ,state-value)
         (let ((,start-position (scanner-position ,scanner-value))
               ,element
               ,list)

           ;; Match start-delimiter.
           ,(if start-delimiter
                `(progn
                   (parse-whitespace ,scanner-value ,state-value)
                   (if (not (scanner-match ,scanner-value ,start-delimiter))
                       (return-from ,parse-list-block (parse-result-no-match)))))

           ;; Match elements.
           (loop

              ;; Try to parse another element.
              (parse-whitespace ,scanner-value ,state-value)
              (setf ,element (,parser-name ,scanner-value ,state-value))
              (parse-whitespace ,scanner-value ,state-value)

              ;; Handle parse result.
              (cond

                ;; Handle match.
                ((parse-result-match-p ,element)
                 (setf ,list (cons (parse-result-value ,element) ,list))
               
                 ;; Consume separator or terminate on end-delimiter.
                 ,(cond

                   ;; In a list with only separators and no end-delimiter,
                   ;; we stop as soon as an element isn't followed by a separator.
                   ((and separator (not end-delimiter))
                    `(if (not (scanner-match ,scanner-value ,separator))
                         (return)))

                   ;; In a list with both separators and an end-delimiter, we
                   ;; continue if we see a separator and we stop if we see an
                   ;; end-delimiter.
                   ((and separator end-delimiter)
                    `(if (scanner-match ,scanner-value ,separator)
                         t
                         (if (scanner-match ,scanner-value ,end-delimiter)
                             (return)
                             (not-implemented "expecting separator or terminator"))))))

                ;; Handle no match.
                ((parse-result-no-match-p ,element)
               
                 ,(if separator
                      `(cond

                         ;; If we don't have an end-delimiter and we've already read
                         ;; one or more elements, we're we missing an element.
                         ((and ,(not end-delimiter)
                               ,list)
                          (let* ((diagnostic (make-diagnostic (get-diagnostic-type-for-expecting ',parser-name)))
                                 (ast (make-ast-error diagnostic)))
                            (not-implemented "error handling")
                            (setf ,list (cons ast ,list))
                            (return)))

                         ;; If next up is a separator, we're missing an element.
                         ((scanner-match ,scanner-value ,separator)
                          (not-implemented "error; expecting element"))
                        
                         ;; Otherwise, if we haven't yet parsed any elements and we don't
                         ;; have an end-delimiter, we consider it a no-match rather than a match
                         ;; against an empty list.
                         ((and (not ,list)
                               ,(not end-delimiter))
                          (return-from ,parse-list-block (parse-result-no-match)))))

                 ,(if end-delimiter
                      `(if (not (scanner-match ,scanner-value ,end-delimiter))
                           (not-implemented "error; expecting element or terminator")))

                 (return))))
                  
           (setf ,list (nreverse ,list))
           (parse-action 'ast-list
                         (make-source-region ,start-position ,scanner-value)
                         state
                         :nodes ,list))))))

(deftest test-parse-list ()
  (labels
      ;; Define a dummy parser that matches "a".
      ((parse-a (scanner state)
         (declare (ignore state))
         (if (scanner-match scanner #\a)
             (parse-result-match (make-instance 'ast-node
                                                :source-region (make-source-region
                                                                 (1- (scanner-position scanner))
                                                                 (scanner-position scanner))))
             (parse-result-no-match)))

       ;; Parser that matches "a, a, a...".
       (parse-comma-separated-as (scanner state)
         (parse-list parse-a scanner state :separator #\,)))
   
    (setf (get 'parse-a :diagnostic-type) (make-instance 'diagnostic-type :code 80000 :name "a"))

    (with-test-name test-no-match
      (test-parser parse-comma-separated-as "foo" :is-match-p nil :end-position 0))

    (with-test-name test-single-element
      (test-parser parse-comma-separated-as "a" :is-match-p t :expected-type 'ast-list :end-position 1))

    (with-test-name test-comma-separated
      (test-parser parse-comma-separated-as "a, a, a]" :is-match-p t :expected-type 'ast-list :end-position 7))))

;; -----------------------------------------------------------------------------
(defun parse-modifier (scanner state)
  (let ((saved-position (scanner-position scanner)))
    (macrolet ((match (modifier ast-type)
                 `(if (scanner-match-keyword scanner ,modifier)
                      (return-from parse-modifier (parse-result-match
                                                    (parse-action ,ast-type
                                                                  (make-source-region saved-position scanner)
                                                                  state))))))
      (match "abstract" 'ast-abstract-modifier)
      (match "immutable" 'ast-immutable-modifier)
      (match "mutable" 'ast-mutable-modifier)
      (match "instantiable" 'ast-instantiable-modifier)
      (match "extend" 'ast-extend-modifier)
      (match "import" 'ast-import-modifier)
      (match "include" 'ast-include-modifier)
      (match "final" 'ast-final-modifier)
      (match "sealed" 'ast-sealed-modifier)
      (parse-result-no-match))))

(deftest test-parse-modifier ()
  (test-parser parse-modifier "importA" :is-match-p nil)
  (test-parser parse-modifier "abstract[" :end-position 8 :is-match-p t :expected-type 'ast-abstract-modifier)
  (test-parser parse-modifier "immutable " :end-position 9 :is-match-p t :expected-type 'ast-immutable-modifier)
  (test-parser parse-modifier "mutable" :end-position 7 :is-match-p t :expected-type 'ast-mutable-modifier)
  (test-parser parse-modifier "import?" :end-position 6 :is-match-p t :expected-type 'ast-import-modifier)
  (test-parser parse-modifier "include%" :end-position 7 :is-match-p t :expected-type 'ast-include-modifier)
  (test-parser parse-modifier "extend+" :end-position 6 :is-match-p t :expected-type 'ast-extend-modifier)
  (test-parser parse-modifier "instantiable!" :end-position 12 :is-match-p t :expected-type 'ast-instantiable-modifier)
  (test-parser parse-modifier "final " :end-position 5 :is-match-p t :expected-type 'ast-final-modifier)
  (test-parser parse-modifier "sealed_" :is-match-p nil)
  (test-parser parse-modifier "sealed#" :end-position 6 :is-match-p t :expected-type 'ast-sealed-modifier))

;; -----------------------------------------------------------------------------
(defun parse-modifier-list (scanner state)
  (parse-list parse-modifier scanner state))

;; -----------------------------------------------------------------------------
(defun parse-identifier (scanner state)
  (let ((start-position (scanner-position scanner))
        (next-char (scanner-peek-next scanner))
        (buffer (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character)))
    (if (or (alpha-char-p next-char)
            (equal next-char #\_))
        (progn
          (scanner-read-next scanner)
          (vector-push-extend next-char buffer)
          (loop
             (if (scanner-at-end-p scanner)
                 (return))
             (setf next-char (scanner-peek-next scanner))
             (if (and (not (alphanumericp next-char))
                      (not (equal next-char #\_)))
                 (return))
             (vector-push-extend next-char buffer)
             (scanner-read-next scanner))
          (parse-result-match (parse-action 'ast-identifier
                                            (make-source-region start-position scanner)
                                            state
                                            :name (intern buffer (package-for-symbols state)))))
        (parse-result-no-match))))

(deftest test-parse-identifier-simple-name ()
  (test-parser parse-identifier "test"
               :end-position 4 :is-match-p t :expected-type 'ast-identifier
               :checks ((test-equal (intern "test" *flux-default-package*) (ast-id-name ast))
                        (test-equal nil (ast-id-qualifier ast)))))

(deftest test-parse-identifier ()
  (test-parse-identifier-simple-name))

;; -----------------------------------------------------------------------------
(defun parse-clause (scanner state)
  (let ((saved-position (scanner-position scanner))
        (clause-type (cond ((scanner-match-keyword scanner "when")
                            'ast-when-clause)
                           ((scanner-match-keyword scanner "requires")
                            'ast-requires-clause)
                           ((scanner-match-keyword scanner "ensures")
                            'ast-ensures-clause)
                           ((scanner-match-keyword scanner "invariant")
                            'ast-invariant-clause)
                           (t
                            (return-from parse-clause (parse-result-no-match))))))
    (parse-whitespace scanner state)
    (let ((expression (parse-expression scanner state)))
      (if (parse-result-no-match-p expression)
          (not-implemented "parse error; expecting expression"))
      (parse-result-match (parse-action clause-type
                                        (make-source-region saved-position scanner)
                                        state
                                        :expression (parse-result-value expression))))))

;; -----------------------------------------------------------------------------
(defun parse-clause-list (scanner state)
  (parse-list parse-clause scanner state))

;; -----------------------------------------------------------------------------
(defun parse-attribute (scanner state)
  (declare (ignore scanner state))
  (parse-result-no-match));////TODO

;; -----------------------------------------------------------------------------
(defun parse-attribute-list (scanner state)
  (parse-list parse-attribute scanner state :start-delimiter #\[ :separator #\, :end-delimiter #\]))

;; -----------------------------------------------------------------------------
(defun parse-type (scanner state)
  (let* ((saved-position (scanner-position scanner))
         (left-type (cond ((scanner-match scanner #\()
                           (parse-whitespace scanner state)
                           (if (scanner-match scanner #\))
                               (parse-result-match (parse-action 'ast-nothing-type
                                                                 (make-source-region saved-position scanner)
                                                                 state))
                               (not-implemented "parenthesized type expressions")))
                          (t
                           (let (modifiers name)
                             (setf modifiers (parse-modifier-list scanner state))
                             (parse-whitespace scanner state)
                             (setf name (parse-identifier scanner state))
                             (if (parse-result-no-match-p name)
                                 (not-implemented "parse error; expecting type name"))
                             (parse-result-match (parse-action 'ast-named-type
                                                               (make-source-region saved-position scanner)
                                                               state
                                                               :name (parse-result-value name)
                                                               :modifiers (parse-result-value modifiers))))))))
    (parse-whitespace scanner state)

    (let ((combination-type (cond ((scanner-match-sequence scanner "->")
                                   'ast-function-type)
                                  ((scanner-match scanner #\&)
                                   'ast-union-type)
                                  ((scanner-match scanner #\|)
                                   'ast-intersection-type)
                                  (t nil))))
      (if (not combination-type)
          (return-from parse-type left-type))

      (parse-whitespace scanner state)
      (let ((right-type (parse-type scanner state)))
        (if (parse-result-no-match-p right-type)
            (not-implemented "parse error; expecting right type"))
        (parse-result-match (parse-action combination-type
                                          (make-source-region saved-position scanner)
                                          state
                                          :left-type (parse-result-value left-type)
                                          :right-type (parse-result-value right-type)))))))

(deftest test-parse-simple-named-type ()
  (test-parser parse-type "Foobar" :is-match-p t :expected-type 'ast-named-type))

(deftest test-parse-nothing-type ()
  (test-parser parse-type "()" :is-match-p t :expected-type 'ast-nothing-type))

(deftest test-parse-simple-function-type ()
  (test-parser parse-type "() -> ()" :is-match-p t :expected-type 'ast-function-type))

(deftest test-parse-simple-union-type ()
  (test-parser parse-type "Foobar & Foobar" :is-match-p t :expected-type 'ast-union-type
               :checks ((test-type 'ast-named-type (ast-type-left ast))
                        (test-type 'ast-named-type (ast-type-right ast)))))

(deftest test-parse-type ()
  (test-parse-simple-named-type)
  (test-parse-nothing-type)
  (test-parse-simple-function-type)
  (test-parse-simple-union-type))

;; -----------------------------------------------------------------------------
(defun parse-type-parameter (scanner state)
  (declare (ignore scanner state))
  (parse-result-no-match));////TODO

;; -----------------------------------------------------------------------------
(defun parse-type-parameter-list (scanner state)
  "Parse a list of type parameters surrounded by '<' and '>'."
  (parse-list parse-type-parameter scanner state :start-delimiter #\< :separator #\, :end-delimiter #\>))

;; -----------------------------------------------------------------------------
(defun parse-value-parameter (scanner state)
  (declare (ignore scanner state))
  (parse-result-no-match));////TODO

;; -----------------------------------------------------------------------------
(defun parse-value-parameter-list (scanner state)
  "Parse a list of value parameters surrounded by '(' and ')'."
  (parse-list parse-value-parameter scanner state :start-delimiter #\( :separator #\, :end-delimiter #\)))

;; -----------------------------------------------------------------------------
(defun parse-statement (scanner state)
  (let ((saved-position (scanner-position scanner)))
    (cond ((scanner-match-keyword scanner "return")
           (parse-whitespace scanner state)
           (let (expression)
             (if (not (scanner-match scanner #\;))
                 (progn
                   (setf expression (parse-expression scanner state))
                   (if (parse-result-no-match-p expression)
                       (not-implemented "parse error; expecting expression after 'return'"))
                   (if (not (scanner-match scanner #\;))
                       (not-implemented "parse error; expecting ';'"))))
             (parse-result-match (parse-action 'ast-return-statement
                                               (make-source-region saved-position scanner)
                                               state
                                               :expression (parse-result-value expression)))))
          (t
           (parse-result-no-match)))))

(deftest test-parse-return-statement ()
  (test-parser parse-statement "return ;" :is-match-p t :expected-type 'ast-return-statement))

(deftest test-parse-statement ()
  (test-parse-return-statement))

;; -----------------------------------------------------------------------------
;; Parse a list of statements surrounded by '{' and '}'.
(defun parse-statement-list (scanner state)
  (let ((scope (push-scope (scope-stack state)))
        (result (parse-list parse-statement scanner state :start-delimiter #\{ :end-delimiter #\})))
    (pop-scope (scope-stack state))
    (if (parse-result-match-p result)
      (setf (local-scope result) scope))
    result))

;; -----------------------------------------------------------------------------
(defun parse-literal (scanner state)
  (if (scanner-at-end-p scanner)
      (parse-result-no-match)
      (let ((saved-position (scanner-position scanner))
            (next-char (scanner-read-next scanner)))
        (cond ((digit-char-p next-char)
               (if (and (char-equal #\0 next-char)
                        (scanner-match scanner #\x))
                   (not-implemented "hex literals"))
               (let ((value (digit-char-to-integer next-char)))
                 (loop
                    (if (scanner-at-end-p scanner)
                        (return))
                    (setf next-char (scanner-peek-next scanner))
                    (if (not (digit-char-p next-char))
                        (return))
                    (scanner-read-next scanner)
                    (setf value (+ (digit-char-to-integer next-char) (* value 10))))
                 (parse-result-match (parse-action 'ast-integer-literal
                                                   (make-source-region saved-position scanner)
                                                   state
                                                   :value value))))
              (t (not-implemented "literal"))))))

(deftest test-parse-integer-literal ()
  (test-parser parse-literal "12" :is-match-p t :expected-type 'ast-integer-literal
               :checks ((test-equal 12 (ast-literal-value ast)))))

(deftest test-parse-literal ()
  (test-parser parse-literal "" :is-match-p nil)
  (test-parse-integer-literal))

;; -----------------------------------------------------------------------------
(defun parse-expression (scanner state)
  (if (scanner-at-end-p scanner)
      (parse-result-no-match)
      (let ((next-char (scanner-peek-next scanner)))
        (cond ((or (digit-char-p next-char)
                   (char-equal #\" next-char))
               (parse-literal scanner state))
              (t
               (parse-result-no-match))))))

(deftest test-parse-literal-expression ()
  (test-parser parse-expression "512" :is-match-p t :expected-type 'ast-integer-literal
               :checks ((test-equal 512 (ast-literal-value ast)))))

(deftest test-parse-expression ()
  (test-parse-literal-expression))

;; -----------------------------------------------------------------------------
(defun parse-definition (scanner state)
  (let (start-position
        attributes
        modifiers
        definition-class
        name
        value-parameters
        type-parameters
        type
        clauses
        value
        body
        ast)

    (parse-whitespace scanner state)
    (setf start-position (scanner-position scanner))

    ;; Parse attributes.
    (setf attributes (parse-attribute-list scanner state))

    ;; Parse modifiers.
    (setf modifiers (parse-modifier-list scanner state))

    ;;////FIXME: this needs to match keyword+whitespace, not just keyword
    ;; Parse definition kind.
    (parse-whitespace scanner state)
    (setf definition-class (cond ((scanner-match-keyword scanner "method")
                                 'ast-method-definition)
                                ((scanner-match-keyword scanner "field")
                                 'ast-field-definition)
                                ((scanner-match-keyword scanner "type")
                                 'ast-type-definition)
                                ((scanner-match-keyword scanner "object")
                                 'ast-object-definition)
                                ((scanner-match-keyword scanner "local")
                                 'ast-variable-definition)
                                ((scanner-match-keyword scanner "function")
                                 'ast-function-definition)
                                ((scanner-match-keyword scanner "features")
                                 'ast-features-definition)
                                ((scanner-match-keyword scanner "module")
                                 'ast-module-definition)
                                (t
                                 (return-from parse-definition (parse-result-no-match)))))

    ;; Parse name.
    (parse-whitespace scanner state)
    (setf name (parse-identifier scanner state))

    ;; Parse type parameters.
    (setf type-parameters (parse-type-parameter-list scanner state))
    
    ;; Parse value parameters.
    (setf value-parameters (parse-value-parameter-list scanner state))

    ;; Parse type.
    (parse-whitespace scanner state)
    (if (scanner-match scanner #\:)
        (progn
          (parse-whitespace scanner state)
          (setf type (parse-type scanner state))))

    ;; Parse clauses.
    (setf clauses (parse-clause-list scanner state))

    ;; Parse body/value.
    (parse-whitespace scanner state)
    (cond ((scanner-match scanner #\;)
           t)
          ;;////TODO: if it's a type definition, we need to parse a type here when hitting '='
          ((scanner-match scanner #\=)
           (parse-whitespace scanner state)
           (setf value (parse-expression scanner state))
           (parse-whitespace scanner state)
           (if (not (scanner-match scanner #\;))
               (not-implemented "parse error; expecting semicolon")))
          (t
           (setf body
                 ;;////FIXME: this should be unified into one single parsing path for both module and other definitions
                 (if (eq 'ast-module-definition definition-class)
                     (parse-definition-list scanner state)
                     (parse-statement-list scanner state)))))

    ;; Create AST node.
    (setf ast (parse-action definition-class
                            (make-source-region start-position scanner)
                            state
                            :attributes attributes
                            :modifiers modifiers
                            :name name
                            :type-parameters type-parameters
                            :value-parameters value-parameters
                            :type type
                            :clauses clauses
                            :value value
                            :body body))

    (parse-result-match ast)))

(deftest test-parse-definition-simple-type ()
  (test-parser parse-definition "type Foobar;"
               :is-match-p t :expected-type 'ast-type-definition
               :checks ((test-equal "Foobar" (identifier-to-string (ast-definition-name ast))))))

(deftest test-parse-definition-simple-function ()
  (test-parser parse-definition "function Foobar : () -> () {}"
               :is-match-p t
               :expected-type 'ast-function-definition
               :checks ((test-equal "Foobar" (identifier-to-string (ast-definition-name ast))))))

(deftest test-parse-definition-simple-module ()
  (test-parser parse-definition "module Foobar {}"
               :is-match-p t
               :expected-type 'ast-module-definition
               :checks ((test-equal "Foobar" (identifier-to-string (ast-definition-name ast))))))

(deftest test-parse-definition-rejects-non-definition ()
  (test-parser parse-definition "Foobar" :is-match-p nil))

(deftest test-parse-definition ()
  (test-parse-definition-simple-type)
  (test-parse-definition-simple-function)
  (test-parse-definition-rejects-non-definition))

;; -----------------------------------------------------------------------------
(defun parse-definition-list (scanner state)
  (push-scope (scope-stack state))
  (let ((result (parse-list parse-definition scanner state :start-delimiter #\{ :end-delimiter #\}))
        (scope (pop-scope (scope-stack state))))
    (if (parse-result-match-p result)
      (setf (slot-value (parse-result-value result) 'local-scope) scope))
    result))

;; -----------------------------------------------------------------------------
(defun parse-compilation-unit (scanner state)
  (let* ((start-position (scanner-position scanner))
         (global-scope (current-scope (scope-stack state)))
         (definitions (parse-list parse-definition scanner state))
         (result (parse-result-match
                   (parse-action 'ast-compilation-unit
                                 (make-source-region start-position scanner)
                                 state
                                 :definitions (if (parse-result-no-match-p definitions)
                                                (parse-action 'ast-list
                                                              (make-source-region start-position start-position)
                                                              state
                                                              :local-scope global-scope)
                                                (let ((definitions (parse-result-value definitions)))
                                                  (setf (local-scope definitions) global-scope)
                                                  definitions))))))
    (if (not (scanner-at-end-p scanner))
        (not-implemented "parse error; unrecognized input"))
    result))

(deftest test-parse-compilation-unit-empty ()
  (test-parser parse-compilation-unit "" :is-match-p t :expected-type 'ast-compilation-unit
               :checks
               ((test-type 'ast-list (ast-unit-definitions ast))
                (test-equal nil (ast-list-nodes (ast-unit-definitions ast))))))

(deftest test-parse-compilation-unit-simple ()
  (test-parser parse-compilation-unit "type Foobar; type Barfoo;" :is-match-p t :expected-type 'ast-compilation-unit
               :checks
               ((test-type 'ast-list (ast-unit-definitions ast))
                (test-equal 2 (length (ast-list-nodes (ast-unit-definitions ast)))))))

(deftest test-parse-compilation-unit-consumes-all-input ()
  (test-parser parse-compilation-unit "type Foobar; fdklajskfl$#@%@@#% FDSF" :is-match-p nil))

(deftest test-parse-compilation-unit ()
  (test-parse-compilation-unit-empty)
  (test-parse-compilation-unit-simple)
  (test-parse-compilation-unit-consumes-all-input))

;; -----------------------------------------------------------------------------
(defsuite test-parsers ()
  (test-current-scope)
  (test-parse-whitespace)
  (test-parse-list)
  (test-parse-modifier)
  (test-parse-identifier)
  (test-parse-literal)
  (test-parse-type)
  (test-parse-expression)
  (test-parse-statement)
  (test-parse-definition)
  (test-parse-compilation-unit-simple))

