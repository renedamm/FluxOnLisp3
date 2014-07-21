
(in-package :fl)

;;;;============================================================================
;;;;    Parser State.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *parse-result-no-match* :no-match)

;; -----------------------------------------------------------------------------
(defparameter *parse-result-no-value* :no-value)

;; -----------------------------------------------------------------------------
(defparameter *parser-line-break-table* nil)

;; -----------------------------------------------------------------------------
(defparameter *parser-diagnostics* nil)

;;;;============================================================================
;;;;    Parser Helper Functions.
;;;;============================================================================

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
(defun parse-result-match-value-or (result default-value)
  (if (parse-result-match-p result)
    (parse-result-value result)
    default-value))

;; -----------------------------------------------------------------------------
(defun parse-result-match (&optional value)
  (if value
      value
      *parse-result-no-value*))

;; -----------------------------------------------------------------------------
(defun parse-result-no-match ()
  *parse-result-no-match*)

;; -----------------------------------------------------------------------------
(defmacro with-new-parser-state (&body body)
  `(let* ((*parser-line-break-table* (make-line-break-table))
          (*parser-diagnostics* nil))
     (with-new-toplevel-scope
       ,@body)))

;; -----------------------------------------------------------------------------
(defmacro test-parser (function string &key (is-match-p :dont-test) end-position expected-type line-breaks-at checks)
  (with-gensyms (res scanner end-position-value expected-type-value line-breaks-at-value string-value)
    `(with-new-parser-state
      (let*
        ((,string-value ,string)
         (,scanner (make-string-scanner ,string-value))
         (,res (,function ,scanner))
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
              (test-equal (length ,string-value) (scanner-position ,scanner))
              t))

          ;; Check value type, if requested.
          (if ,expected-type-value
            (test-type ,expected-type-value (if (parse-result-match-p ,res) (parse-result-value ,res) ,res))
            t)

          ;; Check line breaks, if requested.
          (if ,line-breaks-at-value
            (test-sequence-equal ,line-breaks-at-value *parser-line-break-table*)
            t)

          ;; Check AST.
          ,(if (> (length checks) 0)
             `(let ((ast (parse-result-value ,res)))
                (combine-results ,@checks))))))))

;; -----------------------------------------------------------------------------
(defun derive-definition-name-from-type (type-ast)
  (cond

    ((typep type-ast 'ast-named-type)
     (ast-type-name type-ast))
        
    (t
     (not-implemented (format nil "deriving names from ~a ASTs" (class-of type-ast))))))

(deftest test-derive-definition-name-from-type-simple ()
  (let ((id (derive-definition-name-from-type
              (make-instance 'ast-named-type
                             :name (make-identifier 'Foobar)))))
    (test-equal "FOOBAR" (symbol-name (ast-id-name id)))))

(deftest test-derive-definition-name-from-type ()
  (test-derive-definition-name-from-type-simple))

;; -----------------------------------------------------------------------------
(defun insert-declaration-into-symbol-table (declaration-kind ast &key (scope-index 0))
  ;; Definition AST must have name or type or both.
  (assert (or (ast-definition-type ast)
              (ast-definition-name ast)))
  (let ((name (ast-definition-name ast)))
    ;; If it doesn't have a name, derive it from the type.
    (if (not name)
      (setf name (derive-definition-name-from-type (ast-definition-type ast))))
    ;; Get the declaration.
    (let ((declaration (find-or-create-declaration
                         (get-current-scope :index scope-index)
                         declaration-kind
                         name)))
      ;; And add the definition.
      (add-definition declaration ast))))
  
(deftest test-insert-declaration-into-symbol-table-without-name ()
  (with-new-scope
    (let ((declaration (insert-declaration-into-symbol-table
                         *declaration-kind-variable*
                         (make-instance 'ast-variable-definition
                                        :name nil
                                        :type (make-instance 'ast-named-type
                                                             :name (make-identifier 'Foobar))))))
      (test-equal "Foobar" (symbol-name (declaration-name declaration)))
      (test-same declaration (find-or-create-declaration
                               (get-current-scope)
                               *declaration-kind-variable*
                               (make-identifier 'Foobar))))))

(deftest test-insert-declaration-into-symbol-table ()
  (test-insert-declaration-into-symbol-table-without-name))

;; -----------------------------------------------------------------------------
(defsuite test-parser-helpers ()
  (test-derive-definition-name-from-type))

;;;;============================================================================
;;;;    Parser Actions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; The default parse action is to just create an AST node whose type corresponds
;; to the name of the production and which is initialized from the given arguments.
(defmethod parse-action ((production symbol) region &rest rest)
  (apply 'make-instance (nconc (list production
                                     :source-region region)
                               rest)))

;; -----------------------------------------------------------------------------
(defmacro parse-action-for-definition (ast-class declaration-kind)
  `(defmethod parse-action ((production (eql ',ast-class)) region &rest rest)
     (declare (ignore rest region))
     (let* ((ast (call-next-method)))
       (insert-declaration-into-symbol-table ,declaration-kind ast)
       ast)))
  
(parse-action-for-definition ast-type-definition *declaration-kind-type*)
(parse-action-for-definition ast-function-definition *declaration-kind-function*)
(parse-action-for-definition ast-method-definition *declaration-kind-function*)
(parse-action-for-definition ast-module-definition *declaration-kind-module*)
(parse-action-for-definition ast-variable-definition *declaration-kind-variable*)
(parse-action-for-definition ast-value-parameter-definition *declaration-kind-variable*)

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
(defun parse-whitespace (scanner)
  (let ((initial-position (scanner-position scanner)))
    (flet ((line-break () (add-line-break *parser-line-break-table* (scanner-position scanner)))
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
(defmacro parse-list (parser-name scanner &key start-delimiter end-delimiter separator)
  ;; List must either be undelimited or have both a start and end
  ;; delimiter.
  (with-gensyms (scanner-value list start-position element parse-list-block)
    `(block ,parse-list-block
       (let ((,scanner-value ,scanner))

         (parse-whitespace ,scanner-value)
         (let ((,start-position (scanner-position ,scanner-value))
               ,element
               ,list)

           ;; Match start-delimiter.
           ,(if start-delimiter
                `(progn
                   (parse-whitespace ,scanner-value)
                   (if (not (scanner-match ,scanner-value ,start-delimiter))
                       (return-from ,parse-list-block (parse-result-no-match)))))

           ;; Match elements.
           (loop

              ;; Try to parse another element.
              (parse-whitespace ,scanner-value)
              (setf ,element (,parser-name ,scanner-value))
              (parse-whitespace ,scanner-value)

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
                       ;; one or more elements, we're missing an element.
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

                 ;; We have no separator and no end delimiter.  If we've already read elements
                 ;; or we have a start delimiter, simply return what we have.  Otherwise report
                 ;; an empty list by returning a nil value.
                 ,(if start-delimiter
                   `(return)
                   `(if ,list
                      (return)
                      (return-from ,parse-list-block (parse-result-no-match)))))))
                  
           (setf ,list (nreverse ,list))
           (parse-action 'ast-list
                         (make-source-region ,start-position ,scanner-value)
                         :nodes ,list))))))

(deftest test-parse-list ()
  (labels
      ;; Define a dummy parser that matches "a".
      ((parse-a (scanner)
         (if (scanner-match scanner #\a)
             (parse-result-match (make-instance 'ast-node
                                                :source-region (make-source-region
                                                                 (1- (scanner-position scanner))
                                                                 (scanner-position scanner))))
             (parse-result-no-match)))

       ;; Parser that matches "a, a, a...".
       (parse-comma-separated-as (scanner)
         (parse-list parse-a scanner :separator #\,)))
   
    (setf (get 'parse-a :diagnostic-type) (make-instance 'diagnostic-type :code 80000 :name "a"))

    (with-test-name test-no-match
      (test-parser parse-comma-separated-as "foo"
        :is-match-p nil
        :end-position 0))

    (with-test-name test-single-element
      (test-parser parse-comma-separated-as "a"
        :is-match-p t
        :expected-type 'ast-list
        :end-position 1))

    (with-test-name test-comma-separated
      (test-parser parse-comma-separated-as "a, a, a]"
        :is-match-p t
        :expected-type 'ast-list
        :end-position 7))))

;; -----------------------------------------------------------------------------
(defun parse-modifier (scanner)
  (let ((saved-position (scanner-position scanner)))
    (macrolet ((match (modifier ast-type)
                 `(if (scanner-match-keyword scanner ,modifier)
                      (return-from parse-modifier (parse-result-match
                                                    (parse-action ,ast-type
                                                                  (make-source-region saved-position scanner)))))))
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
(defun parse-modifier-list (scanner)
  (parse-list parse-modifier scanner))

(deftest test-parse-modifier-list-simple ()
  (test-parser parse-modifier-list "abstract immutable foobar"
               :is-match-p t
               :end-position 19
               :expected-type 'ast-list))

(deftest test-parse-modifier-list-no-match ()
  (test-parser parse-modifier-list "foobar"
               :is-match-p nil
               :end-position 0))

(deftest test-parse-modifier-list ()
  (test-parse-modifier-list-simple)
  (test-parse-modifier-list-no-match))

;; -----------------------------------------------------------------------------
(defun parse-identifier (scanner)
  ;;////TODO: parse qualified identifiers
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
                                            :name (make-declaration-name buffer))))
        (parse-result-no-match))))

(deftest test-parse-identifier-simple-name ()
  (test-parser parse-identifier "test"
               :end-position 4 :is-match-p t :expected-type 'ast-identifier
               :checks ((test-equal (intern "test" *config-default-output-package*) (ast-id-name ast))
                        (test-equal nil (ast-id-qualifier ast)))))

(deftest test-parse-identifier ()
  (test-parse-identifier-simple-name))

;; -----------------------------------------------------------------------------
(defun parse-clause (scanner)
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
    (parse-whitespace scanner)
    (let ((expression (parse-expression scanner)))
      (if (parse-result-no-match-p expression)
          (not-implemented "parse error; expecting expression"))
      (parse-result-match (parse-action clause-type
                                        (make-source-region saved-position scanner)
                                        :expression (parse-result-value expression))))))

;; -----------------------------------------------------------------------------
(defun parse-clause-list (scanner)
  (parse-list parse-clause scanner))

;; -----------------------------------------------------------------------------
(defun parse-attribute (scanner)
  (declare (ignore scanner))
  (parse-result-no-match));////TODO

;; -----------------------------------------------------------------------------
(defun parse-attribute-list (scanner)
  (parse-list parse-attribute scanner :start-delimiter #\[ :separator #\, :end-delimiter #\]))

;; -----------------------------------------------------------------------------
(defun parse-type (scanner)
  (let* ((saved-position (scanner-position scanner))
         (left-type (cond ((scanner-match scanner #\()
                           (parse-whitespace scanner)
                           (if (scanner-match scanner #\))
                               (parse-result-match (parse-action 'ast-nothing-type
                                                                 (make-source-region saved-position scanner)))
                               (not-implemented "parenthesized type expressions")))
                          (t
                           (let (modifiers name)
                             (setf modifiers (parse-modifier-list scanner))
                             (parse-whitespace scanner)
                             (setf name (parse-identifier scanner))
                             (if (parse-result-no-match-p name)
                               (if (parse-result-no-match-p modifiers)
                                 (return-from parse-type (parse-result-no-match))
                                 (not-implemented "parse error; expecting type name")))
                             (parse-result-match
                               (parse-action 'ast-named-type
                                             (make-source-region saved-position scanner)
                                             :name (parse-result-value name)
                                             :modifiers (parse-result-match-value-or modifiers nil))))))))
    (parse-whitespace scanner)

    (let ((combination-type (cond ((scanner-match-sequence scanner "->")
                                   'ast-function-type)
                                  ((scanner-match scanner #\&)
                                   'ast-union-type)
                                  ((scanner-match scanner #\|)
                                   'ast-intersection-type)
                                  (t nil))))
      (if (not combination-type)
          (return-from parse-type left-type))

      (parse-whitespace scanner)
      (let ((right-type (parse-type scanner)))
        (if (parse-result-no-match-p right-type)
            (not-implemented "parse error; expecting right type"))
        (parse-result-match (parse-action combination-type
                                          (make-source-region saved-position scanner)
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
(defun parse-type-parameter (scanner)
  (declare (ignore scanner))
  (parse-result-no-match));////TODO

;; -----------------------------------------------------------------------------
(defun parse-type-parameter-list (scanner)
  "Parse a list of type parameters surrounded by '<' and '>'."
  (parse-list parse-type-parameter scanner :start-delimiter #\< :separator #\, :end-delimiter #\>))

;; -----------------------------------------------------------------------------
(defun parse-value-parameter (scanner)
  (let* ((start-position (scanner-position scanner))
         attributes modifiers identifier type value)
    
    ;; Parse attributes, if any.
    (parse-whitespace scanner)
    (setf attributes (parse-attribute-list scanner))

    ;; Parse modifiers, if any.
    (parse-whitespace scanner)
    (setf modifiers (parse-modifier-list scanner))
    
    ;; Parse name, if any.
    (parse-whitespace scanner)
    (setf identifier (parse-identifier scanner))
    
    ;; Parse type.
    (parse-whitespace scanner)
    (if (not (scanner-match scanner #\:))

      ;; This is a short-hand value parameter without a separate name.
      ;; Reset to where we started and parse a type.
      (progn
        (setf (scanner-position scanner) start-position)
        (setf identifier (parse-result-no-match))))
      
    (parse-whitespace scanner)
    (setf type (parse-type scanner))

    (if (and (parse-result-no-match-p identifier)
             (parse-result-no-match-p type))
      (progn
        (setf (scanner-position scanner) start-position)
        (return-from parse-value-parameter (parse-result-no-match))))

    ;; Parse value.
    (parse-whitespace scanner)
    (if (scanner-match scanner #\=)
      (progn
        (parse-whitespace scanner)
        (setf value (parse-expression scanner))))
    
    ;;////TODO: deal with parse error
    (parse-result-value
      (parse-action 'ast-value-parameter-definition
                    (make-source-region start-position scanner)
                    :attributes attributes
                    :modifiers (parse-result-match-value-or modifiers nil)
                    :name (parse-result-match-value-or identifier nil)
                    :type type
                    :value value))))

(deftest test-parse-value-parameter-simple ()
  (test-parser parse-value-parameter "Foo : Integer"
               :is-match-p t
               :expected-type 'ast-value-parameter-definition))

(deftest test-parse-value-parameter-shorthand ()
  (test-parser parse-value-parameter "Integer"
               :is-match-p t
               :expected-type 'ast-value-parameter-definition))

(deftest test-parse-value-parameter-simple-with-value ()
  (test-parser parse-value-parameter "Foo : Integer = 1"
               :is-match-p t
               :expected-type 'ast-value-parameter-definition))

(deftest test-parse-value-parameter-shorthand-with-value ()
  (test-parser parse-value-parameter "Integer = 1"
               :is-match-p t
               :expected-type 'ast-value-parameter-definition))

(deftest test-parse-value-parameter-with-implicit-type ()
  (test-parser parse-value-parameter "Foo := 1"
               :is-match-p t
               :expected-type 'ast-value-parameter-definition))

(deftest test-parse-value-parameter-none ()
  (test-parser parse-value-parameter "123" :is-match-p nil))

(deftest test-parse-value-parameter ()
  (test-parse-value-parameter-simple)
  (test-parse-value-parameter-shorthand)
  (test-parse-value-parameter-simple-with-value)
  (test-parse-value-parameter-shorthand-with-value)
  (test-parse-value-parameter-none))
         
;; -----------------------------------------------------------------------------
(defun parse-value-parameter-list (scanner)
  "Parse a list of value parameters surrounded by '(' and ')'."
  (parse-list parse-value-parameter scanner :start-delimiter #\( :separator #\, :end-delimiter #\)))

;; -----------------------------------------------------------------------------
(defun parse-statement (scanner)
  (let ((saved-position (scanner-position scanner)))
    (if (scanner-at-end-p scanner)
      (return-from parse-statement (parse-result-no-match)))
    (cond

      ;; Parse return statement.
      ((scanner-match-keyword scanner "return")
       (parse-whitespace scanner)
       (let (expression)
         (if (not (scanner-match scanner #\;))
           (progn
             (setf expression (parse-expression scanner))
             (if (parse-result-no-match-p expression)
               (not-implemented "parse error; expecting expression after 'return'"))
             (if (not (scanner-match scanner #\;))
               (not-implemented "parse error; expecting ';'"))))
         (parse-result-match
           (parse-action 'ast-return-statement
                         (make-source-region saved-position scanner)
                         :expression (parse-result-value expression)))))

      ;; Parse block statement.
      ((equal #\{ (scanner-peek-next scanner))
       (with-new-scope
         (let ((statements (parse-statement-list scanner)))
           (if (not (parse-result-match-p statements))
             (not-implemented "parse error in statement list"))
           (parse-result-match
             (parse-action 'ast-block-statement
                           (make-source-region saved-position scanner)
                           :statements (parse-result-value statements)
                           :scope (get-current-scope))))))

      (t
       (parse-result-no-match)))))

(deftest test-parse-statement-return-without-value ()
  (test-parser parse-statement "return ;" :is-match-p t :expected-type 'ast-return-statement))

(deftest test-parse-statement-empty-block ()
  (test-parser parse-statement "{}"
               :is-match-p t
               :expected-type 'ast-block-statement))

(deftest test-parse-statement-simple-block ()
  (test-parser parse-statement "{ return 1; }"
               :is-match-p t
               :expected-type 'ast-block-statement
               :checks ((test-equal 1 (length (ast-list-nodes (get-statements ast)))))))

(deftest test-parse-statement ()
  (test-parse-statement-return-without-value)
  (test-parse-statement-empty-block)
  (test-parse-statement-simple-block))

;; -----------------------------------------------------------------------------
;; Parse a list of statements surrounded by '{' and '}'.
(defun parse-statement-list (scanner)
  (let ((result (parse-list parse-statement scanner :start-delimiter #\{ :end-delimiter #\})))
    (if (parse-result-match-p result)
      (setf (get-local-scope result) (get-current-scope)))
    result))

(deftest test-parse-statement-list-empty ()
  (test-parser parse-statement-list "{}"
               :is-match-p t
               :expected-type 'ast-list))

(deftest test-parse-statement-list-no-match ()
  (test-parser parse-statement-list "  foo"
               :is-match-p nil
               :end-position 2))

(deftest test-parse-statement-list-simple ()
  (test-parser parse-statement-list "{ return 1; }"
               :is-match-p t
               :expected-type 'ast-list))

(deftest test-parse-statement-list ()
  (test-parse-statement-list-empty)
  (test-parse-statement-list-no-match)
  (test-parse-statement-list-simple))

;; -----------------------------------------------------------------------------
(defun parse-character (scanner)
  (if (scanner-at-end-p scanner)
    (parse-result-no-match)
    (let ((next-char (scanner-read-next scanner)))
      (if (equal next-char #\\)
        (not-implemented "escape sequences")
        (parse-result-match next-char)))))

(deftest test-parse-character-simple ()
  (test-parser parse-character "c"
               :is-match-p t
               :expected-type 'character
               :checks ((test-equal #\c ast))))

(deftest test-parse-character ()
  (test-parse-character-simple))

;; -----------------------------------------------------------------------------
(defun parse-literal (scanner)
  (if (scanner-at-end-p scanner)
      (parse-result-no-match)
      (let ((saved-position (scanner-position scanner))
            (next-char (scanner-read-next scanner)))
        (cond 

            ((equal next-char #\")
             (let ((buffer (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character)))
               (loop while (not (scanner-match scanner #\"))
                     do
                     (setf next-char (parse-character scanner))
                     (if (parse-result-no-match-p next-char)
                       (not-implemented "expecting character"))
                     (vector-push-extend (parse-result-value next-char) buffer))
               (parse-result-match
                 (parse-action 'ast-string-literal
                               (make-source-region saved-position scanner)
                               ;;////TODO: strings should go into a table
                               :value buffer))))

            ((equal next-char #\')
             (let ((char (parse-character scanner)))
               (if (parse-result-no-match-p char)
                 (not-implemented "expecting character"))
               (if (not (scanner-match scanner #\'))
                 (not-implemented "expecting '"))
               (parse-result-match
                 (parse-action 'ast-character-literal
                               (make-source-region saved-position scanner)
                               :value (parse-result-value char)))))

            ((digit-char-p next-char)
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
                                                 :value value))))

            (t (not-implemented "literal"))))))

(deftest test-parse-literal-integer ()
  (test-parser parse-literal "12" :is-match-p t :expected-type 'ast-integer-literal
               :checks ((test-equal 12 (ast-literal-value ast)))))

(deftest test-parse-literal-string-simple ()
  (test-parser parse-literal "\"foo\""
               :is-match-p t
               :expected-type 'ast-string-literal
               :checks ((test-equal "foo" (ast-literal-value ast)))))

(deftest test-parse-literal ()
  (test-parser parse-literal "" :is-match-p nil)
  (test-parse-literal-integer)
  (test-parse-literal-string-simple))

;; -----------------------------------------------------------------------------
(defun parse-expression (scanner)
  (if (scanner-at-end-p scanner)
      (return-from parse-expression (parse-result-no-match)))

  (let (expression
        (start-pos (scanner-position scanner)))

    ;; Parse prefix expression.
    ;;////TODO
  
    ;; Parse core expression.
    (let ((next-char (scanner-peek-next scanner)))
      (setf expression (cond ((or (digit-char-p next-char)
                                  (char-equal #\" next-char)
                                  (char-equal #\' next-char))
                              (parse-literal scanner))
                             (t
                              (parse-result-no-match)))))
  
    ;; Parse postfix.
    (loop
      named parse-postfix
      do (parse-whitespace scanner)
         (cond

           ;; Parse dot expression.
           ((scanner-match scanner #\.)
            (parse-whitespace scanner)
            (let ((name (parse-identifier scanner)))
              (if (not (parse-result-match-p name))
                (not-implemented "expecting identifier after '.'"))
              (setf expression (parse-result-value
                                 (parse-action 'ast-dot-expression
                                                (make-source-region start-pos (scanner-position scanner))
                                                :value (parse-result-value expression)
                                                :member name)))))

           (t
            (return-from parse-postfix))))
    
    expression))

(deftest test-parse-expression-literal ()
  (test-parser parse-expression "512" :is-match-p t :expected-type 'ast-integer-literal
               :checks ((test-equal 512 (ast-literal-value ast)))))

(deftest test-parse-expression-dot-simple ()
  (test-parser parse-expression "1.foo"
               :is-match-p t
               :expected-type 'ast-dot-expression
               :checks ((test-type 'ast-integer-literal (get-value-expression ast))
                        (test-type 'ast-identifier (get-member-name ast)))))

(deftest test-parse-expression-dot-multiple ()
  (test-parser parse-expression "1.foo.bar"
               :is-match-p t
               :expected-type 'ast-dot-expression
               :checks ((test-type 'ast-dot-expression (get-value-expression ast))
                        (test-equal "bar" (symbol-name (ast-id-name (get-member-name ast))))
                        (test-type 'ast-integer-literal (get-value-expression (get-value-expression ast))))))

(deftest test-parse-expression ()
  (test-parse-expression-literal)
  (test-parse-expression-dot-simple)
  (test-parse-expression-dot-multiple))

;; -----------------------------------------------------------------------------
(defun parse-definition (scanner)
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

    (parse-whitespace scanner)
    (setf start-position (scanner-position scanner))

    ;; Parse attributes.
    (setf attributes (parse-attribute-list scanner))

    ;; Parse modifiers.
    (setf modifiers (parse-modifier-list scanner))

    ;;////FIXME: this needs to match keyword+whitespace, not just keyword
    ;; Parse definition kind.
    (parse-whitespace scanner)
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
    (parse-whitespace scanner)
    (setf name (parse-identifier scanner))

    ;; Enter local scope for bindings belonging to the definition.
    (with-new-scope
    
      ;; Parse type parameters.
      (setf type-parameters (parse-type-parameter-list scanner))

      ;; Parse value parameters.
      (setf value-parameters (parse-value-parameter-list scanner))

      ;; Parse type.
      (parse-whitespace scanner)
      (if (scanner-match scanner #\:)
        (progn
          (parse-whitespace scanner)
          (setf type (parse-type scanner))))

      ;; Parse clauses.
      (setf clauses (parse-clause-list scanner))

      ;; Parse body/value.
      (parse-whitespace scanner)
      (cond ((scanner-match scanner #\;)
             t)
            ;;////TODO: if it's a type definition, we need to parse a type here when hitting '='
            ((scanner-match scanner #\=)
             (parse-whitespace scanner)
             (setf value (parse-expression scanner))
             (parse-whitespace scanner)
             (if (not (scanner-match scanner #\;))
               (not-implemented "parse error; expecting semicolon")))
            (t
             (setf body
                   ;;////FIXME: this should be unified into one single parsing path for both module and other definitions
                   (if (eq 'ast-module-definition definition-class)
                     (parse-definition-list scanner)
                     (parse-statement-list scanner))))))

    ;; Create AST node.
    (setf ast (parse-action definition-class
                            (make-source-region start-position scanner)
                            :attributes attributes
                            :modifiers (parse-result-match-value-or modifiers nil)
                            :name name
                            :type-parameters (parse-result-match-value-or type-parameters nil)
                            :value-parameters (parse-result-match-value-or value-parameters nil)
                            :type type
                            :clauses clauses
                            :value value
                            :body body))

    (parse-result-match ast)))

(deftest test-parse-definition-type-simple ()
  (test-parser parse-definition "type Foobar;"
               :is-match-p t :expected-type 'ast-type-definition
               :checks ((test-equal "Foobar" (identifier-to-string (ast-definition-name ast)))
                        (test (lookup-declaration *declaration-kind-type*
                                                  (make-identifier (make-declaration-name "Foobar")))))))

(deftest test-parse-definition-function-simple ()
  (test-parser parse-definition "function Foobar : () -> () {}"
               :is-match-p t
               :expected-type 'ast-function-definition
               :checks ((test-equal "Foobar" (identifier-to-string (ast-definition-name ast)))
                        (test-equal nil (ast-definition-value-parameters ast))
                        (test-equal nil (ast-definition-type-parameters ast)))))

(deftest test-parse-definition-method-with-arguments ()
  (test-parser parse-definition "method Foobar( Foo : Integer ) {}"
               :is-match-p t
               :expected-type 'ast-method-definition
               :checks ((test-equal "Foobar" (identifier-to-string (ast-definition-name ast)))
                        (test-equal 1 (length (ast-list-nodes (ast-definition-value-parameters ast)))))))

(deftest test-parse-definition-module-simple ()
  (test-parser parse-definition "module Foobar {}"
               :is-match-p t
               :expected-type 'ast-module-definition
               :checks ((test-equal "Foobar" (identifier-to-string (ast-definition-name ast))))))

(deftest test-parse-definition-rejects-non-definition ()
  (test-parser parse-definition "Foobar" :is-match-p nil))

(deftest test-parse-definition ()
  (test-parse-definition-type-simple)
  (test-parse-definition-function-simple)
  (test-parse-definition-method-with-arguments)
  (test-parse-definition-module-simple)
  (test-parse-definition-rejects-non-definition))

;; -----------------------------------------------------------------------------
(defun parse-definition-list (scanner)
  (let ((result (parse-list parse-definition scanner :start-delimiter #\{ :end-delimiter #\})))
    (if (parse-result-match-p result)
      (setf (slot-value (parse-result-value result) 'local-scope) (get-current-scope)))
    result))

;; -----------------------------------------------------------------------------
(defun parse-compilation-unit (scanner)
  (let* ((start-position (scanner-position scanner))
         (global-scope (get-current-scope))
         (definitions (parse-list parse-definition scanner))
         (result (parse-result-match
                   (parse-action 'ast-compilation-unit
                                 (make-source-region start-position scanner)
                                 :definitions (if (parse-result-no-match-p definitions)
                                                (parse-action 'ast-list
                                                              (make-source-region start-position start-position)
                                                              :local-scope global-scope)
                                                (let ((definitions (parse-result-value definitions)))
                                                  (setf (get-local-scope definitions) global-scope)
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
  ;;////TODO: diagnostics not yet implemented
  ())
  ;(test-parser parse-compilation-unit "type Foobar; fdklajskfl$#@%@@#% FDSF" :is-match-p nil))

(deftest test-parse-compilation-unit ()
  (test-parse-compilation-unit-empty)
  (test-parse-compilation-unit-simple)
  (test-parse-compilation-unit-consumes-all-input))

;; -----------------------------------------------------------------------------
(defsuite test-parsers ()
  (test-parse-whitespace)
  (test-parse-list)
  (test-parse-modifier)
  (test-parse-modifier-list)
  (test-parse-identifier)
  (test-parse-character)
  (test-parse-literal)
  (test-parse-type)
  (test-parse-expression)
  (test-parse-statement)
  (test-parse-statement-list)
  (test-parse-value-parameter)
  (test-parse-definition)
  (test-parse-compilation-unit))

