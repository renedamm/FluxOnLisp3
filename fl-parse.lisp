
(in-package :fl)

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

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *parse-result-no-match* :no-match)

;; -----------------------------------------------------------------------------
(defparameter *parse-result-no-value* :no-value)

;; -----------------------------------------------------------------------------
(defparameter *parser-line-break-table* nil)

;; -----------------------------------------------------------------------------
(defparameter *parser-diagnostics* nil)

;; -----------------------------------------------------------------------------
;; Package in which to intern symbols parsed for identifiers parsed from source.
(defparameter *parser-symbol-package* nil)

;;;;============================================================================
;;;;    Macros.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defmacro with-new-parser-state ((&key symbol-package) &body body)
  (with-gensyms (symbol-package-value)
    `(let* ((,symbol-package-value ,symbol-package)
            (*parser-symbol-package*
              (cond ((packagep ,symbol-package-value)
                     ,symbol-package-value)
                    (,symbol-package-value
                      (let ((package (find-package ,symbol-package-value)))
                        (if (not package)
                          (setf package
                                (make-package ,symbol-package-value :use :common-lisp)))
                        package))
                    (t
                     *config-default-output-package*)))
            (*parser-line-break-table* (make-line-break-table))
            (*parser-diagnostics* nil))
       ,@body)))

;; -----------------------------------------------------------------------------
(defmacro test-parser (function string &key (is-match-p :dont-test) end-position expected-type line-breaks-at checks)
  (with-gensyms (res scanner end-position-value expected-type-value line-breaks-at-value string-value)
    `(with-new-parser-state ()
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
            (test-equal ,end-position-value (get-position ,scanner))
            (if ,(and is-match-p (not (eq is-match-p :dont-test)))
              (test-equal (length ,string-value) (get-position ,scanner))
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

;;;;============================================================================
;;;;    Functions.
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
(defun parse-whitespace (scanner)
  (let ((initial-position (get-position scanner)))
    (flet ((line-break () (add-line-break *parser-line-break-table* (get-position scanner)))
           (consume () (read-next-token scanner)))
      (loop
         (if (is-at-end-p scanner)
             (return))
         (let ((char (peek-next-token scanner)))
           (cond ((equal char #\Newline)
                  (consume)
                  (line-break))
                 ((equal char #\Return)
                  (consume)
                  (match-next-token scanner #\Newline)
                  (line-break))
                 ((or (equal char #\Tab)
                      (equal char #\Space))
                  (consume))
                 ((equal char #\/)
                  (read-next-token scanner)
                  (cond ((match-next-token scanner #\/)
                         (loop
                            (if (or (is-at-end-p scanner)
                                    (not (match-next-token-if scanner
                                                           (lambda (char) (not (or (equal char #\Newline)
                                                                                   (equal char #\Return)))))))
                                (return))))
                        ((match-next-token scanner #\*)
                         (let ((nesting-level 1))
                           (loop
                              (if (or (zerop nesting-level)
                                      (is-at-end-p scanner))
                                  (return))
                              (let ((next-char (read-next-token scanner)))
                                (cond ((and (equal next-char #\/)
                                            (match-next-token scanner #\*))
                                       (incf nesting-level))
                                      ((and (equal next-char #\*)
                                            (match-next-token scanner #\/))
                                       (decf nesting-level))
                                      ((equal next-char #\Return)
                                       (match-next-token scanner #\Newline)
                                       (line-break))
                                      ((equal next-char #\Newline)
                                       (line-break)))))))
                        (t
                         (set-position (1- (get-position scanner)) scanner)
                         (return))))
                 (t
                  (return))))))
    (if (not (equal initial-position (get-position scanner)))
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
(defmacro parse-list (parser-name scanner &key start-delimiter end-delimiter separator can-repeat)
  ;; List must either be undelimited or have both a start and end
  ;; delimiter.
  (with-gensyms (scanner-value list list-count start-position element parse-single-list-block result)
    `(let* ((,scanner-value ,scanner)
            ,list
            (,list-count 0)
            ,start-position)
       (parse-whitespace ,scanner)
       (setf ,start-position (get-position ,scanner))
       (loop
         (let

           ;; Parse a single delimited list adding all elements to 'list'.
           ((,result
             (block ,parse-single-list-block
               (incf ,list-count)
               (let (,element)

                 ;; Match start-delimiter.
                 ,(if start-delimiter
                      `(progn
                         (parse-whitespace ,scanner-value)
                         (if (not (match-next-token ,scanner-value ,start-delimiter))
                             (return-from ,parse-single-list-block (parse-result-no-match)))))

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
                        `(if (not (match-next-token ,scanner-value ,separator))
                             (return)))

                       ;; In a list with both separators and an end-delimiter, we
                       ;; continue if we see a separator and we stop if we see an
                       ;; end-delimiter.
                       ((and separator end-delimiter)
                        `(if (match-next-token ,scanner-value ,separator)
                             t
                             (if (match-next-token ,scanner-value ,end-delimiter)
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
                           ((match-next-token ,scanner-value ,separator)
                            (not-implemented "error; expecting element"))

                           ;; Otherwise, if we haven't yet parsed any elements and we don't
                           ;; have an end-delimiter, we consider it a no-match rather than a match
                           ;; against an empty list.
                           ((and (not ,list)
                                 ,(not end-delimiter))
                            (return-from ,parse-single-list-block (parse-result-no-match)))))

                     ,(if end-delimiter
                        `(if (not (match-next-token ,scanner-value ,end-delimiter))
                           (not-implemented "error; expecting element or terminator")
                           (return)))

                     ;; We have no separator and no end delimiter.  If we've already read elements
                     ;; or we have a start delimiter, simply return what we have.  Otherwise report
                     ;; an empty list by returning a nil value.
                     ,(if start-delimiter
                       `(return)
                       `(if ,list
                          (return)
                          (return-from ,parse-single-list-block (parse-result-no-match)))))))))))

                ;; Decide whether to continue on or return.
                ,(if can-repeat
                  `(cond
                     ;; No match if parse didn't progress and we have no prior successful parse.
                     ((and (parse-result-no-match-p ,result)
                           (eq 1 ,list-count))
                      (return ,result))

                     ;; Match if parse didn't progress and we have a prior successful parse.
                     ((and (parse-result-no-match-p ,result)
                           (> ,list-count 1))
                      (return
                        (parse-result-match
                          (make-instance 'ast-list
                                         :source-region (make-source-region ,start-position ,scanner-value)
                                         :nodes (nreverse ,list))))) ; List may be empty (e.g. "[] []").
                     (t nil)) ; Just keep going.

                  ;; For non-repeating lists, one successful parse is all we want.
                  `(if (parse-result-no-match-p ,result)
                     (return ,result)
                     (return
                       (parse-result-match
                         (make-instance 'ast-list
                                        :source-region (make-source-region ,start-position ,scanner-value)
                                        :nodes (nreverse ,list)))))))))))


(deftest test-parse-list ()
  (labels
      ;; Define a dummy parser that matches "a".
      ((parse-a (scanner)
         (if (match-next-token scanner #\a)
             (parse-result-match (make-instance 'ast-node
                                                :source-region (make-source-region
                                                                 (1- (get-position scanner))
                                                                 (get-position scanner))))
             (parse-result-no-match)))

       ;; Parser that matches "a, a, a...".
       (parse-comma-separated-as (scanner)
         (parse-list parse-a scanner :separator #\,))

       (parse-repeating-list-of-as (scanner)
         (parse-list parse-a scanner :start-delimiter #\[ :separator #\, :end-delimiter #\] :can-repeat t))

       (parse-delimited-list-of-as (scanner)
         (parse-list parse-a scanner :start-delimiter #\( :separator #\, :end-delimiter #\))))
   
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
        :end-position 7))

    (with-test-name test-repeating-list
      (test-parser parse-repeating-list-of-as " [ a, a ] [ a ]"
        :is-match-p t
        :expected-type 'ast-list))

    (with-test-name test-empty-list
      (test-parser parse-delimited-list-of-as " ()"
        :is-match-p t
        :expected-type 'ast-list))

    (with-test-name test-empty-list-with-repeat
      (test-parser parse-repeating-list-of-as " []"
        :is-match-p t
        :expected-type 'ast-list))))

    ;;////TODO: does not work yet
    ;(with-test-name test-trailing-separators
      ;(test-parser parse-comma-separated-as "a, a,"
        ;:is-match-p t
        ;:expected-type 'ast-list))))

;; -----------------------------------------------------------------------------
(defun parse-modifier (scanner)
  (let ((saved-position (get-position scanner)))
    (macrolet ((match (modifier ast-type)
                 `(if (match-keyword scanner ,modifier)
                      (return-from parse-modifier (parse-result-match
                                                    (make-instance ,ast-type
                                                                  :source-region (make-source-region saved-position scanner)))))))
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
  ;;////TODO: escaped identifiers
  ;;////TODO: global qualifier
  (let ((start-position (get-position scanner))
        (next-char (peek-next-token scanner))
        (buffer (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character))
        (expecting-name-after-qualifier nil)
        (current-qualifier nil))
    (flet ((buffer-to-identifier ()
             ;;////TODO: simply use string intern table (shared with string literals) instead?  leave rest to IR symbol stuff.
             ;;  using Lisp symbols is a bit of an abuse
             (let ((name (intern buffer *parser-symbol-package*)))
               (setf (fill-pointer buffer) 0)
               (make-instance 'ast-identifier
                              :source-region (make-source-region start-position scanner)
                              :name name
                              :qualifier current-qualifier))))
      (if (or (alpha-char-p next-char)
              (equal next-char #\_))
        (progn
          (read-next-token scanner)
          (vector-push-extend next-char buffer)
          (loop
            (if (is-at-end-p scanner)
              (return))
            (setf next-char (peek-next-token scanner))

            (if (and (equal next-char #\:)
                     (match-token-sequence scanner "::"))

              ;; Handle qualifier.
              (progn
                (if expecting-name-after-qualifier
                  (not-implemented "Parse error: expecting name after '::'"))
                (if (zerop (length buffer))
                  (not-implemented "global qualifier"))
                (setf current-qualifier (buffer-to-identifier))
                (setf expecting-name-after-qualifier t))

              ;; Handle normal characters.
              (progn
                (if (and (not (alphanumericp next-char))
                         (not (equal next-char #\_)))
                  (progn
                    (if expecting-name-after-qualifier
                      (not-implemented "Parse error: expecting name after '::'"))
                    (return)))
                (vector-push-extend next-char buffer)
                (setf expecting-name-after-qualifier nil)
                (read-next-token scanner))))
          (parse-result-match (buffer-to-identifier)))
        (parse-result-no-match)))))

(deftest test-parse-identifier-simple-name ()
  (test-parser parse-identifier "test"
               :end-position 4 :is-match-p t :expected-type 'ast-identifier
               :checks ((test-same (intern "test" *config-default-output-package*) (get-name ast))
                        (test-equal nil (get-qualifier ast)))))

(deftest test-parse-identifier-qualified ()
  (test-parser parse-identifier "a::b"
               :end-position 4 :is-match-p t :expected-type 'ast-identifier
               :checks ((test-same (intern "b" *config-default-output-package*) (get-name ast))
                        (test-type 'ast-identifier (get-qualifier ast))
                        (test-same (intern "a" *config-default-output-package*) (get-name (get-qualifier ast))))))
         
(deftest test-parse-identifier ()
  (test-parse-identifier-simple-name)
  (test-parse-identifier-qualified))

;; -----------------------------------------------------------------------------
(defun parse-clause (scanner)
  (let ((saved-position (get-position scanner))
        (clause-type (cond ((match-keyword scanner "when")
                            'ast-when-clause)
                           ((match-keyword scanner "requires")
                            'ast-requires-clause)
                           ((match-keyword scanner "ensures")
                            'ast-ensures-clause)
                           ((match-keyword scanner "invariant")
                            'ast-invariant-clause)
                           (t
                            (return-from parse-clause (parse-result-no-match))))))
    (parse-whitespace scanner)
    (let ((expression (parse-expression scanner)))
      (if (parse-result-no-match-p expression)
        (not-implemented "parse error; expecting expression"))
      (parse-result-match (make-instance clause-type
                                         :source-region (make-source-region saved-position scanner)
                                         :expression (parse-result-value expression))))))

;; -----------------------------------------------------------------------------
(defun parse-clause-list (scanner)
  (parse-list parse-clause scanner))

;; -----------------------------------------------------------------------------
(defun parse-attribute (scanner)
  (let ((saved-position (get-position scanner))
        (name-result nil)
        (arguments-result nil))
    (parse-whitespace scanner)
    (setf name-result (parse-identifier scanner))
    (if (parse-result-no-match-p name-result)
      (parse-result-no-match)
      (progn
        (parse-whitespace scanner)
        (setf arguments-result (parse-list parse-expression scanner :start-delimiter #\( :separator #\, :end-delimiter #\)))
        (parse-result-match (make-instance 'ast-attribute
                                           :source-region (make-source-region saved-position scanner)
                                           :name (parse-result-value name-result)
                                           :arguments (if (parse-result-match-p arguments-result)
                                                          (parse-result-value arguments-result))))))))

(deftest test-parse-single-identifier-attribute ()
  (test-parser parse-attribute "test"
               :end-position 4 :is-match-p t :expected-type 'ast-attribute
               :checks ((test-equal "test" (identifier-to-string (get-identifier ast)))
                        (test-equal nil (get-arguments ast)))))

(deftest test-parse-attribute-with-arguments ()
  (test-parser parse-attribute "test(1,2)"
               :end-position 9 :is-match-p t :expected-type 'ast-attribute
               :checks ((test-equal "test" (identifier-to-string (get-identifier ast)))
                        (test-equal 2 (length (get-list (get-arguments ast)))))))

(deftest test-parse-attribute ()
  (test-parse-single-identifier-attribute)
  (test-parse-attribute-with-arguments))

;; -----------------------------------------------------------------------------
;;////TODO: need to parse attribute lists like [ Foo ] [ Bar ]
(defun parse-attribute-list (scanner)
  (parse-list parse-attribute scanner :start-delimiter #\[ :end-delimiter #\] :can-repeat t))

(deftest test-parse-empty-attribute-list ()
  (test-parser parse-attribute-list " [] "
               :is-match-p t
               :expected-type 'ast-list
               :checks ((test-equal 0 (length (get-list ast))))))

(deftest test-parse-attribute-list-with-multiple-attributes ()
  (test-parser parse-attribute-list " [ A B( 1 ) ]"
               :is-match-p t
               :expected-type 'ast-list
               :checks ((test-equal 2 (length (get-list ast))))))

(deftest test-parse-multi-attribute-list ()
  (test-parser parse-attribute-list " [ A ] [ B( 1 ) ]"
               :is-match-p t
               :expected-type 'ast-list
               :checks ((test-equal 2 (length (get-list ast))))))

(deftest test-parse-attribute-list ()
  (test-parse-empty-attribute-list)
  (test-parse-attribute-list-with-multiple-attributes)
  (test-parse-multi-attribute-list))

;; -----------------------------------------------------------------------------
(defun parse-type (scanner)
  (let* ((saved-position (get-position scanner))
         (left-type (cond ((equal #\( (peek-next-token scanner))
                           (let ((type-list
                                   ;;////TODO: ellipsis at end
                                   (parse-list parse-type scanner
                                               :start-delimiter #\(
                                               :separator #\,
                                               :end-delimiter #\))))
                             (if (parse-result-match-p type-list)
                               (let* ((element-types (get-list (parse-result-value type-list)))
                                      (num-element-types (length element-types))
                                      (source-region (get-source-region type-list)))
                                 (cond
                                   ((eq 0 num-element-types)
                                     (make-instance 'ast-nothing-type
                                                    :source-region source-region))
                                    ((eq 1 num-element-types)
                                     (first element-types))
                                    (t
                                     (make-instance 'ast-tuple-type
                                                    :source-region source-region
                                                    :component-types element-types)))))))
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
                               (make-instance 'ast-named-type
                                             :source-region (make-source-region saved-position scanner)
                                             :name (parse-result-value name)
                                             :modifiers (parse-result-match-value-or modifiers nil))))))))
    (parse-whitespace scanner)

    (let ((combination-type (cond ((match-token-sequence scanner "->")
                                   'ast-function-type)
                                  ((match-next-token scanner #\&)
                                   'ast-union-type)
                                  ((match-next-token scanner #\|)
                                   'ast-intersection-type)
                                  (t nil))))
      (if (not combination-type)
          (return-from parse-type left-type))

      (parse-whitespace scanner)
      (let ((right-type (parse-type scanner)))
        (if (parse-result-no-match-p right-type)
            (not-implemented "parse error; expecting right type"))
        (parse-result-match (make-instance combination-type
                                          :source-region (make-source-region saved-position scanner)
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
               :checks ((test-type 'ast-named-type (get-left-type ast))
                        (test-type 'ast-named-type (get-right-type ast)))))

(deftest test-parse-parenthesized-type ()
  (test-parser parse-type "( Foobar )"
               :is-match-p t
               :expected-type 'ast-named-type))

(deftest test-parse-tuple-type ()
  (test-parser parse-type "( Foobar, Barfoo )"
               :is-match-p t
               :expected-type 'ast-tuple-type
               :checks ((test-type 'ast-named-type (first (get-component-types ast)))
                        (test-type 'ast-named-type (second (get-component-types ast))))))

(deftest test-parse-type ()
  (test-parse-simple-named-type)
  (test-parse-nothing-type)
  (test-parse-simple-function-type)
  (test-parse-simple-union-type)
  (test-parse-parenthesized-type)
  (test-parse-tuple-type))

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
  (let* ((start-position (get-position scanner))
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
    (if (not (match-next-token scanner #\:))

      ;; This is a short-hand value parameter without a separate name.
      ;; Reset to where we started and parse a type.
      (progn
        (set-position start-position scanner)
        (setf identifier (parse-result-no-match))))
      
    (parse-whitespace scanner)
    (setf type (parse-type scanner))

    (if (and (parse-result-no-match-p identifier)
             (parse-result-no-match-p type))
      (progn
        (set-position start-position scanner)
        (return-from parse-value-parameter (parse-result-no-match))))

    ;; Parse value.
    (parse-whitespace scanner)
    (if (match-next-token scanner #\=)
      (progn
        (parse-whitespace scanner)
        (setf value (parse-expression scanner))))
    
    ;;////TODO: deal with parse error
    (parse-result-value
      (make-instance 'ast-value-parameter-definition
                    :source-region (make-source-region start-position scanner)
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
  (let ((saved-position (get-position scanner)))
    (if (is-at-end-p scanner)
      (return-from parse-statement (parse-result-no-match)))
    (cond

      ;; Parse return statement.
      ((match-keyword scanner "return")
       (parse-whitespace scanner)
       (let (expression)
         (if (not (match-next-token scanner #\;))
           (progn
             (setf expression (parse-expression scanner))
             (if (parse-result-no-match-p expression)
               (not-implemented "parse error; expecting expression after 'return'"))
             (parse-whitespace scanner)
             (if (not (match-next-token scanner #\;))
               (not-implemented "parse error; expecting ';'"))))
         (parse-result-match
           (make-instance 'ast-return-statement
                         :source-region (make-source-region saved-position scanner)
                         :expression (parse-result-value expression)))))

      ;; Parse block statement.
      ((equal #\{ (peek-next-token scanner))
       (let ((statements (parse-statement-list scanner)))
         (if (not (parse-result-match-p statements))
           (not-implemented "parse error in statement list"))
         (parse-result-match
           (make-instance 'ast-block-statement
                          :source-region (make-source-region saved-position scanner)
                          :statements (parse-result-value statements)))))

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
               :checks ((test-equal 1 (length (get-list (get-statements ast)))))))

(deftest test-parse-statement ()
  (test-parse-statement-return-without-value)
  (test-parse-statement-empty-block)
  (test-parse-statement-simple-block))

;; -----------------------------------------------------------------------------
;; Parse a list of statements surrounded by '{' and '}'.
(defun parse-statement-list (scanner)
  (parse-list parse-statement scanner :start-delimiter #\{ :end-delimiter #\}))

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
(defun parse-type-argument (scanner)
  (parse-type scanner))

;; -----------------------------------------------------------------------------
(defun parse-type-argument-list (scanner)
  (parse-list parse-type-argument scanner :start-delimiter #\{ :end-delimiter #\} :separator #\,))

;; -----------------------------------------------------------------------------
(defun parse-value-argument (scanner)
  (parse-expression scanner))

;; -----------------------------------------------------------------------------
(defun parse-value-argument-list (scanner)
  (parse-list parse-value-argument scanner :start-delimiter #\( :end-delimiter #\) :separator #\,))

;; -----------------------------------------------------------------------------
(defun parse-character (scanner)
  (if (is-at-end-p scanner)
    (parse-result-no-match)
    (let ((next-char (read-next-token scanner)))
      (if (equal next-char #\\)
        (let ((next-next-char (read-next-token scanner)))
          (parse-result-match
            (cond
              ((equal #\n next-next-char) #\linefeed)
              ((equal #\r next-next-char) #\return)
              ((equal #\t next-next-char) #\tab)
              (t next-next-char))))
        (parse-result-match next-char)))))

(deftest test-parse-character-simple ()
  (test-parser parse-character "c"
               :is-match-p t
               :expected-type 'character
               :checks ((test-equal #\c ast))))

(deftest test-parse-character-escape-sequences ()
  (test-parser parse-character "\\n"
               :is-match-p t
               :expected-type 'character
               :checks ((test-equal #\linefeed ast)))
  (test-parser parse-character "\\r"
               :is-match-p t
               :expected-type 'character
               :checks ((test-equal #\return ast)))
  (test-parser parse-character "\\t"
               :is-match-p t
               :expected-type 'character
               :checks ((test-equal #\tab ast)))
  (test-parser parse-character "\\\\"
               :is-match-p t
               :expected-type 'character
               :checks ((test-equal #\\ ast))))

(deftest test-parse-character ()
  (test-parse-character-simple)
  (test-parse-character-escape-sequences))

;; -----------------------------------------------------------------------------
(defun parse-literal (scanner)
  (if (is-at-end-p scanner)
      (parse-result-no-match)
      (let ((saved-position (get-position scanner))
            (next-char (read-next-token scanner)))
        (cond 

            ((equal next-char #\")
             (let ((buffer (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character)))
               (loop
                 (loop while (not (match-next-token scanner #\"))
                       do
                       (setf next-char (parse-character scanner))
                       (if (parse-result-no-match-p next-char)
                         (not-implemented "expecting character"))
                       (vector-push-extend (parse-result-value next-char) buffer))
                 (parse-whitespace scanner)
                 (if (not (match-next-token scanner #\"))
                   (return)))
               (parse-result-match
                 (make-instance 'ast-string-literal
                               :source-region (make-source-region saved-position scanner)
                               ;;////TODO: strings should go into a table
                               :value buffer))))

            ((equal next-char #\')
             (let ((char (parse-character scanner)))
               (if (parse-result-no-match-p char)
                 (not-implemented "expecting character"))
               (if (not (match-next-token scanner #\'))
                 (not-implemented "expecting '"))
               (parse-result-match
                 (make-instance 'ast-character-literal
                               :source-region (make-source-region saved-position scanner)
                               :value (parse-result-value char)))))

            ((digit-char-p next-char)
             (if (and (char-equal #\0 next-char)
                      (match-next-token scanner #\x))
               (not-implemented "hex literals"))
             (let ((value (digit-char-to-integer next-char)))
               (loop
                 (if (is-at-end-p scanner)
                   (return))
                 (setf next-char (peek-next-token scanner))
                 (if (not (digit-char-p next-char))
                   (return))
                 (read-next-token scanner)
                 (setf value (+ (digit-char-to-integer next-char) (* value 10))))
               (parse-result-match (make-instance 'ast-integer-literal
                                                 :source-region (make-source-region saved-position scanner)
                                                 :value value))))

            (t (not-implemented "literal"))))))

(deftest test-parse-literal-integer ()
  (test-parser parse-literal "12" :is-match-p t :expected-type 'ast-integer-literal
               :checks ((test-equal 12 (get-literal-value ast)))))

(deftest test-parse-literal-string-simple ()
  (test-parser parse-literal "\"foo\""
               :is-match-p t
               :expected-type 'ast-string-literal
               :checks ((test-equal "foo" (get-literal-value ast)))))

(deftest test-parse-multi-string-literal ()
  (test-parser parse-literal (format nil "\"foo\"~C\"bar\"" #\linefeed)
               :is-match-p t
               :expected-type 'ast-string-literal
               :checks ((test-equal "foobar" (get-literal-value ast)))))

(deftest test-parse-literal ()
  (test-parser parse-literal "" :is-match-p nil)
  (test-parse-literal-integer)
  (test-parse-literal-string-simple)
  (test-parse-multi-string-literal))

;; -----------------------------------------------------------------------------
(defun parse-expression (scanner)
  (if (is-at-end-p scanner)
      (return-from parse-expression (parse-result-no-match)))

  (let (expression
        (start-pos (get-position scanner)))

    ;; Parse prefix expression.
    ;;////TODO
  
    ;; Parse core expression.
    (let ((next-char (peek-next-token scanner)))
      (setf expression (cond

                         ;; Parse name expression.
                         ((or (alpha-char-p next-char)
                              (equal #\_ next-char))
                          (let ((id (parse-identifier scanner)))
                            (if (not (parse-result-match-p id))
                              (not-implemented "expecting name"))
                            (parse-result-match
                              (make-instance 'ast-name-expression
                                            :source-region (make-source-region start-pos scanner)
                                            :name (parse-result-value id)))))

                         ;; Parse literal.
                         ((or (digit-char-p next-char)
                              (char-equal #\" next-char)
                              (char-equal #\' next-char))
                          (parse-literal scanner))

                         (t
                          (parse-result-no-match)))))
  
    ;; Parse postfix.
    (loop
      named parse-postfix
      do (parse-whitespace scanner)
         (if (not (is-at-end-p scanner))
           (let ((next-char (peek-next-token scanner)))
             (cond

               ;; Parse call expression.
               ((or (equal #\( next-char)
                    (equal #\< next-char))
                (let ((type-arguments (if (equal #\< next-char)
                                        (progn
                                          (parse-type-argument-list scanner)
                                          (parse-whitespace scanner)
                                          (if (not (is-at-end-p scanner))
                                            (setf next-char (peek-next-token scanner))
                                            (setf next-char nil)))
                                        nil))
                      (value-arguments (if (equal #\( next-char)
                                         (parse-value-argument-list scanner)
                                         nil)))
                  (setf expression
                        (parse-result-value
                          (make-instance 'ast-call-expression
                                        :source-region (make-source-region start-pos scanner)
                                        :callee (parse-result-value expression)
                                        :type-arguments (parse-result-match-value-or type-arguments nil)
                                        :value-arguments (parse-result-match-value-or value-arguments nil))))))
               
               ;; Parse dot expression.
               ((equal #\. next-char)
                (read-next-token scanner)
                (parse-whitespace scanner)
                (let ((name (parse-identifier scanner)))
                  (if (not (parse-result-match-p name))
                    (not-implemented "expecting identifier after '.'"))
                  (setf expression (parse-result-value
                                     (make-instance 'ast-dot-expression
                                                   :source-region (make-source-region start-pos scanner)
                                                   :value (parse-result-value expression)
                                                   :member name)))))

               (t
                (return-from parse-postfix))))
           (return-from parse-postfix)))
    
    expression))

(deftest test-parse-expression-literal ()
  (test-parser parse-expression "512" :is-match-p t :expected-type 'ast-integer-literal
               :checks ((test-equal 512 (get-literal-value ast)))))

(deftest test-parse-expression-named ()
  (test-parser parse-expression "foo"
               :is-match-p t
               :expected-type 'ast-name-expression
               :checks ((test-equal "foo" (identifier-to-string (get-identifier ast))))))

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
                        (test-equal "bar" (symbol-name (get-name (get-member-name ast))))
                        (test-type 'ast-integer-literal (get-value-expression (get-value-expression ast))))))

(deftest test-parse-expression-call-simple ()
  (test-parser parse-expression "foo()"
               :is-match-p t
               :expected-type 'ast-call-expression
               :checks ((test-type 'ast-name-expression (get-callee-expression ast))
                        (test-same nil (get-type-arguments ast))
                        (test-equal 0 (length (get-list (get-value-arguments ast)))))))

(deftest test-parse-expression ()
  (test-parse-expression-literal)
  (test-parse-expression-dot-simple)
  (test-parse-expression-dot-multiple)
  (test-parse-expression-call-simple))

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
    (setf start-position (get-position scanner))

    ;; Parse attributes.
    (setf attributes (parse-attribute-list scanner))

    ;; Parse modifiers.
    (setf modifiers (parse-modifier-list scanner))

    ;;////FIXME: this needs to match keyword+whitespace, not just keyword
    ;; Parse definition kind.
    (parse-whitespace scanner)
    (setf definition-class (cond ((match-keyword scanner "method")
                                 'ast-method-definition)
                                ((match-keyword scanner "field")
                                 'ast-field-definition)
                                ((match-keyword scanner "type")
                                 'ast-type-definition)
                                ((match-keyword scanner "object")
                                 'ast-object-definition)
                                ((match-keyword scanner "local")
                                 'ast-variable-definition)
                                ((match-keyword scanner "function")
                                 'ast-function-definition)
                                ((match-keyword scanner "features")
                                 'ast-features-definition)
                                ((match-keyword scanner "module")
                                 'ast-module-definition)
                                ((match-keyword scanner "library")
                                 'ast-library-definition)
                                ((match-keyword scanner "program")
                                 'ast-program-definition)
                                ((match-keyword scanner "namespace")
                                 'ast-namespace-definition)
                                (t
                                 (return-from parse-definition (parse-result-no-match)))))

    ;; Parse name.
    (parse-whitespace scanner)
    (setf name (parse-identifier scanner))

    ;; Parse type parameters.
    (setf type-parameters (parse-type-parameter-list scanner))

    ;; Parse value parameters.
    (setf value-parameters (parse-value-parameter-list scanner))

    ;; Parse type.
    (parse-whitespace scanner)
    (if (match-next-token scanner #\:)
      (progn
        (parse-whitespace scanner)
        (setf type (parse-type scanner))))

    ;; Parse clauses.
    (setf clauses (parse-clause-list scanner))

    ;; Parse body/value.
    (parse-whitespace scanner)
    (cond ((match-next-token scanner #\;)
           t)
          ;;////TODO: if it's a type definition, we need to parse a type here when hitting '='
          ((match-next-token scanner #\=)
           (parse-whitespace scanner)
           (setf value (parse-expression scanner))
           (parse-whitespace scanner)
           (if (not (match-next-token scanner #\;))
             (not-implemented "parse error; expecting semicolon")))
          (t
           (setf body
                 ;;////FIXME: this should be unified into one single parsing path for both module and other definitions
                 (if (or (eq 'ast-module-definition definition-class)
                         (eq 'ast-program-definition definition-class)
                         (eq 'ast-library-definition definition-class)
                         (eq 'ast-namespace-definition definition-class)
                         (eq 'ast-features-definition definition-class))
                   (parse-definition-list scanner)
                   (parse-statement-list scanner)))))

    ;; Create AST node.
    (setf ast (make-instance definition-class
                             :source-region (make-source-region start-position scanner)
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
               :checks ((test-equal "Foobar" (identifier-to-string (get-identifier ast))))))

(deftest test-parse-definition-function-simple ()
  (test-parser parse-definition "function Foobar : () -> () {}"
               :is-match-p t
               :expected-type 'ast-function-definition
               :checks ((test-equal "Foobar" (identifier-to-string (get-identifier ast)))
                        (test-equal nil (get-value-parameters ast))
                        (test-equal nil (get-type-parameters ast)))))

(deftest test-parse-definition-method-with-arguments ()
  (test-parser parse-definition "method Foobar( Foo : Integer ) {}"
               :is-match-p t
               :expected-type 'ast-method-definition
               :checks ((test-equal "Foobar" (identifier-to-string (get-identifier ast)))
                        (test-equal 1 (length (get-list (get-value-parameters ast)))))))

(deftest test-parse-definition-module-simple ()
  (test-parser parse-definition "module Foobar {}"
               :is-match-p t
               :expected-type 'ast-module-definition
               :checks ((test-equal "Foobar" (identifier-to-string (get-identifier ast))))))

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
  (parse-list parse-definition scanner :start-delimiter #\{ :end-delimiter #\}))

;; -----------------------------------------------------------------------------
(defun parse-compilation-unit (scanner)
  (let* ((start-position (get-position scanner))
         (definitions (parse-list parse-definition scanner))
         (result (parse-result-match
                   (make-instance 'ast-compilation-unit
                                 :source-region (make-source-region start-position scanner)
                                 :definitions (if (parse-result-no-match-p definitions)
                                                (make-instance 'ast-list
                                                              :source-region (make-source-region start-position start-position))
                                                (parse-result-value definitions))))))
    (if (not (is-at-end-p scanner))
        (not-implemented "parse error; unrecognized input"))
    result))

(deftest test-parse-compilation-unit-empty ()
  (test-parser parse-compilation-unit "" :is-match-p t :expected-type 'ast-compilation-unit
               :checks
               ((test-type 'ast-list (get-definitions ast))
                (test-equal nil (get-list (get-definitions ast))))))

(deftest test-parse-compilation-unit-simple ()
  (test-parser parse-compilation-unit "type Foobar; type Barfoo;" :is-match-p t :expected-type 'ast-compilation-unit
               :checks
               ((test-type 'ast-list (get-definitions ast))
                (test-equal 2 (length (get-list (get-definitions ast)))))))

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
  (test-parse-attribute)
  (test-parse-character)
  (test-parse-literal)
  (test-parse-type)
  (test-parse-expression)
  (test-parse-statement)
  (test-parse-statement-list)
  (test-parse-value-parameter)
  (test-parse-definition)
  (test-parse-compilation-unit))

