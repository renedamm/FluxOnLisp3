
(in-package :fl)

;;;;============================================================================
;;;;    File Utilities.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

;; -----------------------------------------------------------------------------
(defun directory-pathname-p (pathname)
  (and
   (not (component-present-p (pathname-name pathname)))
   (not (component-present-p (pathname-type pathname)))
   pathname))

;; -----------------------------------------------------------------------------
(defun pathname-as-directory (namestring)
  (let ((pathname (pathname namestring)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p namestring))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
        pathname)))

;; -----------------------------------------------------------------------------
(defun pathname-as-file (namestring)
  (let ((pathname (pathname namestring)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p namestring)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory)
           :name (pathname-name name-and-type)
           :type (pathname-type name-and-type)
           :defaults pathname))
        pathname)))

;; -----------------------------------------------------------------------------
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

;; -----------------------------------------------------------------------------
#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

;; -----------------------------------------------------------------------------
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    
    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-as-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks allow clisp openmcl)
    (error "LIST-DIRECTORY not implemented for current platform")))

;; -----------------------------------------------------------------------------
(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))
  #-(or sbcl lispworks openmcl allegro cmu clisp)
  (error "FILE-EXISTS-P not implemented for current platform"))

;; -----------------------------------------------------------------------------
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

;; -----------------------------------------------------------------------------
(defun read-string (stream-or-pathname)
  (if (not (streamp stream-or-pathname))
      (with-open-file (file stream-or-pathname)
        (read-string file))
      (let* ((length-remaining (- (file-length stream-or-pathname)
                                  (file-position stream-or-pathname)))
             (data (make-string length-remaining)))
        (read-sequence data stream-or-pathname)
        data)))

;;;;============================================================================
;;;;    Miscellaneous Utilities.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defun not-implemented (&optional message)
  (if message
      (error (format nil "Not implemented: ~a" message))
      (error "Not implemented!")))

;; -----------------------------------------------------------------------------
(defun insert-into-array (vector value position)
  (assert (<= position (length vector)))
  (if (equal position (length vector))
      (vector-push-extend value vector)
      (progn
        (replace vector vector :start2 position :start1 (1+ position)
                 :end2 (vector-push-extend value vector))
        (setf (aref vector position) value)))
  vector)

(deftest test-insert-into-array ()
  (let ((vector (make-array 10 :adjustable t :fill-pointer 0)))
    (insert-into-array vector 10 0)
    (test (equal (length vector) 1))))

;; -----------------------------------------------------------------------------
(defun whitespace-char-p (char)
  (or (equal char #\Return)
      (equal char #\Tab)
      (equal char #\Newline)
      (equal char #\Space)))

;; -----------------------------------------------------------------------------
(defun digit-char-to-integer (char)
  (cond ((and (char>= char #\0)
              (char<= char #\9))
         (- (char-code char) (char-code #\0)))
        ((and (char>= char #\a)
              (char<= char #\f))
         (- (char-code char) (char-code #\a)))
        ((and (char>= char #\A)
              (char<= char #\F))
         (- (char-code char) (char-code #\A)))
        (t (error (format "Character ~@C is not a valid digit character" char)))))

(deftest test-digit-char-to-integer ()
  (test-equal 0 (digit-char-to-integer #\0))
  (test-equal 9 (digit-char-to-integer #\9))
  (test-equal 10 (digit-char-to-integer #\A))
  (test-equal 11 (digit-char-to-integer #\b))
  (test-equal 15 (digit-char-to-integer #\f)))

;; -----------------------------------------------------------------------------
(defmacro lazy-initialize-slot (instance slot-name &body init-form)
  (with-gensyms (instance-value slot-name-value slot-value)
    `(let* ((,instance-value ,instance)
            (,slot-name-value ,slot-name)
            (,slot-value (slot-value ,instance-value ,slot-name-value)))
       (if (not ,slot-value)
         (setf ,slot-value
               (setf (slot-value ,instance-value ,slot-name-value)
                     (progn
                       ,@init-form))))
       ,slot-value)))
     
;; -----------------------------------------------------------------------------
(defsuite test-utilities ()
  (test-insert-into-array))

