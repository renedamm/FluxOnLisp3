
(in-package :fl)

;;;;============================================================================
;;;;    Globals.
;;;;============================================================================

;; -----------------------------------------------------------------------------
(defparameter *standard-libraries* nil)

;;;;============================================================================
;;;;    Functions.
;;;;============================================================================

;; -----------------------------------------------------------------------------
;; Read, parse, and rewrite the sources in the libraries folder, if we haven't
;; done so already. Then add all their units to *libraries*, *programs*, and
;; *modules*.
(defun load-standard-libraries ()
  (if (not *standard-libraries*)
    (flet ((load-file (pathname)
             (if (find (pathname-type pathname) *config-source-file-extensions* :test #'equal)
               (progn
                 (print (format nil "Loading ~a" pathname))
                 (let* ((source (make-source pathname)) ;;////TODO: need to save this somewhere
                        (scanner (make-string-scanner (get-text source)))
                        (ast
                         (with-new-parser-state ()
                           (parse-compilation-unit scanner))))
                    (setf *standard-libraries* (cons (rewrite ast) *standard-libraries*)))))))

      (walk-directory *config-library-directory* #'load-file :directories nil))))

;; -----------------------------------------------------------------------------
(defun unload-standard-libraries ()
  (setf *standard-libraries* nil))


