;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: asdf.i; -*-

;;; This is asdf: Another System Definition Facility.
;;; hash - $Format:%H$
;;;
;;; Local Variables:
;;; mode: lisp
;;; End:
;;;
;;; Feedback, bug reports, and patches are all welcome: please mail to
;;; <asdf-devel@common-lisp.net>.  But note first that the canonical
;;; source for asdf is presently on common-lisp.net at
;;; <URL:http://common-lisp.net/project/asdf/>
;;;
;;; If you obtained this copy from anywhere else, and you experience
;;; trouble using it, or find bugs, you may want to check at the
;;; location above for a more recent version (and for documentation
;;; and test files, if your copy came without them) before reporting
;;; bugs.  There are usually two "supported" revisions - the git HEAD
;;; is the latest development version, whereas the revision tagged
;;; RELEASE may be slightly older but is considered `stable'

;;; -- LICENSE START
;;; (This is the MIT / X Consortium license as taken from
;;;  http://www.opensource.org/licenses/mit-license.html on or about
;;;  Monday; July 13, 2009)
;;;
;;; Copyright (c) 2001-2010 Daniel Barlow and contributors
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; -- LICENSE END

;;; the problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file

#+xcvb (module ())

(eval-when (:compile-top-level :load-top-level :execute)
  (export '(asdf.x::initialize-source-registry
            asdf.x::clear-source-registry
            asdf.x::ensure-source-registry
            asdf.x::process-source-registry)))

(in-package :asdf.i)

;;;; -----------------------------------------------------------------
;;;; Source Registry Configuration, by Francois-Rene Rideau
;;;; See README.source-registry and https://bugs.launchpad.net/asdf/+bug/485918

(pushnew 'sysdef-source-registry-search *system-definition-search-functions*)

;; Using ack 1.2 exclusions
(defvar *default-exclusions*
  '(".bzr" ".cdv" "~.dep" "~.dot" "~.nib" "~.plst"
    ".git" ".hg" ".pc" ".svn" "CVS" "RCS" "SCCS" "_darcs"
    "_sgbak" "autom4te.cache" "cover_db" "_build"))

(defun default-registry ()
  ())

(defvar *source-registry* ()
  "Either NIL (for uninitialized), or a list of one element,
said element itself being a list of directory pathnames where to look for .asd files")

(defun source-registry ()
  (car *source-registry*))

(defun (setf source-registry) (x)
  (setf *source-registry* (list x)))

(defun source-registry-initialized-p ()
  (and *source-registry* t))

(defun clear-source-registry ()
  "Undoes any initialization of the source registry.
You might want to call that before you dump an image that would be resumed
with a different configuration, so the configuration would be re-read then."
  (setf *source-registry* '())
  (values))

(defun sysdef-source-registry-search (system)
  (ensure-source-registry)
  (let ((name (coerce-name system)))
    (block nil
      (dolist (dir (source-registry))
        (let ((defaults (eval dir)))
          (when defaults
            (cond ((directory-pathname-p defaults)
                   (let ((file (and defaults
                                    (make-pathname
                                     :defaults defaults :version :newest
                                     :name name :type "asd" :case :local)))
                         #+(and (or win32 windows) (not :clisp))
                         (shortcut (make-pathname
                                    :defaults defaults :version :newest
                                    :name name :type "asd.lnk" :case :local)))
                     (when (and file (probe-file file))
                       (return file))
                     #+(and (or win32 windows) (not :clisp))
                     (when (probe-file shortcut)
                       (let ((target (parse-windows-shortcut shortcut)))
                         (when target
                           (return (pathname target))))))))))))))

(defun read-file-forms (file)
  (with-open-file (in file)
    (loop :with eof = (list nil)
     :for form = (read in nil eof)
     :until (eq form eof)
     :collect form)))

(defun validate-source-registry-directive (directive)
  (unless
   (destructuring-bind (kw &rest rest) directive
     (case kw
       ((:include :directory :tree)
        (and (length=n-p rest 1)
             (typep (car rest) '(or pathname string))))
       ((:exclude)
        (every #'stringp rest))
       ((:default-registry :inherit-configuration :ignore-inherited-configuration)
        (null rest))))
   (error "Invalid directive ~S~%" directive))
  directive)

(defun validate-source-registry-form (form)
  (unless (and (consp form) (eq (car form) :source-registry))
    (error "Error: Form is not a source registry ~S~%" form))
  (loop :with inherit = 0
        :for directive :in (cdr form) :do
        (unless (consp directive)
          (error "invalid directive ~S" directive))
        (when (member (car directive)
                      '(:inherit-configuration :ignore-inherited-configuration))
          (incf inherit))
        (validate-source-registry-directive directive)
        :finally
        (unless (= inherit 1)
          (error "One and only one of :inherit-configuration or :ignore-inherited-configuration is required")))
  form)

(defun validate-source-registry-file (file)
  (let ((forms (read-file-forms file)))
    (unless (length=n-p forms 1)
      (error "One and only one form allowed for source registry. Got: ~S~%" forms))
    (validate-source-registry-form (car forms))))

(defun validate-source-registry-directory (directory)
  (let ((files (sort (ignore-errors
                       (directory (merge-pathnames
                                   (make-pathname :name :wild :type :wild)
                                   directory)
                                  #+sbcl :resolve-symlinks #+sbcl nil))
                     #'string< :key #'namestring)))
    `(:source-registry
      ,@(loop :for file :in files :append
          (mapcar #'validate-source-registry-directive (read-file-forms file)))
      (:inherit-configuration))))

(defun parse-source-registry-string (string)
  (cond
    ((or (null string) (equal string ""))
     '(:source-registry (:inherit-configuration)))
    ((not (stringp string))
     (error "environment string isn't: ~S" string))
    ((eql (char string 0) #\()
     (validate-source-registry-form (read-from-string string)))
    (t
     (loop
      :with inherit = nil
      :with directives = ()
      :with start = 0
      :with end = (length string)
      :for i = (or (position #\: string :start start) end) :do
      (let ((s (subseq string start i)))
        (cond
         ((equal "" s) ; empty element: inherit
          (when inherit
            (error "only one inherited configuration allowed: ~S" string))
          (setf inherit t)
          (push '(:inherit-configuration) directives))
         ((ends-with s "//")
          (push `(:tree ,(subseq s 0 (1- (length s)))) directives))
         (t
          (push `(:directory ,s) directives)))
         (setf start (1+ i))
         (when (>= start end)
           (unless inherit
             (push '(:ignore-inherited-configuration) directives))
           (return `(:source-registry ,@(nreverse directives)))))))))

(defun collect-asd-subdirectories (directory &key (exclude *default-exclusions*) collect)
  (let* ((files (ignore-errors
                 #+sbcl
                 (directory (merge-pathnames #P"**/*.asd" directory) :resolve-symlinks nil)
                 #+clisp
                 (directory (merge-pathnames #P"**/*.asd" directory) :circle t)
                 #-(or sbcl clisp)
                 (directory (merge-pathnames #P"**/*.asd" directory))))
         (dirs (remove-duplicates (mapcar #'pathname-sans-name+type files) :test #'equal)))
    (loop
     :for dir :in dirs
     :unless (loop :for x :in exclude
                   :thereis (find x (pathname-directory dir) :test #'equal))
     :do (funcall collect dir))))

(defparameter *default-source-registries*
  '(process-environment-source-registry
    process-user-source-registry
    process-user-source-registry-directory
    process-system-source-registry
    process-system-source-registry-directory
    process-default-source-registry))

(defun user-configuration-pathname ()
  (merge-pathnames ".config/" (user-homedir-pathname)))
(defun system-configuration-pathname ()
  #p"/etc/")
(defun source-registry-under (directory)
  (merge-pathnames "common-lisp/source-registry.conf" directory))
(defun user-source-registry-pathname ()
  (source-registry-under (user-configuration-pathname)))
(defun system-source-registry-pathname ()
  (source-registry-under (system-configuration-pathname)))
(defun source-registry-directory-under (directory)
  (merge-pathnames "common-lisp/source-registry.conf.d/" directory))
(defun user-source-registry-directory-pathname ()
  (source-registry-directory-under (user-configuration-pathname)))
(defun system-source-registry-directory-pathname ()
  (source-registry-directory-under (system-configuration-pathname)))

(defun process-environment-source-registry (&key inherit collect)
  (process-source-registry (getenv "CL_SOURCE_REGISTRY")
                           :inherit inherit :collect collect))
(defun process-user-source-registry (&key inherit collect)
  (process-source-registry (user-source-registry-pathname)
                           :inherit inherit :collect collect))
(defun process-user-source-registry-directory (&key inherit collect)
  (process-source-registry (user-source-registry-directory-pathname)
                           :inherit inherit :collect collect))
(defun process-system-source-registry (&key inherit collect)
  (process-source-registry (system-source-registry-pathname)
                           :inherit inherit :collect collect))
(defun process-system-source-registry-directory (&key inherit collect)
  (process-source-registry (system-source-registry-directory-pathname)
                           :inherit inherit :collect collect))
(defun process-default-source-registry (&key inherit collect)
  (declare (ignore inherit collect))
  nil)

(defgeneric process-source-registry (spec &key inherit collect))
(defmethod process-source-registry ((pathname pathname) &key
                                    (inherit *default-source-registries*)
                                    collect)
  (cond
    ((directory-pathname-p pathname)
     (process-source-registry (validate-source-registry-directory pathname)
                              :inherit inherit :collect collect))
    ((probe-file pathname)
     (process-source-registry (validate-source-registry-file pathname)
                              :inherit inherit :collect collect))
    (t
     (inherit-source-registry inherit :collect collect))))
(defmethod process-source-registry ((string string) &key
                                    (inherit *default-source-registries*)
                                    collect)
  (process-source-registry (parse-source-registry-string string)
                           :inherit inherit :collect collect))
(defmethod process-source-registry ((x null) &key
                                    (inherit *default-source-registries*)
                                    collect)
  (inherit-source-registry inherit :collect collect))

(defun make-collector ()
  (let ((acc ()))
    (values (lambda (x) (push x acc))
            (lambda () (reverse acc)))))

(defmethod process-source-registry ((form cons) &key
                                    (inherit *default-source-registries*)
                                    collect)
  (multiple-value-bind (collect result)
      (if collect
          (values collect (constantly nil))
        (make-collector))
    (let ((*default-exclusions* *default-exclusions*))
      (dolist (directive (cdr (validate-source-registry-form form)))
        (process-source-registry-directive directive :inherit inherit :collect collect)))
    (funcall result)))

(defun inherit-source-registry (inherit &key collect)
  (when inherit
    (funcall (first inherit) :collect collect :inherit (rest inherit))))

(defun process-source-registry-directive (directive &key inherit collect)
  (destructuring-bind (kw &rest rest) directive
    (ecase kw
      ((:include)
       (destructuring-bind (pathname) rest
         (process-source-registry (pathname pathname) :inherit inherit :collect collect)))
      ((:directory)
       (destructuring-bind (pathname) rest
         (funcall collect (ensure-directory-pathname pathname))))
      ((:tree)
       (destructuring-bind (pathname) rest
         (collect-asd-subdirectories pathname :collect collect)))
      ((:exclude)
       (setf *default-exclusions* rest))
      ((:default-registry)
       (default-registry))
      ((:inherit-configuration)
       (inherit-source-registry inherit :collect collect))
      ((:ignore-inherited-configuration)
       nil))))

;; Will read the configuration and initialize all internal variables,
;; and return the new configuration.
(defun initialize-source-registry ()
  (setf (source-registry)
        (inherit-source-registry *default-source-registries*)))

;; checks an initial variable to see whether the state is initialized
;; or cleared. In the former case, return current configuration; in
;; the latter, initialize.  ASDF will call this function at the start
;; of (asdf:find-system).
(defun ensure-source-registry ()
  (if (source-registry-initialized-p)
      (source-registry)
      (initialize-source-registry)))
