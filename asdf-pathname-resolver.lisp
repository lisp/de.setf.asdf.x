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

(in-package #:asdf.i)

(eval-when (:compile-top-level :load-top-level :execute)
  (export '(asdf.x::*source-to-target-mappings*
            asdf.x::*default-toplevel-directory*
            asdf.x::*centralize-lisp-binaries*
            asdf.x::*include-per-user-information*
            asdf.x::*map-all-source-files*
            asdf.x::output-files-for-system-and-operation
            asdf.x::*enable-asdf-binary-locations*
            asdf.x::implementation-specific-directory-name)
          :asdf.x))

;;; ---------------------------------------------------------------------------
;;; asdf-binary-locations
;;;
;;; this bit of code was stolen from Bjorn Lindberg and then it grew!
;;; see http://www.cliki.net/asdf%20binary%20locations
;;; and http://groups.google.com/group/comp.lang.lisp/msg/bd5ea9d2008ab9fd
;;; ---------------------------------------------------------------------------
;;; Portions of this code were once from SWANK / SLIME

(defparameter *centralize-lisp-binaries* nil
  "If true, compiled lisp files without an explicit mapping (see
\\*source-to-target-mappings\\*) will be placed in subdirectories of
\\*default-toplevel-directory\\*. If false, then compiled lisp files
without an explicitly mapping will be placed in subdirectories of
their sources.")

(defparameter *enable-asdf-binary-locations* nil
  "
If true, then compiled lisp files will be placed into a directory
computed from the Lisp version, Operating System and computer archetecture.
See [implementation-specific-directory-name][] for details.")


(defparameter *default-toplevel-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative ".cache" "common-lisp"))
   (truename (user-homedir-pathname)))
  "If \\*centralize-lisp-binaries\\* is true, then compiled lisp files without an explicit mapping \(see \\*source-to-target-mappings\\*\) will be placed in subdirectories of \\*default-toplevel-directory\\*.")

(defparameter *include-per-user-information*
  nil
  "When \\*centralize-lisp-binaries\\* is true this variable controls whether or not to customize the output directory based on the current user. It can be nil, t or a string. If it is nil \(the default\), then no additional information will be added to the output directory. If it is t, then the user's name \(as taken from the return value of #'user-homedir-pathname\) will be included into the centralized path (just before the lisp-implementation directory). Finally, if \\*include-per-user-information\\* is a string, then this string will be included in the output-directory.")

(defparameter *map-all-source-files*
  nil
  "If true, then all subclasses of source-file will have their output locations mapped by ASDF-Binary-Locations. If nil (the default), then only subclasses of cl-source-file will be mapped.")

(defvar *source-to-target-mappings*
  #-sbcl
  nil
  #+sbcl
  (list (list (princ-to-string (getenv "SBCL_HOME")) nil))
  "The \\*source-to-target-mappings\\* variable specifies mappings from source to target. If the target is nil, then it means to not map the source to anything. I.e., to leave it as is. This has the effect of turning off ASDF-Binary-Locations for the given source directory. Examples:

    ;; compile everything in .../src and below into .../cmucl
    '((\"/nfs/home/compbio/d95-bli/share/common-lisp/src/\"
       \"/nfs/home/compbio/d95-bli/lib/common-lisp/cmucl/\"))

    ;; leave SBCL innards alone (SBCL specific)
    (list (list (princ-to-string (sb-ext:posix-getenv \"SBCL_HOME\")) nil))
")

(defparameter *implementation-features*
  '(:allegro :lispworks :mcl :sbcl :ccl :openmcl :cmu :clisp
    :corman :cormanlisp :armedbear :gcl :ecl :scl))

(defparameter *os-features*
  '(:windows :mswindows :win32 :mingw32
    :solaris :sunos
    :macosx :darwin :apple
    :freebsd :netbsd :openbsd :bsd
    :linux :unix))

(defparameter *architecture-features*
  '(:amd64 (:x86-64 :x86_64 :x8664-target) :i686 :i586 :pentium3
    :i486 (:i386 :pc386 :iapx386) (:x86 :x8632-target) :pentium4
    :hppa64 :hppa :ppc64 :ppc32 :powerpc :ppc :sparc64 :sparc))

;; note to gwking: this is in slime, system-check, and system-check-server too
(defun lisp-version-string ()
  #+cmu       (substitute #\- #\/
                          (substitute #\_ #\Space
                                      (lisp-implementation-version)))
  #+mcl       (substitute #\_ #\Space (lisp-implementation-version))
  #+scl       (lisp-implementation-version)
  #+sbcl      (lisp-implementation-version)
  #+ecl       (reduce (lambda (x str) (substitute #\_ str x))
                      '(#\Space #\: #\( #\))
                      :initial-value (lisp-implementation-version))
  #+gcl       (let ((s (lisp-implementation-version))) (subseq s 4))
  #+openmcl   (format nil "~d.~d~@[-~d~]"
                      ccl::*openmcl-major-version*
                      ccl::*openmcl-minor-version*
                      #+ppc64-target 64
                      #-ppc64-target nil)
  #+lispworks (format nil "~A~@[~A~]"
                      (lisp-implementation-version)
                      (when (member :lispworks-64bit *features*) "-64bit"))
  #+allegro   (format nil
                      "~A~A~A~A"
                      excl::*common-lisp-version-number*
                                        ; ANSI vs MoDeRn
                      ;; thanks to Robert Goldman and Charley Cox for
                      ;; an improvement to my hack
                      (if (eq excl:*current-case-mode*
                              :case-sensitive-lower) "M" "A")
                      ;; Note if not using International ACL
                      ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
                      (excl:ics-target-case
                        (:-ics "8")
                        (:+ics ""))
                      (if (member :64bit *features*) "-64bit" ""))
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+cormanlisp (lisp-implementation-version)
  #+digitool   (subseq (lisp-implementation-version) 8))


(defparameter *implementation-specific-directory-name* nil)

;;;; -------------------------------------------------------------------------
;;;; pathname mapping interface

(defgeneric output-files-using-mappings (source possible-paths path-mappings)
  (:documentation
"Use the variable \\*source-to-target-mappings\\* to find
an output path for the source. The algorithm transforms each
entry in possible-paths as follows: If there is a mapping
whose source starts with the path of possible-path, then
replace possible-path with a pathname that starts with the
target of the mapping and continues with the rest of
possible-path. If no such mapping is found, then use the
default mapping.

If \\*centralize-lisp-binaries\\* is false, then the default
mapping is to place the output in a subdirectory of the
source. The subdirectory is named using the Lisp
implementation \(see
implementation-specific-directory-name\). If
\\*centralize-lisp-binaries\\* is true, then the default
mapping is to place the output in subdirectories of
\\*default-toplevel-directory\\* where the subdirectory
structure will mirror that of the source."))



(defun implementation-specific-directory-name ()
  "Return a name that can be used as a directory name that is
unique to a Lisp implementation, Lisp implementation version,
operating system, and hardware architecture."
  (and *enable-asdf-binary-locations*
       (list
        (or *implementation-specific-directory-name*
            (setf *implementation-specific-directory-name*
                  (labels
                      ((fp (thing)
                         (etypecase thing
                           (symbol
                            (let ((feature (find thing *features*)))
                              (when feature (return-from fp feature))))
                           ;; allows features to be lists of which the first
                           ;; member is the "main name", the rest being aliases
                           (cons
                            (dolist (subf thing)
                              (let ((feature (find subf *features*)))
                                (when feature (return-from fp (first thing))))))))
                       (first-of (features)
                         (loop :for f :in features
                           :when (fp f) :return :it))
                       (maybe-warn (value fstring &rest args)
                         (cond (value)
                               (t (apply #'warn fstring args)
                                  "unknown"))))
                    (let ((lisp (maybe-warn (first-of *implementation-features*)
                                            "No implementation feature found in ~a."
                                            *implementation-features*))
                          (os   (maybe-warn (first-of *os-features*)
                                            "No os feature found in ~a." *os-features*))
                          (arch (maybe-warn (first-of *architecture-features*)
                                            "No architecture feature found in ~a."
                                            *architecture-features*))
                          (version (maybe-warn (lisp-version-string)
                                               "Don't know how to get Lisp ~
                                          implementation version.")))
                      (format nil "~(~@{~a~^-~}~)" lisp version os arch))))))))

(defun pathname-prefix-p (prefix pathname)
  (let ((prefix-ns (namestring prefix))
        (pathname-ns (namestring pathname)))
    (= (length prefix-ns)
       (mismatch prefix-ns pathname-ns))))

(defgeneric output-files-for-system-and-operation
  (system operation component source possible-paths)
  (:documentation "Returns the directory where the componets output files should be placed. This may depends on the system, the operation and the component. The ASDF default input and outputs are provided in the source and possible-paths parameters."))

(defun source-to-target-resolved-mappings ()
  "Answer `*source-to-target-mappings*` with additional entries made
by resolving sources that are symlinks.

As ASDF sometimes resolves symlinks to compute source paths, we must
follow that.  For example, if SBCL is installed under a symlink, and
SBCL_HOME is set through that symlink, the default rule above
preventing SBCL contribs from being mapped elsewhere will not be
applied by the plain `*source-to-target-mappings*`."
  (loop :for mapping :in asdf:*source-to-target-mappings*
    :for (source target) = mapping
    :for true-source = (and source (resolve-symlinks source))
    :if (equal source true-source) :collect mapping
    :else :append (list mapping (list true-source target))))

(defmethod output-files-for-system-and-operation
           ((system system) operation component source possible-paths)
  (declare (ignore operation component))
  (output-files-using-mappings
   source possible-paths (source-to-target-resolved-mappings)))

(defmethod output-files-using-mappings (source possible-paths path-mappings)
  (mapcar
   (lambda (path)
     (loop :for (from to) :in path-mappings
       :when (pathname-prefix-p from source)
       :return
       (if to
           (merge-pathnames
            (make-pathname :type (pathname-type path))
            (merge-pathnames (enough-namestring source from)
                             to))
           path)
       :finally
       (return
         ;; Instead of just returning the path when we
         ;; don't find a mapping, we stick stuff into
         ;; the appropriate binary directory based on
         ;; the implementation
         (if *centralize-lisp-binaries*
             (merge-pathnames
              (make-pathname
               :type (pathname-type path)
               :directory `(:relative
                            ,@(cond ((eq *include-per-user-information* t)
                                     (cdr (pathname-directory
                                           (user-homedir-pathname))))
                                    ((not (null *include-per-user-information*))
                                     (list *include-per-user-information*)))
                            ,@(implementation-specific-directory-name)
                            ,@(rest (pathname-directory path)))
               :defaults path)
              *default-toplevel-directory*)
             (make-pathname
              :type (pathname-type path)
              :directory (append
                          (pathname-directory path)
                          (implementation-specific-directory-name))
              :defaults path)))))
   possible-paths))

(defmethod output-files
    :around ((operation compile-op) (component source-file))
  (if (or *map-all-source-files*
            (typecase component
              (cl-source-file t)
              (t nil)))
    (let ((source (component-pathname component))
          (paths (call-next-method)))
      (output-files-for-system-and-operation
       (component-system component) operation component source paths))
    (call-next-method)))
