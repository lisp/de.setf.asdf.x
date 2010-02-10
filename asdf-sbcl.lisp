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


(in-package :asdf.i)

;;;; -----------------------------------------------------------------
;;;; SBCL hook into REQUIRE
;;;;
#+sbcl
(progn
  (defun module-provide-asdf (name)
    (handler-bind ((style-warning #'muffle-warning))
      (let* ((*verbose-out* (make-broadcast-stream))
             (system (asdf:find-system name nil)))
        (when system
          (asdf:operate 'asdf:load-op name)
          t))))

  (defun contrib-sysdef-search (system)
    (let ((home (getenv "SBCL_HOME")))
      (when (and home (not (string= home "")))
        (let* ((name (coerce-name system))
               (home (truename home))
               (contrib (merge-pathnames
                         (make-pathname :directory `(:relative ,name)
                                        :name name
                                        :type "asd"
                                        :case :local
                                        :version :newest)
                         home)))
          (probe-file contrib)))))

  (pushnew
   '(let ((home (getenv "SBCL_HOME")))
      (when (and home (not (string= home "")))
        (merge-pathnames "site-systems/" (truename home))))
   *central-registry*)

  (pushnew
   '(merge-pathnames ".sbcl/systems/"
     (user-homedir-pathname))
   *central-registry*)

  (pushnew 'module-provide-asdf sb-ext:*module-provider-functions*)
  (pushnew 'contrib-sysdef-search *system-definition-search-functions*))

