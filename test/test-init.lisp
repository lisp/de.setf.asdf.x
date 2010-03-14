;;;
;;; 2010-03-08  janderson
;;;
;;; init file for asdf testing on aws
;;; see asdf-pathname-test.sh

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(setq *load-verbose* (setq *compile-verbose* nil))

#+sbcl
(declaim (sb-ext:muffle-conditions warning))

#+cmucl
(declaim (optimize (ext:inhibit-warnings 3)))

(defparameter *binary-path*
  (make-pathname :directory `(:absolute "ebs" "test" "bin"
                              #+abcl "abcl"
                              #+allegro "alisp"
                              #+ccl "ccl"
                              #+clisp "clisp"
                              #+cmucl "cmucl"
                              #+ecl "ecl"
                              #+lispworks "lw"
                              #+sbcl "sbcl")
                 :name "asdf" :type (pathname-type (compile-file-pathname "test.lisp"))))

(ensure-directories-exist *binary-path*)
(unless (and (probe-file *binary-path*)
             (> (file-write-date *binary-path*) (file-write-date #p"/ebs/test/asdf/asdf.lisp")))
  (print (compile-file #p"/ebs/test/asdf/asdf.lisp" :output-file *binary-path*)))
(load *binary-path*)

