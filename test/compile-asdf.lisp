(in-package #:common-lisp-user)

(load "test/script-support.lisp")

(cond ((probe-file "asdf.lisp")
       (multiple-value-bind (result warnings-p errors-p)
           (compile-file "asdf.lisp")
         (declare (ignore result))
         (cond (warnings-p 
                (leave-lisp "Testuite failed: ASDF compiled with warnings" 1))
               (errors-p 
                (leave-lisp "Testuite failed: ASDF compiled with ERRORS" 2))
               (t
                (leave-lisp "ASDF compiled cleanly" 0)))))
      (t
       (leave-lisp "Testsuite failed: unable to find ASDF source" 3)))
       
                     