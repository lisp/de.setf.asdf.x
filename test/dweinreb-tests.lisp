;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: asdf-user; -*-
;;; By Dan Weinreb, Oct 1, 2006.  Public domain, entirely free, etc.
#|
Daniel Weinreb
dlw@alum.mit.edu
http://danweinreb.org/blog/
http://ilc2009.scheming.org/
|#

(in-package :asdf-user)

(unless (fboundp 'run-unit-test) (load "dweinreb-utilities.lisp"))

;;; (let ((*operation-force* t)) (run-all-unit-tests))

(asdf:defsystem system-1
 :components ((:test-file "a")
               (:test-file "b")
               (:test-file "c" :depends-on ("b"))
               (:test-file "d")))


(define-test test-1 system-1
 :operation-name asdf:load-op
 :already-compiled ("b")
 :expected ((:compiled "a")
             (:loaded "a" :after-compiling "a")
             (:did-not-compile "b")
             (:loaded "b")
             (:compiled "c" :after-loading "b")
             (:loaded "c" :after-loading "b" :after-compiling "c")
             (:compiled "d")
             (:loaded "d" :after-compiling "d")))

(asdf:defsystem system-2
 :components ((:test-file "a" :depends-on ("g" "k"))
               (:test-file "b")
               (:test-file "c" :depends-on ("b"))
               (:test-file "d" :depends-on ("e"))
               (:test-file "e")
               (:test-file "f" :depends-on ("c"))
               (:test-file "g" :depends-on ("h"))
               (:test-file "h")
               (:test-file "i" :depends-on ("f" "b"))
               (:test-file "j" :depends-on ("f" "c"))
               (:test-file "k")
               (:test-file "l" :depends-on ("d"))))

(define-test test-2 system-2
 :operation-name asdf:load-op
 :already-compiled ()
 :expected ((:compiled "a" :after-compiling "g" :after-compiling "k"
                        :after-compiling "h" :after-loading "g" 
                        :after-loading "k" :after-loading "h")
             (:loaded "a" :after-compiling "a")
             (:compiled "b")
             (:loaded "b" :after-compiling "b")
             (:compiled "c" :after-compiling "b" :after-loading "b")
             (:loaded "c"   :after-compiling "b" :after-loading "b"
                      :after-compiling "c")
             (:compiled "d" :after-compiling "e")
             (:compiled "e")
             (:compiled "f" :after-compiling "c" :after-compiling "b")
             (:compiled "g" :after-compiling "h")
             (:compiled "h")
             (:compiled "i" :after-compiling "f" :after-compiling "b")
             (:compiled "j" :after-compiling "f" :after-compiling "b"
                        :after-compiling "c")
             (:compiled "i" :after-compiling "f" :after-compiling "b"
                        :after-compiling "c")
             (:compiled "j" :after-compiling "f" :after-compiling "b"
                        :after-compiling "c")
             (:compiled "k")
             (:compiled "l" :after-compiling "d" :after-compiling "e")))

(asdf:defsystem system-3
 :components ((:test-file "a")
               (:test-file "b" :depends-on ("a"))))

(define-test test-3 system-3
 :operation-name asdf:compile-op
 :already-compiled ("b")
 :expected ((:compiled "a")
             (:loaded "a" :after-compiling "a")
             (:compiled "b" :after-compiling "a" :after-loading "a")
             (:did-not-load "b")))

(define-test test-3-a system-3
 :operation-name asdf:compile-op
 :already-compiled ("a" "b")
 :expected ((:did-not-compile "a")
             (:did-not-load "a")
             (:did-not-compile "b")
             (:did-not-load "b")))

(define-test test-3-b system-3
 :operation-name asdf:compile-op
 :already-compiled ()
 :expected ((:compiled "a")
             (:loaded "a" :after-compiling "a")
             (:compiled "b" :after-compiling "a" :after-loading "a")
             (:did-not-load "b")))

(asdf:defsystem system-4
 :components ((:test-file "a")
               (:test-file "b" :depends-on ("a"))))

(define-test test-4 system-4
 :operation-name asdf:compile-op
 :already-compiled ("a" "b")
 :expected ((:did-not-compile "a")
             (:did-not-load "a")
             (:did-not-compile "b")
             (:did-not-load "b")))

(asdf:defsystem system-5
 :components ((:test-file "a")
               (:test-file "b" :depends-on ("a")
                               :in-order-to ((asdf:compile-op 
                                              (asdf:load-op "a"))))))

(define-test test-5 system-5
 :operation-name asdf:compile-op
 :already-compiled ("a" "b")
 :expected ((:did-not-compile "a")
             (:loaded "a")
             (:did-not-compile "b")
             (:did-not-load "b")))


(asdf:defsystem system-6
 :components ((:test-file "a")
               (:test-file "b"
                           :in-order-to ((asdf:compile-op (asdf:compile-op "a"))
                                         (asdf:load-op (asdf:load-op "a")))
                           :do-first    ((asdf:compile-op (asdf:load-op "a"))))))

(define-test test-6 system-6
 :operation-name asdf:compile-op
 :already-compiled ("a" "b")
 :expected ((:did-not-compile "a")
             (:did-not-load "a")
             (:did-not-compile "b")
             (:did-not-load "b")))

;; Should be just like test-3-b, but a does not get loaded.   Why not?
;; Because you can't really specify :do-first because it clobbers
;; the :do-first that you provide!
(define-test test-6-a system-6
 :operation-name asdf:compile-op
 :already-compiled ()
 :expected ((:compiled "a")
             (:loaded "a" :after-compiling "a")
             (:compiled "b" :after-compiling "a" :after-loading "a")
             (:did-not-load "b")))




(asdf:defsystem system-7
 :components ((:test-file "a")
               (:test-file "b"
                           :in-order-to ((asdf:compile-op (asdf:compile-op "a"))
                                         (asdf:load-op (asdf:load-op "a"))
                                         (asdf:compile-op (asdf:load-op "a"))))))

(define-test test-7 system-7
 :operation-name asdf:compile-op
 :already-compiled ("a" "b")
 :expected ((:did-not-compile "a")
             (:loaded "a")
             (:did-not-compile "b")
             (:did-not-load "b")))
