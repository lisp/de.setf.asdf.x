;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: asdf-user; -*-
;;;
;;; tests for https://bugs.launchpad.net/asdf/+bug/502946
;;;  nb. if the is run in a listener, it may not find the utilities and 
;;;  the system definition mauy not have derived a pathname.
;;;
;;; 2010-02-09 james.anderson@setf.de


(in-package :asdf-user)

(unless (fboundp 'run-unit-test)
  (load (make-pathname :directory '(:relative :up)
                       :name "dweinreb-utilities"
                       :type "lisp")))

;;; (run-unit-test 'bug-502946)

(defsystem :bug-502946
  :components
  ((:file "bar"
    :perform (compile-op :after (op c)
              (push (cons :compiled (component-name c)) *steps*))
    :perform (load-op :after (op c)
              (push (cons :loaded (component-name c)) *steps*)))
   (:module "quux"
    :strongly-depends-on ("bar")
    :components ((:file "zurp"
                  :perform (compile-op :after (op c)
                            (push (cons :compiled (component-name c)) *steps*))
                  :perform (load-op :after (op c)
                            (push (cons :loaded (component-name c)) *steps*)))))))

(defun bug-502946.touch-bar (system-name)
  (let* ((system (registered-system system-name))
         (c (find-component system "/bar")))
    (with-open-file (stream (component-pathname c)
                            :direction :output
                            :if-does-not-exist :error
                            :if-exists :append)
      (format stream "~%;;; touched~%")
      (push (cons :touched (component-name c)) *steps*)
      stream)))
;; (trace bug-502946.touch-bar)

(defun bug-502946.touch-all (system-name)
  (let* ((system (registered-system system-name)))
    (dolist (c (mapcar #'(lambda (name) (find-component system name))
                          '("/bar" "/quux/zurp"))
                  t)
      (let ((pathname (component-pathname c)))
        (ensure-directories-exist pathname)
        (with-open-file (stream pathname
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede)
          (format stream "(in-package :asdf-user)~%")
          (push (cons :touched (component-name c)) *steps*)
          stream)))))

(defun print-steps (system-name)
  (declare (ignore system-name))
  (print (reverse *steps*)))

;; ? how to express the expecteds?
(define-test bug-502946 :bug-502946
 :operations (bug-502946.touch-all load-op bug-502946.touch-bar load-op print-steps)
 :already-compiled ()
 :equal ((:touched . "bar") (:touched . "zurp")
         (:compiled . "bar") (:loaded . "bar") (:compiled . "zurp") (:loaded . "zurp")
         (:touched . "bar")
         (:compiled . "bar") (:loaded . "bar") (:compiled . "zurp") (:loaded . "zurp")))

;; two distinct dependencies:
;; 1. existence
;; 1. temporal
;; the module dependency has been existence for ordering. if one would want it to be
;; temporal, then one would need to specify the transitivity.