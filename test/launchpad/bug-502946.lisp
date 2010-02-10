;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: asdf-user; -*-
;;;
;;; tests for https://bugs.launchpad.net/asdf/+bug/502946
;;;  nb. must load from a source file in order to find the utilities and for
;;;  the system definition to derive a pathname
;;;
;;; 2010-02-09 james.anderson@setf.de


(in-package :asdf-user)

(unless (fboundp 'run-unit-test)
  (load (make-pathname :directory '(:relative :up)
                       :name "dweinreb-utilities"
                       :type "lisp")))

;;; (run-unit-test 'bug-502946)

(defsystem :bug-502946
  :components ((:file "bar"
                :perform (compile-op :after (op c)
                           (push (cons :compiled (asdf:component-name c))
                                 *steps*))
                :perform (load-op :after (op c)
                           (push (cons :loaded (asdf:component-name c))
                                 *steps*)))
               (:module "quux"
                        :depends-on ("bar")
                        :components ((:file "zurp"
                                      :perform (compile-op :after (op c)
                                                (push (cons :compiled (asdf:component-name c))
                                                      *steps*))
                                      :perform (load-op :after (op c)
                                                (push (cons :loaded (asdf:component-name c))
                                                      *steps*)))))))

(defun bug-502946.touch-bar (system-name)
  (let* ((system (registered-system system-name))
         (bar (find-component system "/bar")))
    (with-open-file (stream (first (asdf:input-files 'asdf:compile-op bar))
                            :direction :output
                            :if-does-not-exist :error
                            :if-exists :append)
      stream)))

(defun bug-502946.touch-all (system-name)
  (let* ((system (asdf::registered-system system-name)))
    (dolist (file (mapcar #'(lambda (name) (asdf:find-component system name))
                          '("/bar" "/quux/zurp"))
                  t)
      (let ((pathname (first (asdf:input-files 'asdf:compile-op file))))
        (ensure-directories-exist pathname)
        (with-open-file (stream pathname
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :overwrite)
          (format stream "(in-package :asdf-user)~%")
          stream)))))

;; ? how to express the expecteds?
(define-test bug-502946 :bug-502946
 :operation-name (bug-502946.touch-all load-op bug-502946.touch-bar load-op)
 :already-compiled ()
 :expected ((:compiled "zurp" :after-compiling "bar" :after-loading "bar")
             (:loaded "zurp" :after-compiling "zurp")
             (:compiled "bar")
             (:loaded "bar" :after-compiling "bar")))
