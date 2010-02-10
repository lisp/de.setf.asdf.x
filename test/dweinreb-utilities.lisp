;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: asdf-user; -*-
;;; By Dan Weinreb, Oct 1, 2006.  Public domain, entirely free, etc.
#|
Daniel Weinreb
dlw@alum.mit.edu
http://danweinreb.org/blog/
http://ilc2009.scheming.org/
|#

(in-package :asdf-user)

(defparameter *all-tests* nil)

(defclass test ()
 ((system-name :initarg :system-name :reader test-system-name)
  (operations :initarg :operations :reader test-operations)
  (already-compiled :initarg :already-compiled :reader test-already-compiled)
  (expected :initarg :expected :reader test-expected)))

(defmacro define-test (test-name system-name 
                       &key operation-name (operations `(,operation-name))
                       already-compiled expected)
 `(progn
    (push ',test-name *all-tests*)
    (setf (get ',test-name 'test)
           (make-instance 'test
             :system-name ',system-name
             :operations ',operations
             :already-compiled ',already-compiled
             :expected ',expected))))

;; want's reflexive operations, but no files
(defclass test-file (asdf:component)
  ((asdf::reflexive-operations
    :initform '((load-op compile-op))
    :allocation :class)))

(defvar *test* nil)

(defvar *steps* nil)

;;; the origials undermined any logic which depends on dynamic state.
;;; if the operation is never done, multiple requirements yield
;;; multiple performances
#+(or)
(defmethod asdf:operation-done-p ((o asdf:compile-op) (c test-file))
 (declare (ignorable o))
 (member (asdf:component-name c) (test-already-compiled *test*) 
          :test #'string=))

#+(or)
(defmethod asdf:operation-done-p ((o asdf:operation) (c test-file))
 (declare (ignorable o c))
 nil)

(defmethod asdf:operation-done-p ((o asdf:compile-op) (c test-file))
 (declare (ignorable o))
 (or
  (member (asdf:component-name c) (test-already-compiled *test*) 
          :test #'string=)
  (call-next-method)))

(defmethod asdf:operation-done-p ((o asdf:operation) (c test-file))
  (find (cons (ecase (type-of o) (asdf:load-op :loaded) (asdf:compile-op :compiled))
              (asdf:component-name c))
        *steps*
        :test #'equal))

(defmethod asdf:perform ((o asdf:compile-op) (c test-file))
 (declare (ignorable o))
 (push (cons :compiled (asdf:component-name c)) *steps*))

(defmethod asdf:perform ((o asdf:load-op) (c test-file))
 (declare (ignorable o))
 (push (cons :loaded (asdf:component-name c)) *steps*))

(defun run-unit-test (test-name)
 (let ((*test* (get test-name 'test))
        (*steps* nil)
        (succeeded t))
   (flet ((fail (format-string &rest format-args)
             (setq succeeded nil)
             (format t "Error in test ~S: " test-name)
             (apply #'format t format-string format-args)
             (terpri)))
     (let ((system-name (test-system-name *test*))
            (operations (test-operations *test*)))
        (check-type system-name symbol)
        (dolist (operation operations)
          (check-type operation symbol)
          (cond ((subtypep operation 'asdf:operation)
                 (asdf:operate operation system-name))
                ((fboundp operation)
                 (funcall operation system-name))
                (t
                 (error "Invalid test operation: ~s." operation)))))
     (setq *steps* (nreverse *steps*))
     (loop for steps on *steps* do
        (when (member (first steps) (rest steps) :test #'equal)
          (fail "The step ~S happened more than once: ~S"
                (first steps) *steps*)))
     ;(format t "~2%STEPS: ~S~3%" *steps*)
     (dolist (expectation (test-expected *test*))
        (destructuring-bind (op file &rest at) 
            expectation
          ;(format t "~2%Expectation: ~S~3%" expectation)
          (check-type file string)
          (ecase op
            ((:compiled :loaded)
             (let ((pos (position (cons op file) *steps* :test #'equal)))
               (if (null pos)
                 (fail "~S was not ~A" file op)
                 (loop for (relationship file2) on at by #'cddr do
                   (check-type file2 string)
                   (let* ((op2 (ecase relationship 
                                 (:after-loading :loaded) 
                                 (:after-compiling :compiled)))
                          (pos2 (position (cons op2 file2) *steps*
                                          :test #'equal)))
                     (cond ((null pos2)
                            (fail "~S was not ~A at all, after ~A was ~A"
                                  file2 op2 file op))
                           ((< pos pos2)
                            (fail "Wrong order between ~A of ~S and ~A of ~S"
                                  op file op2 file2))))))))
            (:did-not-compile 
             (when (member (cons :compiled file) *steps*)
               (fail "~A compiled but should not have" file)))
            (:did-not-load
             (when (member (cons :loaded file) *steps*)
               (fail "~A loaded but should not have" file)))))))
   succeeded))

(defun run-all-unit-tests ()
 (let ((n-tests 0)
        (n-succeeded 0) (time 0))
 (dolist (test-name *all-tests*)
   (incf n-tests)
   (loop (when (> (get-universal-time) time)
           (setf time  (get-universal-time))
           (return)))
   (when (run-unit-test test-name)
     (incf n-succeeded)))
 (format t "Summary: ~D of ~D tests succeeded." n-succeeded n-tests)))

;;; (run-all-unit-tests)
