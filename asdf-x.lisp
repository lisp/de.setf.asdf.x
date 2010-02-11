;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: asdf-extensions.x; -*-

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

;;;; -------------------------------------------------------------------------
;;;;
;;;; Contents
;;;; - package definitions: asdf, asdf.implementation, asdf-extensions
;;;; - patches : housekeeping to bridge incompatible changes
;;;; - ASDF system definition interface : defsystem &co
;;;; - CLOS/MOP : standard-asdf-method-combination
;;;; - ASDF operator interface
;;;;   - perform, explain, operation-done-p
;;;;   - operate
;;;; - conditions : operation-error & specializations
;;;; - status : performace-complete, -incomplete, and -failed
;;;; - model classes:
;;;;    component
;;;;    restartable-component
;;;;    component-reference
;;;;    class-context
;;;;    group
;;;;    module
;;;;    system
;;;;    file : source-file, cl-source-file, java-source-file, html-file
;;;; - operation
;;;;    load-op
;;;;    compile-op
;;;; - method definitions : components
;;;;    system : includes find-system
;;;; - method definitions : operations
;;;;    perform, compile-op, feature-op, load-op, load-source-op, test-op
;;;; - general utilities : 
;;;;     sadf-message
;;;;     canonicalize-component-option
;;;; - bootstrap
;;;; -------------------------------------------------------------------------
;;;; Package Definitions

(defpackage #:asdf.x
  (:documentation "Another System Definition Facility")
  (:use :cl)
  (:export #:defsystem #:oos #:operate #:find-system #:run-shell-command
           #:system-definition-pathname #:find-component ; miscellaneous
           #:compile-system #:load-system #:test-system
           #:compile-op #:load-op #:load-source-op
           #:test-op
           #:operation                 ; operations
           #:compile-op
           #:load-op
           #:load-source-op
           #:registered-system
           #:feature                 ; sort-of operation
           #:version                 ; metaphorically sort-of an operation
           
           #:input-files #:output-files #:perform ; operation methods
           #:operation-done-p #:explain
           
           #:component #:source-file
           #:c-source-file #:cl-source-file #:java-source-file
           #:static-file
           #:doc-file
           #:html-file
           #:text-file
           #:source-file-type
           #:module                     ; components
           #:file
           #:system
           #:unix-dso
           
           #:group-constituents          ; component accessors
           #:component-pathname
           #:component-relative-pathname
           #:component-name
           #:component-version
           #:component-parent
           #:component-property
           #:component-system
           #:component-depends-on
           #:reflexive-operations
           #:transitive-operations
           
           #:system-description
           #:system-long-description
           #:system-author
           #:system-maintainer
           #:system-license
           #:system-licence
           #:system-source-file
           #:system-relative-pathname
           #:map-systems
           
           #:operation-on-warnings
           #:operation-on-failure
           
           #:*system-definition-search-functions*
           #:*central-registry*         ; variables
           #:*operation-force*
           #:*operation-warnings-behaviour*
           #:*operation-failure-behaviour*
           #:*operation-missing-behaviour*
           #:*resolve-symlinks*
           #:*pathname-resolver*
           #:*pathname-path-punctuation-character*
           
           #:asdf-version
           
           #:operation-error #:compile-failed #:compile-warned #:compile-error
           #:error-name
           #:error-pathname
           #:missing-definition
           #:error-component #:error-operation
           #:system-definition-error
           #:missing-system
           #:missing-constituent
           #:missing-dependency
           #:circular-dependency        ; errors
           #:duplicate-names
           
           #:try-recompiling
           #:retry
           #:accept                     ; restarts
           #:coerce-entry-to-directory
           #:remove-entry-from-registry
           
           #:standard-asdf-method-combination
           
           
           #:initialize-source-registry
           #:clear-source-registry
           #:ensure-source-registry
           #:process-source-registry)
  (:intern #:asdf-message
           #:canonicalize-component-option
           #:coerce-name
           #:context-find-class
           #:compute-performance-status
           #:dependency
           #:getenv
           #:pathname-sans-name+type
           #:perform-constituent
           #:perform-requirement
           #:restart
           #:resolve-symlinks
           #:system-registered-p))


(defpackage #:asdf-extensions.x
  (:use #:common-lisp :asdf.x)
  (:import-from #:asdf.x
                #:asdf-message
                #:canonicalize-component-option
                #:coerce-name
                #:context-find-class
                #:compute-performance-status
                #:dependency
                #:getenv
                #:pathname-sans-name+type
                #:perform-constituent
                #:perform-requirement
                #:restart
                #:resolve-symlinks
                #:system-registered-p))


;;; create the prospective appearance to allow tests to be written as-if
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :asdf)
    (rename-package :asdf.x :asdf.x '(:asdf)))
  (unless (find-package :asdf)
    (rename-package :asdf-extensions.x :asdf-extensions.x '(:asdf-extensions))))


(defpackage #:asdf-user
  (:use #:common-lisp :asdf.x)
  (:documentation "The package into which .asd files should be loaded."))

#+(or ) ;;??? distinct from the extension package for this file itself
(defpackage :asdf.i
  (:use :common-lisp :asdf.x :asdf-extensions.x))


#+nil
(error "The author of this file habitually uses #+nil to comment out ~
        forms. But don't worry, it was unlikely to work in the New ~
        Implementation of Lisp anyway")

;;; eventually asdf-extensions
(in-package #:asdf-extensions.x)

;;;; -------------------------------------------------------------------------
;;;; -------------------------------------------------------------------------
;;;; Patches: Cleanups before hot-upgrade.
;;;; Things to do in case we're upgrading from a previous version of ASDF.
;;;; See https://bugs.launchpad.net/asdf/+bug/485687
;;;; * fmakunbound functions that once (in previous version of ASDF)
;;;;   were simple DEFUNs but now are generic functions.
;;;; * define methods on UPDATE-INSTANCE-FOR-REDEFINED-CLASS
;;;;   for each of the classes we define that has changed incompatibly.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (fboundp 'system-source-file)
             (not (typep (fdefinition 'system-source-file) 'generic-function)))
    (fmakunbound 'system-source-file))
  (when (and (fboundp 'system-definition-pathname)
             (not (typep (fdefinition 'system-definition-pathname) 'generic-function)))
    (fmakunbound 'system-definition-pathname))
  #+ecl
  (when (find-class 'compile-op nil)
    (defmethod update-instance-for-redefined-class :after
        ((c compile-op) added deleted plist &key)
      (format *trace-output* "~&UI4RC:a ~S~%" (list c added deleted plist))
      (let ((system-p (getf plist 'system-p)))
        (when system-p (setf (getf (slot-value c 'flags) :system-p) system-p))))))

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

;;;; -------------------------------------------------------------------------
;;;; User-visible parameters
;;;;

(defparameter *asdf-version*
  ;; the 1+ hair is to ensure that we don't do an inadvertent find and replace
  (subseq "VERSION:1.502" (1+ (length "VERSION"))))

(defun asdf-version ()
  *asdf-version*)

(defun make-defined-systems-table ()
  (make-hash-table :test 'equal))

(defvar *defined-systems* (make-defined-systems-table)
  "This is a hash table whose keys are strings, being the
names of the systems, and whose values are systems.")

(defvar +asdf-package+ (find-package '#:asdf.x))
(defvar +asdf-user-package+ (find-package '#:asdf-user))

(defvar *resolve-symlinks* t
  "Determine whether or not ASDF resolves symlinks when defining systems.

Defaults to `t`.")

(defvar *operation-force* nil)

(deftype warning-behaviour () '(member :warn :error :ignore :continue nil))
(deftype failure-behaviour () '(member :warn :error :ignore :continue nil))
(deftype missing-behaviour () '(member :warn :error :ignore :continue))
(deftype success-behaviour () '(member :force :continue))

(defvar +operation-failure-behaviour+ #+sbcl :error #-sbcl :warn)
(defvar +operation-missing-behaviour+ :warn)
(defvar +operation-warning-behaviour+ :warn)
(defvar +operation-success-behaviour+ :continue)

(defvar *operation-failure-behaviour* +operation-failure-behaviour+)
(defvar *operation-missing-behaviour* +operation-missing-behaviour+)
(defvar *operation-warning-behaviour* +operation-warning-behaviour+)
(defvar *operation-success-behaviour* +operation-success-behaviour+)
(defvar *operation-circular-behaviour* :error)          ; not dynamically rebound


(defvar *operation-stack* nil
  "A dynamic stack of (operation-type . component) entries maintained by
 perform in order to detect circularity.")

(defvar *verbose-out* nil)

(defvar +null-pathname+ (make-pathname :name nil :type nil :directory nil :host nil))

(defvar *pathname-resolver* nil)

(defvar *pathname-punctuation-character* #\/)
(defvar *component-punctuation-character* #\/)

(defvar *system-load-package* +asdf-user-package+
  "The package into which system definitions are loaded. If null, a new
 package is created for each load and delted thereafter.")

(defvar *if-feature-does-not-exist* :error
  "Specifies the action if a feature constraint is not satisfied.")

(defvar +traversal-order+ :postorder)

;;; these values are the way they are 'cause that's the way it was.
;;; they should not be this way. they should be () and the system/module
;;; should be specialized.
(defvar +module-classes+ '((:file . cl-source-file))
  "The class map with which the `module` class will be initialized.")
(defvar +system-classes+ '((:file . cl-source-file))
  "The class map with which the `module` class will be initialized.")

;;; these values are the way they are 'cause that's the way it was.
;;; they should not be this way. the component values should be ()
(defvar +component-transitive-operations+ '((compile-op load-op)))
(defvar +component-reflexive-operations+ '((load-op compile-op)))
(defvar +source-file-transitive-operations+ '((compile-op load-op)))
(defvar +source-file-reflexive-operations+ '((load-op compile-op)))
(defvar +module-transitive-operations+ '((t t)))

(defvar *traversal-order* +traversal-order+
  "Specifies the order of operation action between a group proper and
 its constituents.
 - :inorder - indicates to first perform the operation on the group component;
 - :postorder - indicates to first perform the operation on the constituents;
 - :preorder - indicates to first perform the component, then the dependencies,
 and then the constituents - applies to systems.
 The global value corresponds to the component :traversal-order setting.")

(defparameter +asdf-methods+
  '(perform explain output-files operation-done-p)
  "The list of methods which will be recognized in component initialization argument
 list.")

(defparameter +asdf-requirement-names+
  '(:do-first :in-order-to :depends-on :weakly-depends-on :strongly-depends-on)
  "The list of requirements which will be recognized in component initialization argument
 list when constructing the dependency links.")

(defvar *operation-stack* nil
  "Collects a stack if (operation-type . component) pairs inthe dynamic context of
 perform calls in order to recognize circularity.")

(defvar *sysdef-file-type* "asdx"
  "Specifies the file type for system definition files")

(defvar *extension-file-type* "asdx"
  "Specifies the file type for component, and constraint class definition files.")

(defvar *system-definition-search-functions*
  '(sysdef-central-registry-search)
  "For the sake of keeping things reasonably neat, we adopt a
 convention that functions in this list are prefixed SYSDEF-")

(defvar *central-registry*
  `((directory-namestring *default-pathname-defaults*))
"A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or a function
which evaluates to a pathname. For example:

    (setf asdf:*central-registry*
          (list '*default-pathname-defaults*
                #p\"/home/me/cl/systems/\"
                #p\"/usr/share/common-lisp/systems/\"))
")

(defun call-with-settings (op &rest settings)
  (declare (dynamic-extent settings))
  (let ((saved-settings ()))
    (unwind-protect
      (progn (loop for (setting value) on settings by #'cddr
                   do (setf saved-settings
                            (list* setting (shiftf (setting setting) value)
                                   saved-settings)))
             (funcall op))
      (loop for (setting value) on saved-settings by #'cddr
            do (setf (setting setting) value)))))

(defmacro with-settings ((&rest settings) &rest body)
  `(flet ((.settings-contour. () ,@body))
    (declare (dynamic-extent #'.settings-contour.))
    (call-with-settings #'.settings-contour. ,@settings)))

(defgeneric setting (name)
  (:method ((name symbol))
    (ecase name                         ; anything else-> its own method
      (:warning-behaviour *operation-warning-behaviour*)
      (:success-behaviour *operation-success-behaviour*)
      (:failure-behaviour *operation-failure-behaviour*)
      (:missing-behaviour *operation-missing-behaviour*)
      (:central-registry *central-registry*)
      (:resolve-symlinks *resolve-symlinks*)
      (:features *features*)
      (:package *package*)
      (:force *operation-force*)
      (:traversal-order *traversal-order*))))

(defgeneric (setf setting) (value name)
  (:documentation "Set a system parameter by name to a given value. A method
 must exist for the respective name which permits aand/or coerces the given value
 and sets the global parameter.")
  (:method ((value t) (name t))
    (error "Invalid setting: ~s, ~s." name value))

  (:method (value (name (eql :success-behaviour)))
    (if (typep value 'success-behaviour)
      (setq *operation-success-behaviour* value)
      (call-next-method)))

  (:method (value (name (eql :warning-behaviour)))
    (if (typep value 'warning-behaviour)
      (setq *operation-warning-behaviour* value)
      (call-next-method)))

  (:method (value (name (eql :failure-behaviour)))
    (if (typep value 'failure-behaviour)
      (setq *operation-failure-behaviour* value)
      (call-next-method)))

  (:method (value (name (eql :missing-behaviour)))
    (if (typep value 'missing-behaviour)
      (setq *operation-missing-behaviour* value)
      (call-next-method)))

  (:method ((value list) (name (eql :central-registry)))
    (setq *central-registry* value))

  (:method (value (name (eql :resolve-symlinks)))
    (if (typep value 'boolean)
      (setq *resolve-symlinks* value)
      (call-next-method)))

  (:method ((value list) (name (eql ':features)))
    "monotonic addition to features."
    (setq *features* (append value *features*)))

  (:method ((value t) (name (eql ':package)))
    (setq *package* (or (find-package name) (error "Package not found: ~s" name))))

  (:method ((value symbol) (name (eql :force)))
    (setq *operation-force* value))

  (:method ((value t) (name (eql :traversal-order)))
    (setq *traversal-order* value)))

(defmacro with-standard-operation-behaviour (&rest body)
  `(let ((*operation-failure-behaviour* +operation-failure-behaviour+)
         (*operation-missing-behaviour* +operation-missing-behaviour+)
         (*operation-warning-behaviour* +operation-warning-behaviour+)
         (*operation-success-behaviour* +operation-success-behaviour+)
         (*traversal-order* +traversal-order+))
     ,@body))

(defmacro with-saved-settings (&rest body)
  `(let ((*central-registry* *central-registry*)
         (*features* *features*)
         (*package* *package*)
         (*resolve-symlinks* *resolve-symlinks*)
         (*component-punctuation-character* *component-punctuation-character*)
         (*pathname-punctuation-character* *pathname-punctuation-character*))
     ,@body))
         

(defvar *status-aborted* )
(defvar *status-circular* )
(defvar *status-failed* )
(defvar *status-ignored* )
(defvar *status-missing* )
(defvar *status-repeated* )
(defvar *status-skipped* )
(defvar *status-succeeded* )
(defvar *status-warned* )


;;;; -------------------------------------------------------------------------
;;;; ASDF Definition Interface, in terms of macros and functions
;;;;  defsystem
;;;;  defmodule
;;;;  defelement
;;;;  define-system
;;;;  define-component

;;; asdf.o implemnted the definition operator in a wonderfully inscrutable manner.
;;; on-the-fly, it destructured the definition form, canoncialized arguments,
;;; resolved class designators relative to context components, and ...
;;; this alternative implementation perfomrs the tasks in clear phases.
;;; - the macros destructure and generate executable definitions
;;; - the delayed execution step - to make the context before the constituent
;;;   is handled by doing exactly that: define the context component and
;;;   delegate the task of defining its components to it. when a component
;;;   comprises constituents, a function is defined to do that when applied to the context
;;;   
(labels ((rewrite-options (class options)
           (let ((declared-class class)
                 (prototype (allocate-instance (context-find-class nil class :type 'component)))
                 (initargs ())
                 (initforms ())
                 (context-var (intern (format nil "~a-~a" :the class) (symbol-package 'defsystem))))
             (do ((key (pop options) (pop options))
                  (form (pop options) (pop options)))
                 ((null key))
               (case key
                 (:class
                  (setf class form))
                 ((:components :constituents)
                  (setf initforms
                        (nconc initforms
                               (mapcar #'(lambda (component)
                                           (etypecase component
                                             (cons
                                              (destructuring-bind (type name . options) component
                                                (rewrite-component context-var type name options)))))
                                       form))))
                 (t
                  (multiple-value-bind (key form) (canonicalize-component-option prototype key form)
                    (setf initargs (nconc initargs (list key form)))))))
             (values declared-class initargs
                     ;; if constituents were present, include the operator to allow the
                     ;; context component to create them.
                     (when initforms `(lambda (,context-var) ,@initforms ,context-var)))))
         (rewrite-component (context type name options)
           (multiple-value-bind (class initargs initfunction) (rewrite-options type options)
             (let ((definition-form `(define-component ,context ',class :name ,(coerce-name name) ,@initargs)))
               (if initfunction
                 `(,initfunction ,definition-form)
                 definition-form)))))
  
  (defmacro defcomponent (context type name &body options)
    (rewrite-component context type name options))
  
  (defmacro defmodule (system name &body options)
    `(defcomponent ,system :module ,name ,@options))
  
  (defmacro defelement (system name &body options)
    `(defcomponent ,system :element ,name ,@options))
  
  
  (defmacro defsystem (name &body options)
    "Define a system with the given name, compoennts, and other options.
 The option clauses correspond to the intiialization parameters for the respective
 system class, with the exception that the :class options specifies the class itself.
 
 Clauses are intended to comprise self-evaluating forms and element designators, which are
 lists which combine a compoent class designator a list of further clauses. The clauses are
 rewritten as follows:
 string -> string
 keyword -> keyword
 symbol -> 'symbol
 designator-list -> (add-component <context> 'class-designator . elements"
    
    (multiple-value-bind (class initargs initfunction) (rewrite-options 'system options)
      (let ((definition-form `(define-system ',class :name ,(coerce-name name)
                                ,@initargs)))
        (if initfunction
          `(,initfunction ,definition-form)
          definition-form)))))



;;;; -------------------------------------------------------------------------
;;;; CLOS magic for asdf performance protocol


(define-method-combination standard-asdf-method-combination ()
  ((asdf-dependency (asdf.x::dependency))
   (asdf-restart (asdf.x::restart))
   (asdf-around (asdf.x::around))       ; dynamic upgrade: delete them
   (around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (:generic-function gf)
  (when asdf-around                     ; upgrade cleans obsolete methods
    (asdf-message "Deleting obsolete 'around' methods: ~s." asdf-around)
    (dolist (method asdf-around)
      (remove-method gf method)))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let* ((form (if (or before after (rest primary))
                     `(multiple-value-prog1
                          (progn ,@(call-methods before)
                                 (call-method ,(first primary)
                                              ,(rest primary)))
                        ,@(call-methods (reverse after)))
                     `(call-method ,(first primary)))))
      ;; arrange them such that dependency wraps restartable constituency/self
      ;; performance, which wraps the standard methods
      (dolist (wrappers (list around asdf-dependency asdf-restart))
        (when wrappers
          (setf form `(call-method ,(first wrappers)
                                   (,@(rest wrappers) (make-method ,form))))))
      form)))
    
(setf (documentation 'standard-asdf-method-combination
                     'method-combination)
      "This method combination follows the standard method combination pattern,
but adds new method-qualifiers, `asdf:dependency` and `asdf:restart`.
- `asdf:dependency` methods will be run *around* any `:around` methods, so that
the core perform protocol dependency mechanism will not interfere with around
methods added by a system developer.
- `asdf:restart` methods are run around everything for restartable components
to support restart cases immediate and reflexive operations.")


;;;; -------------------------------------------------------------------------
;;;; ASDF Operator Interface, in terms of generic functions.


(defgeneric perform (operation component)
  (:method-combination standard-asdf-method-combination))
(defgeneric operation-done-p (operation component)
  (:method-combination standard-asdf-method-combination))
(defgeneric explain (operation component)
  (:method-combination standard-asdf-method-combination))
(defgeneric output-files (operation component)
  (:method-combination standard-asdf-method-combination))
(defgeneric input-files (operation component)
  (:method-combination standard-asdf-method-combination))

(defgeneric system-source-file (system)
  (:documentation "Return the source file in which system is defined."))

(defgeneric component-system (component)
  (:documentation "Find the top-level system containing COMPONENT")
  (:method ((component null)) nil))

(defgeneric component-pathname (component)
  (:documentation "Extracts the pathname applicable for a particular component."))

(defgeneric component-relative-pathname (component)
  (:documentation "Extracts the relative pathname applicable for a particular component."))

(defgeneric component-property (component property))

(defgeneric (setf component-property) (new-value component property))

(defgeneric version-satisfies (component version))

(defgeneric find-component (module name &optional version)
  (:documentation "Finds the component with name NAME present in the
MODULE module; if MODULE is nil, then the component is assumed to be a
system."))

(defgeneric source-file-type (component))

(defgeneric operation-ancestor (operation)
  (:documentation
   "Recursively chase the operation's parent pointer until we get to
the head of the tree"))

(defgeneric component-visited-p (operation component))

(defgeneric visit-component (operation component data))

(defgeneric (setf visiting-component) (new-value operation component))

(defgeneric component-visiting-p (operation component))

(defgeneric component-depends-on (operation component)
  (:documentation
   "Returns a list of dependencies needed by the component to perform
    the operation.  A dependency has one of the following forms:

      (<operation> <component>*), where <operation> is a class
        designator and each <component> is a component
        designator, which means that the component depends on
        <operation> having been performed on each <component>; or

      (FEATURE <feature>), which means that the component depends
        on <feature>'s presence in *FEATURES*.

    Methods specialized on subclasses of existing component types
    should usually append the results of CALL-NEXT-METHOD to the
    list."))

(defgeneric component-self-dependencies (operation component))

(defgeneric traverse (operation component)
  (:documentation
"Generate and return a plan for performing `operation` on `component`.

The plan returned is a list of dotted-pairs. Each pair is the `cons`
of ASDF operation object and a `component` object. The pairs will be
processed in order by `operate`."))



;;;; -------------------------------------------------------------------------
;;;; Invoking Operations

(defgeneric operate (operation system &key)

  (:method ((operation symbol) system &rest args)
    (apply #'operate (apply #'make-instance operation :original-initargs args args)
           system
           args)))


(defun oos (operation-class system &rest args &key force (verbose t) version
            &allow-other-keys)
  (declare (ignore force verbose version))
  (apply #'operate operation-class system args))

(let ((operate-docstring
  "Operate does three things:

1. It creates an instance of `operation-class` using any keyword parameters
as initargs.
2. It finds the  asdf-system specified by `system` (possibly loading
it from disk).
3. It then calls `perform` with the operation and system as arguments

The perform operation is wrapped in `with-compilation-unit` and error
handling code. If a `version` argument is supplied, then operate also
ensures that the system found satisfies it using the `version-satisfies`
method.

It returns two arguments:
- true if the operation completes successfully and false otherwise
- the completion status

Note that dependencies may cause the operation to invoke other
operations on the system or its components: the new operations will be
created with the same initargs as the original one.
"))
  (setf (documentation 'oos 'function)
        (format nil
                "Short for _operate on system_ and an alias for the [operate][] function. ~&~&~a"
                operate-docstring))
  (setf (documentation 'operate 'function)
        operate-docstring))

(defun load-system (system &rest args &key force (verbose t) version)
  "Shorthand for `(operate 'asdf:load-op system)`. See [operate][] for details."
  (declare (ignore force verbose version))
  (apply #'operate 'load-op system args))

(defun compile-system (system &rest args &key force (verbose t) version)
  "Shorthand for `(operate 'asdf:compile-op system)`. See [operate][] for details."
  (declare (ignore force verbose version))
  (apply #'operate 'compile-op system args))

(defun test-system (system &rest args &key force (verbose t) version)
  "Shorthand for `(operate 'asdf:test-op system)`. See [operate][] for details."
  (declare (ignore force verbose version))
  (apply #'operate 'test-op system args))



;;;; -------------------------------------------------------------------------
;;;; Conditions

(define-condition system-definition-error (error) ()
  ;; [this use of :report should be redundant, but unfortunately it's not.
  ;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function
  ;; over print-object; this is always conditions::%print-condition for
  ;; condition objects, which in turn does inheritance of :report options at
  ;; run-time.  fortunately, inheritance means we only need this kludge here in
  ;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]
  #+cmu (:report print-object))

(define-condition formatted-system-definition-error (system-definition-error)
  ((format-control :initarg :format-control :reader format-control)
   (format-arguments :initarg :format-arguments :reader format-arguments))
  (:report (lambda (c s)
             (apply #'format s (format-control c) (format-arguments c)))))

(define-condition missing-definition (system-definition-error)
  ((name :initarg :name :reader error-name)
   (pathname :initarg :pathname :reader error-pathname))
  (:report (lambda (c s)
             (format s "~@<Definition search function returned a wrong pathname ~A ~
                        in search of a definition for system ~A.~@:>"
                     (error-pathname c) (error-name c)))))

(define-condition circular-dependency (system-definition-error)
  ((components :initarg :components :reader circular-dependency-components)))

(define-condition duplicate-names (system-definition-error)
  ((name :initarg :name :reader duplicate-names-name)))

(define-condition missing-component (system-definition-error)
  ((requires :initform "(unnamed)" :reader missing-component-requires :initarg :requires)
   (required-by :initarg :required-by :reader missing-component-required-by)
   (version :initform nil :reader missing-component-version :initarg :version))
  (:report (lambda (c s)
             (format s "~@<Component ~S not found~@[ at version ~a~]~@[ for ~A~]~@:>"
                     (typecase (missing-component-requires c)
                       (null nil)
                       (component (component-name (missing-component-requires c)))
                       (t (missing-component-requires c)))
                     (missing-component-version c)
                     (when (missing-component-required-by c)
                       (component-name (missing-component-required-by c)))))))

(define-condition missing-system (missing-component)
  ()
  (:report (lambda (c s)
             (format s "~@<System ~S not found~@[ at version ~a~]~@:>"
                     (missing-component-requires c)
                     (missing-component-version c)))))


(define-condition missing-dependency (missing-component)
  ())

(define-condition missing-constituent (missing-component)
  ())

(define-condition operation-error (error)
  ((component :reader error-component :initarg :component)
   (operation :reader error-operation :initarg :operation))
  (:report (lambda (c s)
             (format s "~@<erred while invoking ~A on ~A~@:>"
                     (error-operation c) (error-component c)))))

(define-condition operation-failed (operation-error) ())
(define-condition operation-warned (operation-error) ())

(define-condition compile-error (operation-error) ())
(define-condition compile-failed (compile-error operation-failed) ())
(define-condition compile-warned (compile-error operation-warned) ())


(defun sysdef-error (format &rest arguments)
  (error 'formatted-system-definition-error
         :format-control format :format-arguments arguments))

;;;; -------------------------------------------------------------------------
;;;; Status classes

(defclass performance-status () ())
(defclass performance-complete (performance-status) ())
(defclass performance-incomplete (performance-status) ())
(defclass performance-failed (performance-status) ())

(defclass status-succeeded (performance-complete) ())
(defclass status-warned (status-succeeded) ())
(defclass status-repeated (performance-complete) ())
(defclass status-failed (performance-failed) ())
(defclass status-circular (performance-failed) ())
(defclass status-aborted (performance-incomplete) ())
(defclass status-missing (performance-incomplete) ())
(defclass status-ignored (performance-incomplete) ())
(defclass status-skipped (performance-incomplete) ())

(setq *status-aborted* (make-instance 'status-aborted))
(setq *status-failed* (make-instance 'status-failed))
(setq *status-circular* (make-instance 'status-circular))
(setq *status-ignored* (make-instance 'status-ignored))
(setq *status-missing* (make-instance 'status-missing))
(setq *status-repeated* (make-instance 'status-repeated))
(setq *status-skipped* (make-instance 'status-skipped))
(setq *status-succeeded* (make-instance 'status-succeeded))
(setq *status-warned* (make-instance 'status-warned))

(defgeneric performance-complete-p (performance)
  (:documentation "Return t if the argument is performance-complete _or_
 a non-performace type. Return nil for -incomplete and -failed.")

  (:method ((status performance-complete))   t)
  (:method ((status performance-incomplete)) nil)
  (:method ((status performance-failed))     nil)
  ;; treat anomalous as true
  (:method ((status t))                      t))


;;;; -------------------------------------------------------------------------
;;;; Model Classes


(defclass component ()
  ((name
    :initform (error "name required") :initarg :name
    :accessor component-name
    :type string
    :documentation
    "Component name: designator for a string composed of portable pathname characters")
   (parent
    :initform nil :initarg :parent
    :accessor component-parent :reader context-parent)
   (constituents
    :initform nil :initarg :constituents :initarg :components
    :accessor component-constituents)
   (unreinitialized-constituents
    :initform nil :accessor component-unreinitialized-constituents
    :documentation "Caches constituents as of reinitialization for use to
 resolve names for subsequent define-component calls in order to reuse
 previous constituents, and eventually to remove any dangling inline methods.")
   (dependencies
    :initform nil
    :accessor component-dependencies)
   (version
    :initform nil  :initarg :version
    :accessor component-version)
   (description
    :initform nil :initarg :description
    :accessor component-description)
   (requirement-names
    :initform +asdf-requirement-names+ :allocation :class
    :reader component-requirement-names
    :documentation "The list of asdf dependency relations for which performance requirements
     will be incorporated into the component when it is updated.
     (see component-dependency-requirements)")
   (constituency-requirements
    :reader get-component-constituency-requirements
    :writer (setf component-constituency-requirements))
   (dependency-requirements
    :reader get-component-dependency-requirements
    :writer (setf component-dependency-requirements))
   #+(or )
   (in-order-to
    :initform nil :initarg :in-order-to
    :accessor component-in-order-to
    :documentation "The list of dependency definitions which is interpreted
     when performing operations on the component. The value is of the form:
       in-order-to ::= ( dependency+ )
       dependency  ::= ( dependent-op requirement+ )
       requirement ::= ( required-op required-component+ )
                     | ( predicate-op arguments* )
       required-component ::= component-name | ( component-name argument* )
     If dependent-op is t, then required-op is also t and serves as a wild-card
     to match all operators.")
   ;; XXX crap name
   #+(or )
   (do-first
    :initform nil :initarg :do-first
    :accessor component-do-first)
   ;; no direct accessor for pathname, we do this as a method to allow
   ;; it to default in funky ways if not supplied
   (pathname
    :initform nil
    :reader get-component-pathname
    :writer setf-component-pathname
    :documentation "The external location at which the component data is stored.
     This applies to the pathname for source text for program files, or the directory name
     for modules or systems. If an extension for uri is enabled, it can also denote a remote
     location.")
   (relative-pathname
    :initform nil
    :reader get-component-relative-pathname
    :writer setf-component-relative-pathname)
   (input-files
    :initform nil
    :accessor component-input-files
    :documentation "A cached a-list which relates operations types to their
     input files. It is computed on-demand based on the declared reflexive
     operations.")
   (operation-tags
    :initform (make-hash-table)
    :accessor component-operation-tags)
   ;; XXX we should provide some atomic interface for updating the
   ;; component properties
   (properties
    :initform nil :initarg :properties
    :accessor component-properties)
   (inline-methods
    ;; no accessor, no initialization
    ;; should one get to use the mop, specializer-direct-methods would be better.
    :documentation "A cache of the methods defined using the 'inline' style
     in a defsystem form. Each entry is of the form
        (function-name method)
     The cache is used to delete them when the component is updated without
     requiring mop support to examine spacializers. It is initially unbound and
     filled as a side-effect of processing those initialization arguments which
     apper in `component-inline-method-names`.")
   (inline-method-names
    :initform +asdf-methods+ :allocation :class
    :reader component-inline-method-names
    :documentation "The list of asdf operators for which 'inline' methods will
     be generated when the component is instantiated or reinitialized. The initial
     value is that of `+asdf-methods+`")
   (reflexive-operations
    :initform +component-reflexive-operations+
    ;; as long as these reflect source file requirements, class allocation is wrong
    ;; :allocation :class
    :reader component-reflexive-operations
    :documentation "The operational dependencies automatically generated
 for operations on the component itelf.")
   (transitive-operations
    :initform +component-transitive-operations+
    ;; :allocation :class
    :reader component-transitive-operations
    :documentation "The operational dependencies automatically generated
 from operations on the component to operations on a required component.")
   (settings
    :initform nil :initarg :settings
    :accessor component-settings
    :documentation "Each component asserts its own settings around the dynamic extent
 of any operations on it."))
  (:documentation "A component is an abstract element of a system. Its specialized forms
    include entire systems, groups of other components and individual files."))


(defclass restartable-component (component)
  ())


(defclass component-reference (node)
  ((version :initform nil :initarg :version
            :reader component-version))
  (:documentation "A component-reference acts as a delegate for an unresolved component."))
(defclass dependency-reference (component-reference) ())
(defclass constituency-reference (component-reference) ())

(defclass class-context ()
  ((classes
    :initform () :initarg :classes :allocation :class
    :accessor context-classes)))

(defclass group (restartable-component class-context)
  ((constituents
    :accessor group-constituents))
  (:documentation "A group is a setf of constituent components for for which an
 operation applies transitively. It serves to amalgamate the operation, but does not
 affest locations."))


(defclass module (group)
  ((constituents
    :accessor module-components)
   (classes
    :initform +module-classes+
    ;; as long as they reflect source-file presumptions
    ;; :allocation :class
    )
   (settings
    :initform '(:traversal-order (:requirements :constituents)))
   (transitive-operations
    :initform +module-transitive-operations+ :allocation :class
    :documentation "The module operational dependencies by default pass everything through.")))


(defclass system (group)
  ((classes
    :initform +system-classes+
    ;; as long as they reflect source-file presumptions
    ;; :allocation :class
    )
   (long-description
    :initform nil  :initarg :long-description
    :accessor system-long-description)
   (author
    :initform nil :initarg :author
    :accessor system-author)
   (maintainer
    :initform nil :initarg :maintainer
    :accessor system-maintainer)
   (licence
    :initform nil
    :accessor system-licence :initarg :licence
    :accessor system-license :initarg :license)
   (source-file
    :initform nil :initarg :source-file
    :reader system-source-file
    :writer %set-system-source-file
    :documentation "The pathname for the system definition file itself.
     By default, in the `.asd` file named as the system in same directory as the
     component pathname.")
   (settings
    :initform '(:traversal-order :preorder)))
  (:documentation "A system is intended as a top-level component. It comprises elementary components
 and groups, provides them with a base location, and with initial operation settings.
 In addition it collects documentation information about th esystem as a whole, such as author,
 and maintainer."))


;;; distinguish files, which are unified by physical pathname
;;; from source files, which describe a particular role in a particular context

(defclass file (component)
  ())


(defclass source-file (restartable-component file)
  ((type                                ; specialization must provide for this
    :initform (error "type is required.")
    :reader source-file-type)
   (output-files
    :initform nil :initarg :output-files
    :accessor component-output-files
    :documentation "Caches the output files for the respective reference file for
 specific to each operation type.")
   (reflexive-operations
    :initform +source-file-reflexive-operations+ :allocation :class)
   (transitive-operations
    :initform +source-file-transitive-operations+ :allocation :class))
   (:documentation "A source-file is a file which implements a reflexive
     compile-before-load performance constraint."))
          

(defclass cl-source-file (source-file)
  ((type :initform "lisp" :allocation :class)))
          
(defclass c-source-file (source-file)
  ((type :initform "c" :allocation :class)))

(defclass java-source-file (source-file)
  ((type :initform "java" :allocation :class)))

(defclass static-file (file) ())

(defclass doc-file (static-file) ())

(defclass html-file (doc-file)
  ((type :initform "html" :allocation :class)))


(defclass operation (class-context)
  ((forced :initform nil :initarg :force :accessor operation-forced)
   (original-initargs :initform nil :initarg :original-initargs
                      :accessor operation-original-initargs)
   (visited-nodes :initform nil :accessor operation-visited-nodes)
   (visiting-nodes :initform nil :accessor operation-visiting-nodes)
   (parent
    :initform nil :initarg :parent
    :accessor operation-parent :reader context-parent)
   (settings :initform nil :initarg :settings :accessor operation-settings)
   (failed-error-class :initform 'operation-failed :allocation :class
                       :reader operation-failed-error-class)
   (warned-error-class :initform 'operation-warned :allocation :class
                       :reader operation-warned-error-class)
   (missing-dependency-error-class :initform 'missing-dependency :allocation :class
                       :reader operation-missing-dependency-error-class)
   (missing-constituent-error-class :initform 'missing-constituent :allocation :class
                       :reader operation-missing-constituent-error-class)
   (time
    :initarg :time :initform (get-universal-time)
    :reader operation-time
    :documentation "The universal time at which the operation - or
     the respective parent operation, was created.")
   (:cycle
    :initform (gensym "OP-CYCLE-") :initarg :cycle
    :reader operation-cycle)
   (classes
    :initform () :allocation :class))
  (:documentation "An operation embodies the logic to perform some task.
 When operate is called with an operation name, a new one is instantiated with
 the given arguments and used to traverse the given component graph to carry
 out the tasks."))


(defclass compile-op (operation)
  ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
   (flags :initarg :system-p :accessor compile-op-flags :initform nil)
   (warned-error-class :initform 'compile-warned :allocation :class)
   (failed-error-class :initform 'compile-failed :allocation :class)))


(defclass basic-load-op (operation) ())

(defclass feature-op (operation) ())

(defclass load-op (basic-load-op) ())

(defclass load-source-op (basic-load-op) ())

(defclass test-op (operation) ())

;; could be used for features, but not now
(defclass state-op (operation)
  ((predicate
     :initform (error "PREDICATE is required.") :initarg :predicate
     :reader operation-predicate
     :documentation "The operator's predicate is a function of two arguments,
      the operation and the constrained value - a component or a feature,
      which returns true iff the test is satisfied.")))

(defstruct (operation-tag (:type list)) cycle time)

;;;; ------------------------------------------------------------------------
;;;; methods: component

(defmethod print-object ((c component) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (prin1 (when (slot-boundp c 'name) (component-name c)) stream)))


;;; the initialization protocol  recognizes three paths:
;;; - initialize : delegate for dependencies and methods 
;;; - reinitialize : delegate for dependencies and methods
;;; - shared-initialize : reset all 'version-specific' slots
;;; The distinction is to allow that class changes affect neither methods
;;; nor dependencies, but to update anything which can contingent on the
;;; presences of a specific initarg and to unbind slots which are computed
;;; on demand.
;;; The method keywords (see +asdf-methods+) are just declared, and left to
;;; %define-component-inline-methods to process.
;;; The dependency keywords are just declared, and left to
;;; %refresh-component-dependencies to process.
;;; Should others be necessary, the specialized component class must define
;;; an initialization method which declares the respective key argument.
;;; 

(defmethod shared-initialize ((component component) (slots t)
                              &key
                              (force nil f-s)
                              perform explain output-files operation-done-p   ; permits method
                              depends-on weakly-depends-on strongly-depends-on
                              in-order-to do-first         ; permits dependencies
                              pathname
                              missing-behaviour failure-behaviour warning-behaviour)
  "coerce the name and organize settings, canonicalize pathnames"
   (declare (ignore perform explain output-files operation-done-p
                   depends-on weakly-depends-on in-order-to do-first strongly-depends-on
                   pathname))
  (slot-makunbound component 'constituency-requirements)       ; computed on-demand
  (slot-makunbound component 'dependency-requirements)          ; computed on-demand
  (call-next-method)
  (when f-s (setf (getf (component-settings component) :force) force))
  (when missing-behaviour (setf (getf (component-settings component) :missing-behaviour) missing-behaviour))
  (when failure-behaviour (setf (getf (component-settings component) :failure-behaviour) failure-behaviour))
  (when warning-behaviour (setf (getf (component-settings component) :warning-behaviour) warning-behaviour)))


(defgeneric initialize-component (component initargs)
  (:method ((component component) initargs)
    (destructuring-bind (&key name pathname &allow-other-keys) initargs
      (setf (component-name component) (setf name (coerce-name name)))
      ;; as there is yet no connection to a parent, set the two pathname values
      ;; based on the features intirisic to the initialization arguments and leave
      ;; complete resolution to the eventual later reference.
      (multiple-value-bind (pathname relative-pathname)
                           (compute-component-pathnames component pathname name)
        (setf-component-pathname pathname component)
        (setf-component-relative-pathname relative-pathname component)))
    (setf (component-dependencies component)
          (collect-component-dependencies component initargs))
    (%remove-component-inline-methods component)
    (%define-component-inline-methods component initargs)))


(defmethod initialize-instance ((component component) &rest initargs)
  (declare (dynamic-extent initargs))
  (initialize-component component initargs)
  (call-next-method))


(defmethod reinitialize-instance ((component component) &rest initargs)
  (declare (dynamic-extent initargs))
  (initialize-component component initargs)
  (map nil #'%remove-component-inline-methods
       (component-unreinitialized-constituents component))
  (setf (component-unreinitialized-constituents component)
        (component-constituents component))
  (setf (component-constituents component) nil)
  component)


(defun %remove-component-inline-methods (component)
  "Clear component-specific methods, as reflected in it's cache."
  (when (slot-boundp component 'inline-methods)
    (loop for (gf-name method) in (slot-value component 'inline-methods)
          when (fboundp gf-name)        ; who knows, it could disappear?
          do (remove-method (symbol-function gf-name) method))
    (slot-makunbound component 'inline-methods)))
;; (trace %remove-component-inline-methods)

(defun %define-component-inline-methods (component initargs)
  (let ((names (component-inline-method-names component)))
    (setf (slot-value component 'inline-methods)
          (loop for (keyword value) on initargs by #'cddr
                for name = (find keyword names :test #'string-equal)
                when name
                collect (list name (funcall value component))))))
;(trace %define-component-inline-methods)

(defun collect-component-dependencies (component initargs)
  (let ((dependencies ()))
    (dolist (requirement (component-requirement-names component))
      (loop for (key value) on initargs by #'cddr
            when (string-equal requirement key)
            do (setf dependencies (nconc dependencies (list key value)))))
    dependencies))

(defgeneric component-constituency-requirements (component)
  (:method ((component component))
    (if (slot-boundp component 'constituency-requirements)
      (get-component-constituency-requirements component)
      (setf (component-constituency-requirements component)
            (compute-constituency-requirements component (component-constituents component))))))

(defgeneric component-dependency-requirements (component)
  (:method ((component component))
    (if (slot-boundp component 'dependency-requirements)
      (get-component-dependency-requirements component)
      (setf (component-dependency-requirements component)
            (compute-dependency-requirements component (component-dependencies component))))))

(defun compute-dependency-requirements (component initargs)
  "Construct the requirement links by combining
   - reflexive requirements : each element in each chain expands to and
     independent requirement
   - the first do-first requirement : if any
   - all argument which correspond to requirement names."

  (let ((requirements ()))
    ;; for each operation chain, expand the combination into requirements
    (dolist (constraint (component-reflexive-operations component))
      (loop for (op required-op) on constraint
            when required-op
            do (setf requirements
                     (append-component-requirement requirements op
                                                   `(,required-op ,component)))))
    ;; then add other requirements, in apparent order
    (let ((names (component-requirement-names component))
          (t-ops (component-transitive-operations component)))
      (loop for (keyword value) on initargs by #'cddr
            for name = (find keyword names :test #'string-equal)
            when  name
            do (setf requirements
                     (append requirements (canonicalize-requirement name value t-ops)))))
    requirements))

(defgeneric compute-constituency-requirements (component constituents)
  (:method ((component component) (constituents cons))
    `((t (t ,@constituents))))
  (:method ((component component) (constituents null))
    nil)

  (:method ((component file) (constituents list))
    nil))



(defgeneric append-component-requirement (context operation requirement)
  
  (:method ((context list) op requirement)
    "Destructively modify a requirements list to add a requirement for a given
     operation. Check for existing operator or respective required operator
     entries to extend. Otherwise add a new entry."
    (destructuring-bind (required-op &rest required-components) requirement
      (let* ((op-entry (assoc op context))
             (requirement-entry (assoc required-op (rest op-entry))))
        (cond (requirement-entry
               (setf (rest (last requirement-entry)) (copy-list required-components)))
              (op-entry
               (setf (rest (last op-entry)) (list (copy-list requirement))))
              ((consp context)
               (setf (rest (last context)) (list (list op (copy-list requirement)))))
              (t
               (setf context (list (list op (copy-list requirement))))))
        context))))

(defgeneric canonicalize-requirement (name requirements transitive-operations)
  (:method ((name t) (constituents null) (t-ops t))
    nil)

  (:method ((name (eql :depends-on)) (requirements cons) (t-ops list))
    (loop for t-op-chain in t-ops
          append
          (loop for (t-op required-op) on t-op-chain
                when required-op
                append `((,t-op ,@(mapcar #'(lambda (requirement)
                                               (typecase requirement
                                                 (cons 
                                                  (destructuring-bind (op . arguments) requirement
                                                    (case (find op '(:feature :version) :test #'string-equal)
                                                      (:feature
                                                       `(feature-op ,@arguments))
                                                      (:version
                                                       `(t ,(first arguments) :version ,@(rest arguments)))
                                                      (t      ; ? fail this if the ops don't match
                                                       (unless (subtypep op required-op)
                                                         (warn "Required operators not matched: ~s -> ~s !> ~s."
                                                               t-op required-op op))
                                                       `(,op ,requirement)))))
                                                 (t
                                                  `(,required-op ,requirement))))
                                           requirements))))))

  (:method ((name (eql :weakly-depends-on)) (requirements cons) (t-ops t))
    `((t (:missing-behaviour :ignore)
         ,@(rest (first (canonicalize-requirement :depends-on requirements t-ops))))))

  (:method ((name (eql :strongly-depends-on)) (requirements cons) (t-ops t))
    `((t (:success-behaviour :force)
         ,@(rest (first (canonicalize-requirement :depends-on requirements t-ops))))))

  (:method ((name (eql :do-first)) (requirements list) (t-ops t))
    requirements)

  (:method ((name (eql :in-order-to)) (requirements list) (t-ops t))
    requirements))


(defmethod perform ((operation operation) (c component))
  "The base method for a component succeeds"
  *status-succeeded*)

(defun component-operation-time (component operation)
  (operation-tag-time (component-operation-tag component (type-of operation))))

(defun (setf component-operation-time) (time component operation)
  (setf (operation-tag-time (component-operation-tag component (type-of operation)))
        time))


(defun component-operation-cycle (component operation)
  (operation-tag-cycle (component-operation-tag component (type-of operation))))

(defun (setf component-operation-cycle) (cycle component operation)
  (setf (operation-tag-cycle (component-operation-tag component (type-of operation)))
        cycle))


(defgeneric component-operation-tag (component operation)
  (:method ((component component) (operation operation))
    (component-operation-tag component (type-of operation)))
  (:method ((component component) (operation symbol))
    (or (gethash operation (component-operation-tags component))
        (setf (gethash operation (component-operation-tags component))
              (make-operation-tag :time most-negative-fixnum :cycle nil)))))

(defgeneric (setf component-operation-tag) (tag component operation)
  (:method (tag (component component) (operation operation))
    (setf (component-operation-tag component (type-of operation)) tag))
  (:method (tag (component component) (operation symbol))
    (setf (gethash operation (component-operation-tags component))
          tag)))


(defmethod component-system ((component component))
  (component-system (component-parent component)))

#+(or )
(defmethod component-pathname ((component component))
  "The value should be set when the component is combined with a group. This logic
 also permits that to be defered - if the group location is unknown, or the system root
 is moved."
  (or (get-component-pathname component)
      (setf-component-pathname (merge-pathnames (component-relative-pathname component)
                                                (component-pathname (component-parent component)))
                               component)))

(defmethod component-pathname ((component component))
  "The value should be set when the component is combined with a group. This logic
 also permits that to be defered - if the group location is unknown, or the system root
 is moved."
  (or (get-component-pathname component)
      (setf-component-pathname
       (let ((parent (component-pathname (component-parent component)))
             (relative (component-relative-pathname component)))
         (merge-pathnames (make-pathname :directory (append (pathname-directory parent)
                                                            (rest (pathname-directory relative)))
                                         :defaults relative)
                          parent))
       component)))

(defgeneric compute-component-pathnames (component pathname name)
  (:method ((component component) (pathname string) (name string))
    (let ((*default-pathname-defaults* +null-pathname+))
      (setf pathname
            (cond ((equal pathname ".")
                   (make-pathname :directory '(:relative) :name nil :type nil))
                  ((equal pathname "..")
                   (make-pathname :directory '(:relative :up) :name nil :type nil))
                  (t
                   (pathname pathname))))
      (compute-component-pathnames component pathname name)))
  
  (:method ((component component) (pathname pathname) (name string))
    (ecase (first (pathname-directory pathname))
      ((nil)
       (let ((*default-pathname-defaults* +null-pathname+))
         (values nil (make-pathname :directory `(:relative ,(pathname-name pathname)) :name nil :type nil))))
      (:absolute (values pathname nil))
      (:relative (values nil pathname))))

  ;; this is the wrong place to specialize. file would be more correct,
  ;; but existing tests expect component to support this
  (:method ((component component) (pathname null) (name string))
    (let ((*default-pathname-defaults* +null-pathname+))
      (values nil (make-pathname :directory '(:relative) :name name))))

  (:method ((component source-file) (pathname null) (name string))
    (let ((*default-pathname-defaults* +null-pathname+))
      (values nil (make-pathname :directory '(:relative) :name name
                                 :type (source-file-type component)))))

  (:method ((component group) (pathame null) (name string))
    (let ((*default-pathname-defaults* +null-pathname+))
      (values nil (make-pathname :directory `(:relative ,name) :name nil :type nil)))))

(defmethod component-relative-pathname ((component component))
  (get-component-relative-pathname component))




    
  

(defmethod component-property ((c component) property)
  (cdr (assoc property (slot-value c 'properties) :test #'equal)))

(defmethod (setf component-property) (new-value (c component) property)
  (let ((a (assoc property (slot-value c 'properties) :test #'equal)))
    (if a
        (setf (cdr a) new-value)
        (prog1 new-value
          (setf (component-properties c)
                (acons property new-value (slot-value c 'properties)))))))


(defmethod version-satisfies ((c component) version)
  (unless (and version (component-version c))
    (return-from version-satisfies t))
  (let ((x (mapcar #'parse-integer
                   (split (component-version c) nil '(#\.))))
        (y (mapcar #'parse-integer
                   (split version nil '(#\.)))))
    (labels ((bigger (x y)
               (cond ((not y) t)
                     ((not x) nil)
                     ((> (car x) (car y)) t)
                     ((= (car x) (car y))
                      (bigger (cdr x) (cdr y))))))
      (and (= (car x) (car y))
           (or (not (cdr y)) (bigger (cdr x) (cdr y)))))))


;;;;
;;;; Finding components


(defmethod find-component ((component component) (name-path cons) &optional version)
  (ecase (first name-path)
    (:global (find-component nil name-path version))
    (:absolute (find-component (component-system component) (cons :relative (rest name-path)) version))
    (:relative
     (cond ((equal (second name-path) "..")
            (find-component (component-parent component) (cons :relative (cddr name-path)) version))
           ((equal (second name-path) ".")
            (when (version-satisfies component version) component))
           ((and (null (cdr name-path))
                 (version-satisfies component version))
            component)))))

(defmethod find-component ((group group) (name-path cons) &optional version)
  (or (call-next-method)
      (case (first name-path)
        (:relative
         (and (cdr name-path)
              (let ((sub-path (cons :relative (cddr name-path)))
                    (sub-name (second name-path)))
                (some #'(lambda (constituent)
                          (and (equal sub-name (component-name constituent))
                               (find-component constituent sub-path version)))
                      (group-constituents group))))))))


(defmethod find-component ((module (eql nil)) (name-path cons) &optional version)
  "Given a null context, that is, a component with no parent, look for a system"
  (case (first name-path)
    (:global (let ((s (registered-system (second name-path))))
               (when (and s (version-satisfies s version))
                 (find-component s (cons :relative (cddr name-path)) version))))))

(defmethod find-component ((module (eql nil)) (name string) &optional version)
  (if (find *component-punctuation-character* name)
    (find-component nil (split-component-namestring name) version)
    (find-component nil `(:global ,name) version)))
                 

(defmethod find-component ((context t) (name string) &optional version)
  (if (find *component-punctuation-character* name)
    (find-component context (split-component-namestring name) version)
    (find-component context `(:relative ,name) version)))

(defmethod find-component ((context t) (name symbol) &optional version)
  (find-component context (coerce-name name) version))




;;;; ------------------------------------------------------------------------
;;;; Methods: group


(defmethod shared-initialize ((instance group) (slots t) &key default-component-class
                              (if-component-dep-fails nil))
  (call-next-method)
  (when if-component-dep-fails
    (setf (component-if-component-dep-fails instance) if-component-dep-fails))
  (when default-component-class
    (setf (context-classes instance)
          (acons :file default-component-class (context-classes instance)))))


(defgeneric component-if-component-dep-fails (group)
  (:documentation "Indicates what to do if we can't satisfy a requirement of one
     of this module's components. This allows a limited form of conditional processing.
     The alternatives are 
     - fail : causes this component's performance to failif a constituent's fails
     - try-next : skips over the constituent failure to perform on the next constituent
     - ignore : (? CVS: sbcl/contrib/sb-rotate-byte sb-rotate-byte.asd was the only
       visible use-case. it reads as if the expected effect is the same as try-next)")

  (:method ((group group))
    (ecase (getf (component-settings group) :failure-behaviour :error)
      (:error :fail)
      (:continue :try-next)
      (:ignore :ignore))))

(defgeneric (setf component-if-component-dep-fails) (mode group)
  (:method (value (group group))
    (setf value (ecase value
                  ((:fail :error) :error)
                  ((:try-next :continue) :continue)
                  ((:ignore) :ignore)))
    (setf (getf (component-settings group) :failure-behaviour)
          value)
    (setf (getf (component-settings group) :missing-behaviour)
          value)))


(defmethod operation-done-p ((o operation) (c group))
  "In addition to the base tests, all group constituents must be utd."
  (and (call-next-method)
       (dolist (constituent (component-constituents c) t)
         (unless (operation-done-p o constituent) (return nil)))))

;;;; ------------------------------------------------------------------------
;;;; Methods: static file

(defmethod perform ((operation operation) (c static-file))
  "Any operation on a static file is always skipped."
  *status-skipped*)

(defmethod output-files ((operation operation) (c static-file))
  "Any static file has no output files."
  nil)

(defmethod input-files ((op operation) (c static-file))
  "Any static file has no input files."
  nil)

(defmethod operation-done-p ((operation operation) (c static-file))
  "Any operation on any static file is always complete."
  t)



;;;; ------------------------------------------------------------------------
;;;; Methods: system



(defmethod operate ((operation operation) (system t) &rest args)
  (let ((result (apply #'operate operation (find-system system) args)))
    (values (performance-complete-p result)
            result)))

(defmethod component-system ((system system))
  system)

(defgeneric compute-system-pathnames (system pathname source-file name)
  (:method ((system system) pathname source-file name)
    (unless source-file
      (setf name (coerce-name name))
      (setf source-file
            (cond ((and pathname
                        (probe-file (make-pathname :name name :type *sysdef-file-type*
                                                   :defaults pathname))))
                  (*load-pathname*
                   (if *resolve-symlinks*
                     (resolve-symlinks *load-truename*)
                     *load-pathname*))
                  ((and *default-pathname-defaults*
                        (probe-file (make-pathname :name name :type *sysdef-file-type*
                                                   :defaults *default-pathname-defaults*))))
                  ((search-for-definition-pathname name))
                  (t
                   nil))))
    (unless pathname
      (setf pathname
            (cond (source-file
                   (pathname-sans-name+type source-file))
                  (t
                   (make-pathname :directory '(:absolute) :defaults +null-pathname+)))))
    (values pathname source-file)))


(defmethod compute-component-pathnames ((system system) (pathname pathname) (name string))
    (ecase (first (pathname-directory pathname))
      (:absolute (values pathname nil))
      (:relative (warn "Relative system pathname: ~s." pathname)
                 (values pathname nil))))

(defmethod initialize-instance ((system system) &rest initargs
                                  &key pathname source-file name)
  "new and reinitialized systems reconcile the given component pathname
 and system source-file."
  (declare (dynamic-extent initargs))
  (multiple-value-setq (pathname source-file)
    (compute-system-pathnames system pathname source-file name))
                          
  (apply #'call-next-method system
         :pathname pathname
         :source-file source-file
         initargs))

(defmethod reinitialize-instance ((system system) &rest initargs
                                  &key pathname source-file name)
  "new and reinitialized systems reconcile the given component pathname
 and system source-file."
  (declare (dynamic-extent initargs))
  (multiple-value-setq (pathname source-file)
    (compute-system-pathnames system pathname source-file name))
                          
  (apply #'call-next-method system
         :pathname pathname
         :source-file source-file
         initargs))

(defmethod operate ((operation operation) (system system) &key (verbose t) version)
  (unless (version-satisfies system version)
    (error 'missing-system :requires system :version version))
  (let* ((*package* *package*)
         (*readtable* *readtable*)
         (*verbose-out* (if verbose *standard-output* (make-broadcast-stream)))
         (settings (operation-settings operation)))
    (flet ((do-operation ()
             (with-compilation-unit ()
               (perform operation system))))
      (declare (dynamic-extent #'do-operation))
      (if settings
        (apply #'call-with-settings #'do-operation settings)
        (do-operation))))
  operation)


;;;;
;;;; Finding systems
;;;;
;;;; Three resolution operators and two definition location pathnames contribute
;;;; to the processes of finding a ststem instance. The operators are:
;;;; - registered-system
;;;;   returns an already registered instance by name, or nil if none is present
;;;; - find-system 
;;;;   given a name, evaluates the definition pathname yielded by searching,
;;;;   against any extand system's source file and/or load time to either
;;;;   return that system as up-to-date, or load a newone from the definition.
;;;; - registered-system-p : returns a boolean based on the registered-system
;;;;                         result
;;;; The locations are
;;;; - system-source-file : the value bound in the instance
;;;; - system-definition-pathname : in general, the value yielded by searching


(defun system-registered-p (name)
  (not (null (registered-system name))))

(defun register-system (name system)
  (asdf-message "~&~@<; ~@;registering ~A as ~A~@:>~%" system name)
  (setf (gethash (coerce-name name) *defined-systems*)
        system))

(defun registered-system (name)
  (gethash (coerce-name name) *defined-systems*))


(defun map-systems (fn)
  "Apply `fn` to each defined system.

`fn` should be a function of one argument. It will be
called with an object of type `asdf:system`."
  (maphash (lambda (_ datum)
             (declare (ignore _))
             (destructuring-bind (_ . def) datum
               (declare (ignore _))
               (funcall fn def)))
           *defined-systems*))


(defgeneric system-definition-pathname (system-name)
  (:documentation "Given a system name, first, search for the respective
 definition file pathname and return if found. Given a system, returns the
 source file for an extant system.")

  (:method ((system-name t))
    (system-definition-pathname (coerce-name system-name)))

  (:method ((system-name string))
    (search-for-definition-pathname system-name))

  (:method ((system system))
    (system-source-file system)))


(defun search-for-definition-pathname (name &key ((:file-type *sysdef-file-type*) *sysdef-file-type*))
  "Given a name and file type, apply definition search functions until one succeeds at locating
 such a file.
 NAME : string : the system, component type, or constraint type name
 FILE-TYPE : string : the file type. (default *sysdef-file-type*, which defaults to \"asd\".
 Returns the pathname for a matched file, or nil is none is found."
  (some (lambda (x) (funcall x name))
        *system-definition-search-functions*))


(defun sysdef-central-registry-search (system)
  (let ((name (coerce-name system))
        (to-remove nil)
        (to-replace nil))
    (block nil
      (unwind-protect
           (dolist (dir *central-registry*)
             (let ((defaults (eval dir)))
               (when defaults
                 (cond ((directory-pathname-p defaults)
                        (let ((file (and defaults
                                         (make-pathname
                                          :defaults defaults :version :newest
                                          :name name :type *sysdef-file-type* :case :local)))
                               #+(and (or win32 windows) (not :clisp))
                               (shortcut (make-pathname
                                          :defaults defaults :version :newest
                                          :name name :type "asd.lnk" :case :local)))
                          (if (and file (probe-file file))
                              (return file))
                          #+(and (or win32 windows) (not :clisp))
                          (when (probe-file shortcut)
                            (let ((target (parse-windows-shortcut shortcut)))
                              (when target
                                (return (pathname target)))))))
                       (t
                        (restart-case
                            (let* ((*print-circle* nil)
                                   (message
                                    (format nil
                                            "~@<While searching for system `~a`: `~a` evaluated ~
to `~a` which is not a directory.~@:>"
                                            system dir defaults)))
                              (error message))
                          (remove-entry-from-registry ()
                            :report "Remove entry from *central-registry* and continue"
                            (push dir to-remove))
                          (coerce-entry-to-directory ()
                            :report (lambda (s)
                                      (format s "Coerce entry to ~a, replace ~a and continue."
                                              (ensure-directory-pathname defaults) dir))
                            (push (cons dir (ensure-directory-pathname defaults)) to-replace))))))))
        ;; cleanup
        (dolist (dir to-remove)
          (setf *central-registry* (remove dir *central-registry*)))
        (dolist (pair to-replace)
          (let* ((current (car pair))
                 (new (cdr pair))
                 (position (position current *central-registry*)))
            (setf *central-registry*
                  (append (subseq *central-registry* 0 position)
                          (list new)
                          (subseq *central-registry* (1+ position))))))))))


(defun find-system (name &optional (error-p t))
  "Given an system name, if a path is known for the definition, then if no
 system is registered, or if the extant instance is out of date, load it from
 its definition path. If that succeeded, return the system. If no path is
 known, but a system instance exists, return that. Otherwise return nil or
 signal an error as specified by error-p."

  (let* ((registered-system (registered-system name))
         (definition-pathname (system-definition-pathname name)))
    (or (if definition-pathname
          (if (and registered-system
                   (equalp definition-pathname (system-source-file registered-system))
                   (operation-done-p 'load-op registered-system))
            registered-system
            (when (and (load-system-definition definition-pathname)
                       (setf registered-system (registered-system name))
                       (equalp definition-pathname (system-source-file registered-system)))
              registered-system))
          registered-system)
        (when error-p
          (error 'missing-system :requires name)))))


;;;; -------------------------------------------------------------------------
;;;; Methods : operations

(defmethod shared-initialize ((operation operation) (slots t) &rest initargs
                              &key
                              (force nil f-s)
                              (on-failure nil of-s)
                              (on-warnings nil ows-s) (on-warning on-warnings ow-s)
                              (on-success nil os-s)
                              parent (time (if parent (operation-time parent) (get-universal-time))))
  "just coerce the name and organize settings"
  (apply #'call-next-method operation slots
         :time time
         initargs)
  (when f-s (setf (getf (operation-settings operation) :force) force))
  (when of-s
    (assert (typep on-failure 'failure-behaviour) ()
            "Invalid failure behaviour: ~s." on-failure)
    (setf (getf (operation-settings operation) :failure-behaviour) on-failure))
  (when (or ow-s ows-s)
    (assert (typep on-warnings 'warning-behaviour) ()
            "Invalid warning behaviour: ~s." on-warnings)
    (setf (getf (operation-settings operation) :warning-behaviour) on-warning))
  (when os-s
    (assert (typep on-success 'success-behaviour) ()
            "Invalid success behaviour: ~s." on-success)
    (setf (getf (operation-settings operation) :success-behaviour) on-success)))


(defmethod print-object ((o operation) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (ignore-errors
      (prin1 (operation-original-initargs o) stream))))


(defun node-for (o c)
  (cons (class-name (class-of o)) c))

(defmethod operation-ancestor ((operation operation))
  (aif (operation-parent operation)
       (operation-ancestor it)
       operation))


;; no explanation why only system boundaries modulate force-p
(defun make-sub-operation (operation type)
  "Given a wild-card for the required operation, replicate the current one.
 Otherwise reuse or recreate depending on the type and whether force is
 at issuse."
  
  (if (eq type t)
    (setf type (type-of operation))
    (assert (subtypep type 'operation) ()
            "Invalid relation operation: ~s." type))
  (let* ((args (operation-original-initargs operation))
         (force-p (getf args :force *operation-force*)))
    ;; note explicit comparison with T: any other non-NIL force value
    ;; (e.g. :recursive) will pass through
    (cond ((eql force-p t)
           (apply #'make-instance (context-find-class operation type)
                  :parent operation :force nil
                  :time (operation-time operation)
                  :cycle (operation-cycle operation)
                  :original-initargs args args))
          ((subtypep (type-of operation) type)
           operation)
          (t
           (apply #'make-instance (context-find-class operation type)
                  :time (operation-time operation)
                  :cycle (operation-cycle operation)
                  :parent operation :original-initargs args args)))))



(defmethod component-depends-on ((operation operation) (c component))
  (component-depends-on (type-of operation) c))

(defmethod component-depends-on ((operation symbol) (component component))
  (let ((operation-type (type-of operation)))
    (labels ((ensure-name (component)
               (etypecase component
                 (string component)
                 (symbol (string component))
                 (component (component-name component))
                 (cons (ensure-name (second component))))))
      (loop for (operation . requirements) in (component-dependency-requirements component)
            when (subtypep operation-type operation)
            append (loop for requirement in requirements
                     unless (find requirement all-requirements :test #'equalp)
                     collect requirement)
            into all-requirements
            finally (return all-requirements)))))


(defmethod input-files ((operation operation) (component component))
  (rest (or (assoc (type-of operation) (component-input-files component))
            (first (setf (component-input-files component)
                         (acons (type-of operation)
                                (compute-operation-component-input-files operation component)
                                (component-input-files component)))))))

;;; the previous implementation of input-files / component-self-dependencies
;;; read as if all dependencies were examined, and everything was incorporated
;;; which included the component among the required components.
;;; yet, the destructuring permitted just one name in the requirement,
;;; which it proceeded to resolve in the parent, but, given the initial
;;; constraint in c-s-d, the rseult cold be only the component itself.
;;; so, instead, construct the input files from the reflexive requirements directly...

(defgeneric compute-operation-component-input-files (operation component)
  (:method ((operation operation) (component component))
    (or (loop for r-op in (mapcar #'first (component-reflexive-operations component))
              append (output-files (make-sub-operation operation r-op)
                                   component))
        ;; no reflexive operations needed?  I guess we work with the
        ;; original source file, then
        (list (component-pathname component)))))


(defmethod input-files ((operation operation) (c group)) nil)

(defmethod output-files ((o operation) (c component)) nil)


(defmethod operation-done-p ((o operation) (c component))
  ;; the component must be u-t-d  and all required components
  ;; for the op must be u-t-d
  (let ((predecessors (compute-operation-predecessors o c)))
    (or
     ;; either, already performed this operation on this cycle, or
     (eq (component-operation-cycle c o) (operation-cycle o))
     ;; not forced, and all predecessors are done, and internal state is done
     (and
      (not *operation-force*)
      (or (null predecessors)
          ;; given required components, in order to
          (every #'(lambda (p)
                     (let* ((pc (find-component (component-parent c) p))
                           (pc-dp (and pc (operation-done-p o pc)))
                           (p-cycle (and pc (component-operation-cycle pc o)))
                           (o-cycle (operation-cycle o))
                           (ceq (eq p-cycle o-cycle)))
                       (and pc-dp (not ceq))))
                 predecessors))
      (operation-files-done-p o c)))))


;;; this factors the file aspects of the about out for eventual specialization on
;;; source-file or file

(defgeneric operation-files-done-p (operation component)
  (:method ((o operation) (c component))
    (let ((out-files (output-files o c))
          (in-files (input-files o c))
          (component-time (component-operation-time c o)))
      (cond ((and (not in-files) (not out-files))
             ;; arbitrary decision: an operation that uses nothing to
             ;; produce nothing probably isn't doing much
             t)
            ((not out-files)
             (> component-time
                (reduce #'max in-files :key #'safe-file-write-date )))
            ((not in-files) nil)
            (t (and
                (every #'probe-file out-files)
                (> (apply #'min (mapcar #'safe-file-write-date out-files))
                   (apply #'max (mapcar #'safe-file-write-date in-files)))))))))



;;;; -------------------------------------------------------------------------
;;;; perform : the generic requirement propagation operator


;;; the original traverse description:
;;;  "Given an operation and a component, determine whether it needs to be performed. If so
;;; carry it out and record the results. This approach combines the graph walk and the action
;;; for three reasons:
;;; 1. To allow that the system states and/or environment change during traversal.
;;; 2. To allow to establish restarts on groups
;;; 3. To recognize the contingencies for duplicated components contemporaneously rather than a-priori.
;;;
;;; the existing : 
;;; - went through prcedecessors to collect them - with contingencies for version and features.
;;; - went through constituents to collect them
;;; - ignored itself"
;;;
;;; this version implements traversal as a simple graph walk in serveral methods of the perform
;;; operator. the combination allows three qualifiers specific to this process
;;; - constituency  : used to traverse the node and its constituents in/post-order
;;; - restart : wraps around with error restarts for restartable nodes
;;; - dependency : wraps restarts, around with a check for previous completion and predecessor operations
;;;



(defmethod perform ((operation operation) (c source-file))
  "The base method for an operation on a source-file signals an error, to indicate
 the missing implementation."
  (sysdef-error
   "~@<required method PERFORM not implemented ~
    for operation ~A, component ~A~@:>"
   (class-of operation) (class-of c)))

  

(defmethod perform restart ((operation operation) (component restartable-component))
  "The restart method for an abstract group operation establishes handlers for
 - retry : restarts the next method to try the operation on the entire group
 - backtrack : try one of the self-predecessors
 - accept : marks the operation as completed and returns as it it had succeeded."
  (let* ((self-ops nil))
    (labels ((self-ops ()
               (or self-ops
                   (setf self-ops
                         (dolist (op-list (component-reflexive-operations component)
                                          (sort self-ops #'string-lessp))
                           (dolist (op op-list)
                             (pushnew op self-ops)))))))

      ;; set up restarts for this operation and all other self-operations
      ;; upon which this one depended
      (loop
        (restart-case (return (call-next-method))
          (retry () :report  (lambda (s)
                               (format s "~@<Retry performing ~S on ~S.~@:>"
                                       (type-of operation) component)))
          (backtrack (operation) :report (lambda (s)
                                           (format s "~@<Specify an operation from ~S for ~S.~@:>"
                                                   (self-ops) component))
                     (assert (member operation self-ops) () "Invalid operation.")
                     (perform operation component))
          (accept () :report (lambda (s)
                               (format s "~@<Continue, treating ~S on ~S as ~
                                          having been successful.~@:>"
                                       operation component))
                  (setf (component-operation-time component operation) (get-universal-time)
                        (component-operation-cycle component operation) (operation-cycle operation))
                  (return (values *status-succeeded* operation))))))))


;; features, weakly, and contingently, absolutely.


(defmethod perform dependency ((operation operation) (component component))
  "Interpret a component's dependencies - requirements and constituency.
 Check first for circularity and for completeness and skip or abort if
 appropriate. Performance proceeds, as per the current `*traversal-order*`,
 to perform the reauored 'sub operation' on the related component:
 - the component combines with the operation
 - constituents combine as declared for each constituent link
 - requirements combine as declared for each dependency link
 For the latter two, the link annotations may include predicates which
 prune the traversal."
  
  (let* ((operation-type (type-of operation))
         (tag (cons operation-type component))
         (performance-status *status-skipped*)
         (settings (if (component-settings component)
                     (append (component-settings component) (operation-settings operation))
                     (component-settings component))))
    (declare (dynamic-extent tag))
    (labels ((traverse (&optional (relation *traversal-order*))
               (etypecase relation
                 (cons (map nil #'traverse relation))
                 (symbol
                  (ecase relation
                    (:preorder (traverse '(:component :requirements :constituents)))
                    (:postorder (traverse '(:requirements :constituents :component)))
                    (:inorder (traverse '(:requirements :component :constituents)))
                    (:dependency (traverse '(:requirements :constituents)))
                    (:component (perform-component))
                    (:constituents (perform-constituents))
                    (:requirements (perform-requirements))))))
             (combine-status (new)
               (typecase new
                 (performance-complete (setf performance-status new))
                 (performance-status )
                 (t                     ; treat as success
                  (setf performance-status *status-succeeded*))))

             (perform-traversal (traversal-operator requirements)
               (loop for (r-operation . requirements) in requirements
                     when (subtypep operation-type r-operation)
                     do (let ((saved-settings ()))
                          (unwind-protect
                            (loop for (required-operation . arguments) in requirements
                                  do (etypecase required-operation
                                       (keyword (setf saved-settings
                                                      (list* required-operation (shiftf (setting required-operation) (first arguments))
                                                             saved-settings)))
                                       (symbol
                                        (dolist (required-component arguments)
                                          (multiple-value-bind (component-operation required-component)
                                                               (etypecase required-component
                                                                 ((or component string symbol)
                                                                  (values (make-sub-operation operation required-operation)
                                                                          required-component))
                                                                 (cons
                                                                  (values (apply #'make-sub-operation operation required-operation
                                                                                 (rest required-component))
                                                                          (first required-component))))
                                            (multiple-value-bind (requirement-status effective-component)
                                                                 (funcall traversal-operator operation component
                                                                          required-operation required-component)
                                              (etypecase (setf requirement-status
                                                               (compute-performance-status component-operation effective-component requirement-status))
                                                (status-aborted
                                                 (return-from perform (values requirement-status effective-component)))
                                                (performance-failed
                                                 (return-from perform (values requirement-status effective-component)))
                                                (performance-status (combine-status requirement-status)))))))))
                            (loop for (setting value) on saved-settings by #'cddr
                                  do (setf (setting setting) value))))))

             (perform-constituents ()
               (setq *operation-success-behaviour* :continue)
               (perform-traversal #'perform-constituent (component-constituency-requirements component)))

             (perform-requirements ()
               (setq *operation-success-behaviour* :continue)
               (perform-traversal #'perform-requirement (component-dependency-requirements component)))

             (perform-component ()
               (multiple-value-bind (requirement-status effective-component)
                                    (call-next-method)
                 (declare (ignore effective-component))
                 (combine-status requirement-status))))
      #+(or )
      (print (list (type-of operation) component 
                   :done-p (operation-done-p operation component)
                   :force *operation-force*
                   :c-o-tag (component-operation-tag component operation)
                   :o-t (operation-time operation)))
      (cond ((find tag *operation-stack* :test #'equal)
             (setf performance-status *status-circular*))
            ((operation-done-p operation component)
             ;; if the operation is already complete - modulo forcing, indicate (yes no)
             (setf performance-status *status-skipped*))
            (t
             (let ((*operation-stack* (cons tag *operation-stack*)))
               (with-standard-operation-behaviour
                 (if settings
                   (apply #'call-with-settings #'traverse settings)
                   (traverse))))))
      (when (typep performance-status 'performance-complete)
        (setf (component-operation-time component operation) (get-universal-time)
              (component-operation-cycle component operation) (operation-cycle operation)))
      (values performance-status component))))


;;; traversing performance operators for requirements and constituents
;;; differ only in the context for resolving component designators
;;; and the reference type should there be no component found.

(defgeneric perform-requirement (current-operation current-component required-operation required-component &rest arguments)
  (:documentation "In the context of a current operation and component, perform
    an operation on a required component. The 'current' arguments are
    always instances, while the requirements are designated. The required operation
    will be instantiated as a sub-operation of the current one and the designated
    required component will be resolved in the context of the current's parent.
    If the required operation is `t`, that designates the current's type.")

  (:method ((operation operation) component (r-operation symbol) r-component &rest arguments)
    "GIven a designator for the required operation, make a sub-operation to
     the current one and continue."
    (declare (dynamic-extent arguments))
    (apply #'perform-requirement operation component (apply #'make-sub-operation operation r-operation arguments) r-component
           arguments))

  (:method ((operation t) (component component) (r-operation t) (component-name t) &key version &allow-other-keys)
    "GIven a designator for the required component, resolve that in the context
     of the current component's parent. Should that fail, return a
     `status-missing` and a forward reference with the designator."
    (let ((r-component (find-component (component-parent component) component-name version)))
      (if r-component
        (perform-requirement operation component r-operation r-component)
        (values *status-missing* (make-component (component-parent component) 'requirement-reference
                                                 :name component-name :version version)))))

  (:method ((operation operation) (component component) (r-operation function) (r-component t) &key)
    (funcall r-operation r-component))
  (:method ((operation operation) (component component) (r-operation operation) (r-component component) &key)
    "Once everything has been resolved, perform the operation"
    (perform r-operation r-component)))


(defgeneric perform-constituent (current-operation current-component consituent-operation constituent-component &rest arguments)
  (:documentation "In the context of a current operation and component, perform
    an operation on a consituent component. The 'current' arguments are
    always instances, while the constituents are designated. The constituent operation
    will be instantiated as a sub-operation of the current one and the designated
    constituent component will be resolved in the context of the current component.
    If the required operation is `t`, that designates the current's type.")

  (:method ((operation operation) component (c-operation symbol) c-component &rest arguments)
    "GIven a designator for the constituent operation, make a sub-operation to
     the current one and continue."
    (declare (dynamic-extent arguments))
    (apply #'perform-constituent operation component (apply #'make-sub-operation operation c-operation arguments) c-component
           arguments))

  (:method ((operation t) (component component) (c-operation t) (component-name t) &key version &allow-other-keys)
    "GIven a designator for the required component, resolve that in the context
     of the current component's parent. Should that fail, return a
     `status-missing` and a forward reference with the designator."
    (let ((c-component (find-component component component-name version)))
      (if c-component
        (perform-constituent operation component c-operation c-component)
        (values *status-missing* (make-component component 'constituent-reference
                                                 :name component-name :version version)))))

  (:method ((operation operation) (component component) (r-operation function) (r-component t) &key)
    (funcall r-operation r-component))
  (:method ((operation operation) (component component) (c-operation operation) (c-component component) &key)
    "Once everything has been resolved, perform the operation"
    (perform c-operation c-component)))




;;; should probably dispense with the behaviour argument and restrict to
;;; the current dyanmic binding
(defgeneric compute-performance-status (operation component status &key)
  (:documentation "Interpret the status of an operation performed on a
 component. Return two values the effective status or nil if not complete,
 and the original status. The two results :warning and :failure are compared
 against the current value for *performance-behaviour* to determine exceptional
 actions. Any other status is treated as success.")

  ;; simple status are left alone
  (:method ((operation t) (component t) (status performance-complete) &key &allow-other-keys)
    (ecase *operation-success-behaviour*
      (:continue )
      (:force (setq *operation-force* t)))
    status)

  ;; if some strongly dependent requirement succeeded, force subsequent (normally, constituent) operations
  (:method ((operation t) (component t) (status status-skipped) &key &allow-other-keys)
    (ecase *operation-success-behaviour*
      (:continue )
      (:force
       (when (eq (component-operation-cycle component operation)
                 (operation-cycle operation))
         (setq *operation-force* t))))
    status)

  (:method ((operation t) (component t) (status performance-incomplete) &key &allow-other-keys)
    status)

  (:method  ((operation operation) (component t) (status status-warned)
             &key (operation-warning-behaviour *operation-warning-behaviour*))
    (ecase operation-warning-behaviour
      (:abort        *status-aborted*)
      (:continue     *status-succeeded*)
      (:error
       (operation-warned-error operation component))
      ((:ignore nil) *status-ignored*)
      ((:warn :warning)
       (warn "~@<~a warned for ~A.~@:>" (type-of operation) component)
       status)))

  (:method ((operation operation) (component t) (status performance-failed)
            &key (operation-failure-behaviour *operation-failure-behaviour*))
    (ecase operation-failure-behaviour
      (:abort        *status-aborted*)
      (:continue     *status-succeeded*)
      (:error
       (operation-failed-error operation component))
      ((:ignore nil) *status-ignored*)
      ((:warn :warning)
       (warn "~@<~a failed for ~A.~@:>" (type-of operation) component)
       *status-warned*)))

  (:method ((operation operation) (component t) (status status-missing)
            &key (operation-missing-behaviour *operation-missing-behaviour*))
    (ecase operation-missing-behaviour
      (:abort        *status-aborted*)
      (:continue     *status-succeeded*)
      (:error
       (component-missing-error operation component))
      ((:ignore nil) *status-ignored*)
      ((:warn :warning)
       (warn "~@<~a missing component ~A.~@:>" (type-of operation) component)
       *status-warned*)))

  (:method ((operation operation) (component t) (status status-circular)
            &key (operation-circular-behaviour *operation-circular-behaviour*))
    (ecase operation-circular-behaviour
      (:abort        *status-aborted*)
      (:continue     *status-skipped*)
      (:error
       (error 'circular-dependency :components (list component)))
      ((:ignore nil) *status-ignored*)
      ((:warn :warning)
       (warn "Circular operation: (~s ~s)" (type-of operation) component)
       *status-ignored*))))

(defgeneric operation-warned-error (operation component &rest arguments)
  (:method ((operation operation) (component component) &rest arguments)
    (apply #'error (operation-warned-error-class operation) :operation operation :component component arguments)))

(defgeneric operation-failed-error (operation component &rest arguments)
  (:method ((operation operation) (component component) &rest arguments)
    (apply #'error (operation-failed-error-class operation) :operation operation :component component arguments)))

(defgeneric component-missing-error (operation component &rest arguments)
  (:method ((operation operation) (component dependency-reference) &rest arguments)
    (apply #'error (operation-missing-dependency-error-class operation) :operation operation :required-by component
           :required component
           :version (component-version component)
           arguments))
  (:method ((operation operation) (component constituency-reference) &rest arguments)
    (apply #'error (operation-missing-constituent-error-class operation) :operation operation :required-by component
           :required component
           :version (component-version component)
           arguments)))



(defgeneric compute-operation-predecessors (operation component)
  (:documentation "Given operation and component instances, return an association list
 which specifies operations to be performed on predecessors. Each element has the form
   (operation predecessor-name . arguments)
 where the predecessor name is to be resolved in the context component's parent.")
  
  (:method ((operation operation) (context t) )
    (compute-operation-predecessors (type-of operation) context))
  
  (:method ((operation t) (component component))
    (remove component
            (compute-operation-predecessors operation (component-dependency-requirements component))))
  
  (:method ((operation-type symbol) (dependency-definitions list))
    ;; return a single list of component designators in order of appearance
    (let ((predecessors ()))
      (loop for (r-operation . requirements) in dependency-definitions
            when (subtypep operation-type r-operation)
            do (loop for (required-operation . arguments) in requirements
                     do (etypecase required-operation
                          (keyword )            ; ignore settings
                          (symbol (dolist (required-component arguments)
                                    (setf required-component
                                          (etypecase required-component
                                            ((or component string symbol) required-component)
                                            (cons (first required-component))))
                                    (pushnew required-component predecessors
                                             :test #'string-equal))))))
      (nreverse predecessors))))



(defmethod explain ((operation operation) (component component))
  (asdf-message "~&;;; ~A on ~A~%" operation component))

;;;; -------------------------------------------------------------------------
;;;; methods: compile-op

(defmethod perform :before ((operation compile-op) (c source-file))
  (map nil #'ensure-directories-exist (output-files operation c)))


;;; perform is required to check output-files to find out where to put
;;; its answers, in case it has been overridden for site policy
(defmethod perform ((operation compile-op) (c cl-source-file))
  #-:broken-fasl-loader
  (let ((source-file (component-pathname c))
        (output-file (car (output-files operation c))))
    (multiple-value-bind (output warnings-p failure-p)
                         (apply #'compile-file source-file :output-file output-file
                                (compile-op-flags operation))
      (values (if output
                (cond (warnings-p *status-warned*)
                      (failure-p *status-failed*)
                      (t *status-succeeded*))
                *status-failed*)
              c))))

(defmethod output-files ((operation compile-op) (c cl-source-file))
  #-:broken-fasl-loader (list (compile-file-pathname (component-pathname c)))
  #+:broken-fasl-loader (list (component-pathname c)))



;;;; -------------------------------------------------------------------------
;;;; methods: feature-op

(defmethod perform ((o feature-op) (feature-constraint t))
  (if (feature-p feature-constraint)
    (values *status-succeeded* feature-constraint)
    (values *status-missing* feature-constraint)))



;;;; -------------------------------------------------------------------------
;;;; methods: load-op

(defmethod perform ((o load-op) (c cl-source-file))
  (mapcar #'load (input-files o c))
  (values *status-succeeded* c))


(defmethod perform ((op load-op) (system system))
  (if (system-source-file system)
    (if (load-system-definition system)
      (values *status-succeeded* system)
      (values *status-missing* system))
    (values *status-skipped* system)))

(defgeneric load-system-definition (system)
  (:method ((system system))
    (let* ((pathname (system-source-file system))
           (package (or *system-load-package* (make-temporary-package))))
      (unwind-protect
        (with-open-file (asd pathname :if-does-not-exist nil)
          (when asd
            (let ((*package* package))
              (asdf-message
               "~&~@<; ~@;loading system definition from ~A into ~A~@:>~%"
               ;; FIXME: This wants to be (ENOUGH-NAMESTRING
               ;; ON-DISK), but CMUCL barfs on that.
               pathname
               *package*)
              (load pathname))))
        (unless (eq package *system-load-package*)
          (delete-package package))))))



;;;; -------------------------------------------------------------------------
;;;; methods: load-source-op

(defmethod perform ((o load-source-op) (c cl-source-file))
  (let ((source (component-pathname c)))
    (setf (component-property c 'last-loaded-as-source)
          (and (load source)
               (get-universal-time)))))

(defmethod perform ((operation load-source-op) (c static-file))
  nil)

(defmethod output-files ((operation load-source-op) (c component))
  nil)

;;; FIXME: we simply copy load-op's dependencies.  this is Just Not Right.
(defmethod component-depends-on ((o load-source-op) (c component))
  (let ((what-would-load-op-do (cdr (assoc 'load-op
                                           (component-dependency-requirements c)))))
    (mapcar (lambda (dep)
              (if (eq (car dep) 'load-op)
                  (cons 'load-source-op (cdr dep))
                  dep))
            what-would-load-op-do)))

(defmethod operation-done-p ((o load-source-op) (c source-file))
  (if (or (not (component-property c 'last-loaded-as-source))
          (> (safe-file-write-date (component-pathname c))
             (component-property c 'last-loaded-as-source)))
      nil t))


;;;; -------------------------------------------------------------------------
;;;; methods: state-op

(defmethod perform ((operation state-op) (component component))
  (funcall (operation-predicate operation) operation component))

;;;; -------------------------------------------------------------------------
;;;; methods: test-op

(defmethod perform ((operation test-op) (c component))
  nil)

(defmethod operation-done-p ((operation test-op) (c system))
  "Testing a system is _never_ done."
  nil)

(defmethod component-in-order-to :around ((o test-op) (c system))
  (cons `((test-op (load-op ,(component-name c))))
        (call-next-method)))


;;;; ------------------------------------------------------------------------
;;;; Model operators : define-component, make-component


(defgeneric make-component (context class &rest initargs)
  (:method ((group group) (class symbol) &rest initargs)
    (apply #'make-component group (context-find-class group class) initargs))

  (:method ((group t) (class class) &rest initargs)
    (apply #'make-instance class
           :parent group
           initargs)))


(defgeneric define-component (group-context class &rest initargs)
  (:method ((group t) (class t) &rest initargs)
    (apply #'define-component (find-component nil group) class initargs))
  (:method ((group group) (class symbol) &rest initargs)
    (apply #'define-component group (context-find-class group class) initargs))

  (:method ((group group) (class class) &rest initargs &key (name (error "name required.")) (version nil)
            &allow-other-keys)
    (let ((old (find name (component-unreinitialized-constituents group) :key #'component-name :test #'string-equal)))
      (cond ((and old (version-satisfies group version))
             (unless (eq (class-of old) class)
               (change-class old class))
             (define-component group
               (if (cddr initargs) (apply #'reinitialize-instance old initargs) old)))
            (t
             (define-component group
               (apply #'make-component group class initargs))))))

  (:method ((group group) (component component) &key &allow-other-keys)
    "To combine a group with a constituent component, register add the constituent to the
 groups' members and resolve the constituent's location."
    ;; clean up reinitialization state
    (setf (component-unreinitialized-constituents group)
          (remove component (component-unreinitialized-constituents group)))
    ;; allow just one
    (setf (component-parent component) group)
    (setf (group-constituents group)
          (append (group-constituents group) (list component)))
    component))


(defgeneric define-system (class &rest initargs)

  (:method ((class symbol) &rest initargs &key (name (error "name required.")) &allow-other-keys)
    (let ((old (registered-system name)))
      (cond (old
             (unless (eq (type-of old) class)
               (change-class old (find-class class)))
             (let ((old-source-file (system-source-file old)))
               (apply #'reinitialize-instance old initargs)
               (unless (equalp old-source-file (system-source-file old))
                 (warn "Redefining system from ~s to ~s."
                       old-source-file
                       (system-source-file old))))
             (define-system old))
            (t
             (define-system (apply #'make-instance class initargs))))))

  (:method ((system system) &key (name (component-name system)))
    (register-system name system)))
  

;;;; ------------------------------------------------------------------------
;;;; pathname/location/component-class resolution
;;;;


(defgeneric context-find-class (class-context class-designator &rest args
                                        &key if-does-not-exist type)
  (:documentation "GIven a class-context and a class-designator, find and return
 the designated class, allowing for class aliases specific to the context.
 If no class is found, delegate to the context's parent. Once parents have been
 exhausted, search through packages for candidates which meet the type
 constraint. The package search depends on the current *package*
 - first, unless a keyword, the read name;
 - next, when a keyword, *package*::name;
 - finally, when either not a keyword or *package* is not asdf-user, asdf-user::name.
 The effect is to permit any form of reference to standard classes, names
 in the current package, as well as explict, package-qualified names.")
  (declare (dynamic-extent args))

  (:method ((context null) (class-name symbol) &key (if-does-not-exist :error) (type t))
    (labels ((find-named-class (name)
               (when name
                 (let ((class (find-class name nil)))
                   (when (and class (subtypep name type))
                     class))))
             (find-alternatively-named-class (name)
               (let ((asdf-name nil) (package-name nil))
                 (or (unless (keywordp name) (find-named-class name))
                     (and (keywordp name)
                          (setf package-name (find-symbol (symbol-name name) *package*))
                          (not (eq package-name name))
                          (find-named-class package-name))
                     (and (or (not (keywordp name)) (not (eq *package* +asdf-user-package+)))
                          (setf asdf-name (find-symbol (symbol-name name) +asdf-user-package+))
                          (not (eq asdf-name name))
                          (find-named-class asdf-name))))))
      ;; try the alternative names, otherwise if a definition is to be found load it and try again
      (or (find-alternatively-named-class class-name)
          (let ((pathname (search-for-definition-pathname class-name :file-type *extension-file-type*)))
            (when (and pathname (load pathname))
              (find-alternatively-named-class class-name)))
          (case if-does-not-exist
            ((nil) nil)
            (t (sysdef-error "~@<don't recognize component type ~A~@:>" class-name))))))

  (:method ((context class-context) (class-name symbol) &rest args )
    (declare (dynamic-extent args))
    (let ((local (rest (assoc class-name (context-classes context)))))
      (if local
        (find-class local)
        (apply #'context-find-class (context-parent context) class-name args))))

  (:method ((context component) (class-name symbol) &rest args &key (type 'component) if-does-not-exist)
    (declare (dynamic-extent args) (ignore if-does-not-exist))
    (apply #'call-next-method context class-name :type type args))

  (:method ((context operation) (class-name symbol) &rest args &key (type 'operation) if-does-not-exist)
    (declare (dynamic-extent args) (ignore if-does-not-exist))
    (apply #'call-next-method context class-name :type type args)))




;;;; ---------------------------------------------------------------------------
;;;; run-shell-command
;;;;
;;;; run-shell-command functions for other lisp implementations will be
;;;; gratefully accepted, if they do the same thing.
;;;; If the docstring is ambiguous, send a bug report.
;;;;
;;;; We probably should move this functionality to its own system and deprecate
;;;; use of it from the asdf package. However, this would break unspecified
;;;; existing software, so until a clear alternative exists, we can't deprecate
;;;; it, and even after it's been deprecated, we will support it for a few
;;;; years so everyone has time to migrate away from it. -- fare 2009-12-01

(defun run-shell-command (control-string &rest args)
  "Interpolate `args` into `control-string` as if by `format`, and
synchronously execute the result using a Bourne-compatible shell, with
output to `*verbose-out*`.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (asdf-message "; $ ~A~%" command)
    #+sbcl
    (sb-ext:process-exit-code
     (apply #'sb-ext:run-program
            #+win32 "sh" #-win32 "/bin/sh"
            (list  "-c" command)
            :input nil :output *verbose-out*
            #+win32 '(:search t) #-win32 nil))

    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *verbose-out*))

    #+allegro
    ;; will this fail if command has embedded quotes - it seems to work
    (multiple-value-bind (stdout stderr exit-code)
        (excl.osi:command-output
         (format nil "~a -c \"~a\""
                 #+mswindows "sh" #-mswindows "/bin/sh" command)
         :input nil :whole nil
         #+mswindows :show-window #+mswindows :hide)
      (format *verbose-out* "~{~&; ~a~%~}~%" stderr)
      (format *verbose-out* "~{~&; ~a~%~}~%" stdout)
      exit-code)

    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :output-stream *verbose-out*)

    #+clisp                     ;XXX not exactly *verbose-out*, I know
    (ext:run-shell-command  command :output :terminal :wait t)

    #+openmcl
    (nth-value 1
               (ccl:external-process-status
                (ccl:run-program "/bin/sh" (list "-c" command)
                                 :input nil :output *verbose-out*
                                 :wait t)))

    #+ecl ;; courtesy of Juan Jose Garcia Ripoll
    (si:system command)

    #-(or openmcl clisp lispworks allegro scl cmu sbcl ecl)
    (error "RUN-SHELL-COMMAND not implemented for this Lisp")
    ))

;;;; ---------------------------------------------------------------------------
;;;; system-relative-pathname

(defmethod system-source-file ((system-name t))
  (system-source-file (find-system system-name)))

(defun system-source-directory (system-name)
  (make-pathname :name nil
                 :type nil
                 :defaults (system-source-file system-name)))

(defun system-relative-pathname (system pathname &key name type)
  (let ((directory (pathname-directory pathname)))
    (merge-pathnames
     (make-pathname :name (or name (pathname-name pathname))
                    :type (or type (pathname-type pathname))
                    :directory (if (eq (car directory) :absolute)
                                   (cons :relative (cdr directory))
                                   directory))
     (system-source-directory system))))


;;;; -------------------------------------------------------------------------
;;;; General Purpose Utilities


(defun pathname-sans-name+type (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME and TYPE components"
  (make-pathname :name nil :type nil :defaults pathname))

(define-modify-macro appendf (&rest args)
  append "Append onto list")

(defun asdf-message (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (fresh-line *verbose-out*)
  (apply #'format *verbose-out* format-string format-args))

;;; with apologies to christophe rhodes ...
(defun split (string &optional max (constraint '(#\Space #\Tab)))
  (let ((is-ws (etypecase constraint
                 (sequence #'(lambda (char) (find char constraint)))
                 (character #'(lambda (char) (eql char constraint))))))
    (declare (dynamic-extent is-ws))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
         (when (and max (>= words (1- max)))
           (return (cons (subseq string start) list)))
         (setf end (position-if is-ws string :start start))
         (push (subseq string start end) list)
         (incf words)
         (unless end (return list))
         (setf start (1+ end)))))))

(defun split-path-string (s &optional force-directory)
  (check-type s string)
  (let* ((components (split s nil *pathname-punctuation-character*))
         (last-comp (car (last components))))
    (multiple-value-bind (relative components)
        (if (equal (first components) "")
          (values :absolute (cdr components))
          (values :relative components))
      (cond
        ((equal last-comp "")
         (values relative (butlast components) nil))
        (force-directory
         (values relative components nil))
        (t
         (values relative (butlast components) last-comp))))))

(defgeneric split-component-namestring (s)
  (:method ((name null))
    ())
  (:method ((name symbol))
    (split-component-namestring (coerce-name name)))
  (:method ((name string))
    (let ((elements (split name nil *component-punctuation-character*)))
      (if (equal (first elements) "")
        (if (equal (second elements) "")
          (cons :global (cddr elements))
          (cons :absolute (rest elements)))
        (cons :relative elements)))))
  
  
  

(defun remove-keys (key-names args)
  (loop :for (name val) :on args :by #'cddr
    :unless (member (symbol-name name) key-names
                    :key #'symbol-name :test 'equal)
    :append (list name val)))

(defun remove-keyword (key args)
  (loop :for (k v) :on args :by #'cddr
    :unless (eq k key)
    :append (list k v)))

(defun resolve-symlinks (path)
  #-allegro (truename path)
  #+allegro (excl:pathname-resolve-symbolic-links path))

(defun getenv (x)
  #+allegro   (sys:getenv x)
  #+clisp     (ext:getenv x)
  #+clozure   (ccl::getenv x)
  #+cmu       (cdr (assoc (intern x :keyword) ext:*environment-list*))
  #+ecl       (si:getenv x)
  #+gcl       (system:getenv x)
  #+lispworks (lispworks:environment-xiable x)
  #+mcl       (progn x nil)
  #+sbcl      (sb-ext:posix-getenv x))

(defun ensure-directory-pathname (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory form."
  (cond
   ((stringp pathspec)
    (pathname (concatenate 'string pathspec "/")))
   ((not (pathnamep pathspec))
    (error "Invalid pathname designator ~S" pathspec))
   ((wild-pathname-p pathspec)
    (error "Can't reliably convert wild pathnames."))
   ((directory-pathname-p pathspec)
    pathspec)
   (t
    (make-pathname :directory (append (or (pathname-directory pathspec)
                                          (list :relative))
                                      (list (file-namestring pathspec)))
                   :name nil :type nil :version nil
                   :defaults pathspec))))

(defun length=n-p (x n) ;is it that (= (length x) n) ?
  (check-type n (integer 0 *))
  (loop
    :for l = x :then (cdr l)
    :for i :downfrom n :do
    (cond
      ((zerop i) (return (null l)))
      ((not (consp l)) (return nil)))))

(defun ends-with (s suffix)
  (check-type s string)
  (check-type suffix string)
  (let ((start (- (length s) (length suffix))))
    (and (<= 0 start)
         (string-equal s suffix :start1 start))))

(defun coerce-name (name)
  (typecase name
    (component (component-name name))
    (symbol (string-downcase (symbol-name name)))
    (string name)
    (t (sysdef-error "~@<invalid component designator ~A~@:>" name))))


(defun directory-pathname-p (pathname)
  "Given a `pathname`, return true iff it designates a directory.

A directory-pathname is a pathname _without_ a filename. The three
ways that the filename components can be missing are for it to be `nil`,
`:unspecific` or the empty string.

Note that this does _not_ check to see that `pathname` points to an
actually-existing directory."
  (flet ((check-one (x)
           (not (null (member x '(nil :unspecific "")
                              :test 'equal)))))
    (and (check-one (pathname-name pathname))
         (check-one (pathname-type pathname)))))


(defun make-temporary-package ()
  (flet ((try (counter)
           (ignore-errors
             (make-package (format nil "~a~D" 'asdf counter)
                           :use '(:cl :asdf)))))
    (do* ((counter 0 (+ counter 1))
          (package (try counter) (try counter)))
         (package package))))


(defun safe-file-write-date (pathname)
           ;; if FILE-WRITE-DATE returns NIL, it's possible that the
           ;; user or some other agent has deleted an input file.  If
           ;; that's the case, well, that's not good, but as long as
           ;; the operation is otherwise considered to be done we
           ;; could continue and survive.
  (or (file-write-date pathname) 0))


(defgeneric feature-p (form)
  (:method ((form symbol))
    (find form *features* :test #'string-equal))
  (:method ((form string))
    (find form *features* :test #'string-equal))
  (:method ((form null))
    t)
  (:method ((form cons))
    (ecase (first form)
      (not (not (feature-p (second form))))
      (and (every #'feature-p (rest form)))
      (or (some #'feature-p (rest form))))))
  

(labels ((compute-method-definition (operator form)
           (destructuring-bind (op qual (o c) &body body) form
             `(lambda (,c) (defmethod ,operator ,qual ((,o ,op) (,c (eql ,c)))
                             ,@body))))
         (check-op (op)
           (cond ((eq op t) t)
                 ((subtypep op 'operation) op)
                 (t
                  (sysdef-error "invalid operation: ~s." op))))
         (rewrite-dependency (dependency)
           (etypecase dependency
             (cons
              (destructuring-bind (dependent-op . requirements) dependency
                (cons (check-op dependent-op)
                      (mapcar #'rewrite-requirement requirements))))))
         (rewrite-requirement (requirement)
           (etypecase requirement
             (cons
              (destructuring-bind (required-op . required-components) requirement
                (cond ((string-equal required-op :feature)
                       `(feature-op ,@required-components))
                      (t
                       (cons (check-op required-op)
                             (mapcar #'rewrite-component-ref required-components))))))))
         (rewrite-component-ref (component-ref)
           (etypecase component-ref
             ((or string (and symbol (not null))) (coerce-name component-ref))
             (cons
              (destructuring-bind (dependent-op . requirements) component-ref
                (cond ((string-equal dependent-op :version)
                       (destructuring-bind (component-name version) requirements
                         `(,component-name :version ,version)))
                      (t 
                       (sysdef-error "invalid component reference: ~s." component-ref)))))))
         (rewrite-dependency-def (dependency-def)
           (etypecase dependency-def
             ((or string (and symbol (not null))) (coerce-name dependency-def))
             (cons
              (destructuring-bind (dependent-op . requirements) dependency-def
                (cond ((string-equal dependent-op :feature) `(feature-op ,@requirements))
                      ((string-equal dependent-op :version) (destructuring-bind (component-name version) requirements
                                                              `(,component-name :version ,version)))
                      (t (list (check-op dependent-op) (mapcar #'rewrite-dependency-def requirements))))))))
         )

  (defgeneric canonicalize-component-option (prototype option-key option-form)
    (:documentation "Given a component prototype and the option-key and option-form
 from a definition form, canonicalize the definition form for evaluation as
 an initialization argument in a define-system or define-component form.
 This includes coarse type checks, quoting, and delayed evaluation forms for
 'inline' methods, and system pathname defaults.")
    (:argument-precedence-order option-form option-key prototype)

    (:method ((type t) (key (eql :author)) (form string))                         (values key form))
    (:method ((type t) (key (eql :author)) (form list))                         (values key `(quote ,form)))
    (:method ((type group) (key (eql :default-component-class)) (class symbol))  (values key `(quote ,class)))
    (:method ((type t) (key (eql :depends-on)) (form list))                 
      (values key (when form `(quote ,(mapcar #'rewrite-dependency-def form)))))
    (:method ((type t) (key (eql :depends-on)) (form t))
      (sysdef-error "~a :depends-on must be a list: ~s" type form))
    (:method ((type t) (key (eql :description)) (form t))                    (values key form))
    (:method ((type t) (key (eql :do-first)) (form list))    ; in-order-to rewrite involves just type-checking
      (values key (when form `(quote ,(mapcar #'rewrite-dependency form)))))
    (:method ((type t) (key (eql :do-first)) (form t))
      (sysdef-error "~a :do-first must be a list of dependecies: ~s" type form))
    (:method ((type t) (key (eql :explain)) (form list))                     (values key (compute-method-definition 'explain form)))
    (:method ((type t) (key (eql :failure-behaviour)) (form t))              (values key form))
    (:method ((type t) (key (eql :in-order-to)) (form list))    ; in-order-to rewrite involves just type-checking
      (values key (when form `(quote ,(mapcar #'rewrite-dependency form)))))
    (:method ((type t) (key (eql :in-order-to)) (form t))
      (sysdef-error "~a :in-order-to must be a list of dependecies: ~s" type form))
    (:method ((type t) (key (eql :long-description)) (form t))               (values key form))
    (:method ((type t) (key (eql :license)) (form t))                        (values key form))
    (:method ((type t) (key (eql :licence)) (form t))                        (values key form))
    (:method ((type t) (key (eql :missing-behaviour)) (form t))              (values key form))
    (:method ((type t) (key (eql :maintainer)) (form t))                     (values key (string form)))
    (:method ((type t) (key (eql :name)) (form t))                           (values key (coerce-name form)))
    (:method ((type t) (key (eql :operation-done-p)) (form list))            (values key (compute-method-definition 'operation-done-p form)))
    (:method ((type t) (key (eql :output-files)) (form list))                (values key (compute-method-definition 'output-files form)))
    (:method ((type t) (key (eql :pathname)) (form string))                  (values key form))
    (:method ((type t) (key (eql :pathname)) (form pathname))                (values key form))
    (:method ((type t) (key (eql :perform)) (form list))                     (values key (compute-method-definition 'perform form)))
    (:method ((type t) (key (eql :properties)) (form t))                     (values key (when form `(quote ,form))))
    (:method ((type t) (key (eql :serial)) (form list))                      (values key form))
    (:method ((type t) (key (eql :settings)) (form t))                       (values key (when form `(quote ,form))))
    (:method ((type system) (key (eql :source-file)) (form string))                  (values key form))
    (:method ((type system) (key (eql :source-file)) (form pathname))                (values key form))
    (:method ((type t) (key (eql :strongly-depends-on)) (form list))           
      (values key (when form `(quote ,(mapcar #'rewrite-dependency-def form)))))
    (:method ((type t) (key (eql :strongly-depends-on)) (form t))
      (sysdef-error "~a :weakly-depends-on must be a list : ~s" type form))
    (:method ((type t) (key (eql :success-behaviour)) (form t))              (values key form))
    (:method ((type t) (key (eql :version)) (form t))                        (values key form))
    (:method ((type t) (key (eql :warning-behaviour)) (form t))              (values key form))
    (:method ((type t) (key (eql :weakly-depends-on)) (form list))           
      (values key (when form `(quote ,(mapcar #'rewrite-dependency-def form)))))
    (:method ((type t) (key (eql :weakly-depends-on)) (form t))
      (sysdef-error "~a :weakly-depends-on must be a list : ~s" type form))

    (:method ((type symbol) (key t) (form t))
      (sysdef-error "~a does not support the options: ~s . ~s" type key form))
    (:method ((type component) (key t) (form t))
      "If the option is not recognized, the error should abbreviate the component
 to specify just the type."
      (sysdef-error "~a does not support the options: ~s . ~s"
                    (type-of type) key form))))



;;;; ------------------------------------------------------------------------
;;;; Bootstrap

;;; given this loaded file, bootstrap the asdf definition:
;;; - load its colocated .asd,
;;; - adjusting the "asdf.lisp"'s component properties to indicate that it has
;;;   been loaded as whether it has been compiled.
;;; - load-op the system
;;;
;;; the consequence should be that the system is loaded. if this file does not
;;; need to be compiled, the load will be net this file. 

#+(or )  ;; has not been tested with this version
(unless (boundp '*asdf-bootstrap)        ; don't go on forever
  (let* ((*asdf-bootstrap t)
         (asdf-system-pathname
          (make-pathname :name "asdf" :type *sysdef-file-type* :defaults *load-pathname*))
         (asdf-system nil)
         (asdf-source-file-component nil)
         (asdf-source-file-pathname nil))
    (declare (special *asdf-bootstrap))
    (cond ((and (probe-file asdf-system-pathname)
                (load asdf-system-pathname)
                (setf asdf-system (find-system :asdf nil))
                (setf asdf-source-file-component (find-component asdf-system "asdf")))
           ;; adjust the component to reflect its loaded state
           (setf (component-operation-time asdf-source-file-component 'load-op)
                 (get-universal-time))
           (setf asdf-source-file-pathname (component-pathname asdf-source-file-component))
           (setf (component-operation-time asdf-source-file-component 'compile-op)
                 (file-write-date (compile-file-pathname asdf-source-file-pathname)))
           (when (equalp (truename *load-pathname*) (truename asdf-source-file-pathname))
             (setf (component-property asdf-source-file-component 'last-loaded-as-source)
                   (get-universal-time)))
           ;; with the adjustments to reflect reality, load the system
           (operate 'load-op :asdf))
          (t
           (asdf-message ";; No system definition in ~a" asdf-system-pathname)))))
  

;;;; -------------------------------------------------------------------------
;;;; Cleanups after hot-upgrade.
;;;; Things to do in case we're upgrading from a previous version of ASDF.
;;;; See https://bugs.launchpad.net/asdf/+bug/485687
;;;;
;;;; TODO: debug why it's not enough to upgrade from ECL <= 9.11.1
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ecl ;; Support upgrade from before ECL went to 1.369
  (when (fboundp 'compile-op-system-p)
    (defmethod compile-op-system-p ((op compile-op))
      (getf :system-p (compile-op-flags op)))))

;;;; -----------------------------------------------------------------
;;;; Done!
(when *load-verbose*
  (asdf-message ";; ASDF, version ~a" (asdf-version)))

(pushnew :asdf *features*)
;;(pushnew :asdf2 *features*) ;; do that when we reach version 2

(provide :asdf)

;;;; The End.
