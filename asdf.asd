;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license.              ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2001-2009 Daniel Barlow and contributors           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem :asdf
  :author ("Daniel Barlow")
  :licence "MIT"
  :description "Another System Definition Facility"
  :long-description "ASDF builds Common Lisp software organized into defined systems."
  :depends-on ()
  :components
  ((:file "asdf")
   #+ecl (:file "asdf-ecl" :depends-on ("asdf"))
   #+windows (:file "asdf-windows" :depends-on ("asdf"))
   (:module :de.setf
    :pathname #p"LIBRARY:de;setf;utility;asdf;"
    :components ((:file "patches")
                 (:file "operators")
                 (:file "hierarchical-names")
                 (:file "contingent-on")))
   #+sbcl(:file "asdf-sbcl" :depends-on ("asdf"))))
