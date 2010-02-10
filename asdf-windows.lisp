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
;;;; Windows shortcut support.  Based on:
;;;;
;;;; Jesse Hager: The Windows Shortcut File Format.
;;;; http://www.wotsit.org/list.asp?fc=13
;;;; -----------------------------------------------------------------

(defparameter *link-initial-dword* 76)
(defparameter *link-guid* #(1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70))

(defun read-null-terminated-string (s)
  (with-output-to-string (out)
    (loop :for code = (read-byte s)
      :until (zerop code)
      :do (write-char (code-char code) out))))

(defun read-little-endian (s &optional (bytes 4))
  (loop
    :for i :from 0 :below bytes
    :sum (ash (read-byte s) (* 8 i))))

(defun parse-file-location-info (s)
  (let ((start (file-position s))
        (total-length (read-little-endian s))
        (end-of-header (read-little-endian s))
        (fli-flags (read-little-endian s))
        (local-volume-offset (read-little-endian s))
        (local-offset (read-little-endian s))
        (network-volume-offset (read-little-endian s))
        (remaining-offset (read-little-endian s)))
    (declare (ignore total-length end-of-header local-volume-offset))
    (unless (zerop fli-flags)
      (cond
        ((logbitp 0 fli-flags)
          (file-position s (+ start local-offset)))
        ((logbitp 1 fli-flags)
          (file-position s (+ start
                              network-volume-offset
                              #x14))))
      (concatenate 'string
        (read-null-terminated-string s)
        (progn
          (file-position s (+ start remaining-offset))
          (read-null-terminated-string s))))))

(defun parse-windows-shortcut (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (handler-case
        (when (and (= (read-little-endian s) *link-initial-dword*)
                   (let ((header (make-array (length *link-guid*))))
                     (read-sequence header s)
                     (equalp header *link-guid*)))
          (let ((flags (read-little-endian s)))
            (file-position s 76)        ;skip rest of header
            (when (logbitp 0 flags)
              ;; skip shell item id list
              (let ((length (read-little-endian s 2)))
                (file-position s (+ length (file-position s)))))
            (cond
              ((logbitp 1 flags)
                (parse-file-location-info s))
              (t
                (when (logbitp 2 flags)
                  ;; skip description string
                  (let ((length (read-little-endian s 2)))
                    (file-position s (+ length (file-position s)))))
                (when (logbitp 3 flags)
                  ;; finally, our pathname
                  (let* ((length (read-little-endian s 2))
                         (buffer (make-array length)))
                    (read-sequence buffer s)
                    (map 'string #'code-char buffer)))))))
      (end-of-file ()
        nil))))
