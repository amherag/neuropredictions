;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ParEn -*-

(format t
 "~&
;;;=====================================================================
;;;
;;;  		 ParEn (tm) Common LISP Parser Environment
;;;
;;;  Unpublished-rights reserved under the copyright laws of the United
;;;  States.
;;;  
;;;  This data and information is proprietary to, and a valuable trade
;;;  secret of, SRI International.  It is given in confidence by SRI
;;;  International.  Its use, duplication, or disclosure is subject to
;;;  the restrictions set forth in the License Agreement under which it
;;;  has been distributed.
;;;				    
;;;
;;;	    Unpublished Copyright (c) 1987, SRI International
;;;	         ParEn is a Trademark of SRI International
;;;
;;;=====================================================================~%")


;;;================================================================================
;;;
;;;			       PAREN.lisp
;;;
;;;			 Written by Barney Pell
;;;
;;; Jun  3, 1988 -- Sandy Wells:
;;;        changed some file names 
;;;        conditionalized some pathname defaults
;;;        added system to use list of paren for lucid
;;; 	   (was breaking on macroexpansion of backquote in client lambdas)
;;; 
;;;================================================================================


(provide :paren)
(in-package :ParEn 
	    :use '(:user :lisp :system))


;;; for allegro common lisp on a sun3
(defvar *default-paren-pathname*
  (concatenate 'string user::*paren-load-path* "foo.lisp"))
(defvar *default-bin-suffix* "o")
(defvar *default-lisp-suffix* "lisp")

#+3600
(defvar *default-paren-pathname* "Paren:Paren;foo.lisp.newest")
#+3600
(defvar *default-bin-suffix* "BIN")
#+3600
(defvar *default-lisp-suffix* "LISP")

;;; for lucid lisp on a sun3
;;;#+akcl
;;;(defvar *default-paren-pathname* "/s1/D/torrance/paren/foo.lisp")
;;;#+akcl
;;;(defvar *default-bin-suffix* "o")
;;;#+akcl
;;;(defvar *default-lisp-suffix* "lisp")



;;; for lucid lisp on a sun3
#+(and :lucid :sun3)
(defvar *default-paren-pathname* "/home/proj1/paren/foo.lisp")
#+(and :lucid :sun3)
(defvar *default-bin-suffix* "2bin")
#+(and :lucid :sun3)
(defvar *default-lisp-suffix* "lisp")

;;; for lucid lisp on a sun2
#+(and :lucid :sun (not :sun3))
(defvar *default-paren-pathname* "/ab/sw/lisp/paren/foo.lisp")
#+(and :lucid :sun (not :sun3))
(defvar *default-bin-suffix* "lbin")
#+(and :lucid :sun (not :sun3))
(defvar *default-lisp-suffix* "lisp")

;;; for lucid lisp on an ibm pcrt (which has a pretty un-helpful features list)
;;; IS THERE A BETTER WAY TO FIGURE THIS OUT?
#+(and :unix :lucid (not :sun))
(push :rt *features*)

#+rt
(defvar *default-paren-pathname* "/u/sheleg/paren/foo.lisp")
#+rt
(defvar *default-bin-suffix* "bbin")
#+rt
(defvar *default-lisp-suffix* "lisp")



(defvar *default-version* :newest)


(defvar *paren-system-pathnames*
	'((:metagrammar . "metagramr.src")
	  (:metagrammar-tables . "metagramr-tables")
	  (:util . "util")
	  (:oset . "oset")
	  (:g-symbol . "g-symbol")
	  (:slrpgram . "slrpgram")
	  (:item . "item")
	  (:closure0 . "closure0")
	  (:closure1 . "closure1")
	  (:lr0-sets . "lr0-sets")
	  (:empty-st . "empty-st")
	  (:first . "first")
	  (:follow . "follow")
	  (:dump . "dump")
	  (:tables . "tables")			;Build parse tables
	  (:lalr1 . "lalr1")			;LALR1 table generator routines
	  (:tables-from-grammar . "gm-to-tab")	;Paren Top Level
	  (:lr-parse . "lr-parse")		;Parser driver
	  (:default-parser . "def-parse")
	  (:meta-parser . "metaparse")
	  ))


;;; LOOKUP-PATHNAME
;;; ===============
;;;
;;;	Returns the most recent file defining the given module as
;;;	defined by the alist above.  Tries adding *default-bin-suffix*
;;;	as an extension as an alternative file name.

(defun lookup-pathname (module)
  (let* ((lisp-path
	   (merge-pathnames
	     (cdr (assoc module *paren-system-pathnames*))
	     *default-paren-pathname*))
	 (lisp-file (probe-file lisp-path))
	 (bin-path
	   (make-pathname
			 :type *default-bin-suffix*
			 :defaults lisp-path
			 :version *default-version*))
	 (bin-file (probe-file bin-path)))
    
    (if lisp-file
	(if bin-file
	    (if (> (file-write-date bin-file)
		   (file-write-date lisp-file))
		(add-version bin-path bin-file)
		(add-version lisp-path lisp-file))
	    (add-version lisp-path lisp-file))
	(if bin-file 
	    (add-version bin-path bin-file)
	    nil))))


;;; ADD-VERSION
;;; ===========
;;;
;;;	Returns a pathname the same as the PATHNAME but with the version
;;;	taken from the FILENAME.  Intended to be used where PATHNAME is
;;;	a logical pathname and FILENAME is the truename of the most
;;;	recent version of the file.  An appropriate logical pathname for
;;;	the file instance is therefore generated which is transportable
;;;	between systems.  All of this trouble is only necessary because
;;;	of logical pathnames on Symbolics machines.

(defun add-version (pathname filename)
  (make-pathname :version (pathname-version filename)
		 :defaults pathname))


;;; SYSTEM-COMPILE
;;; ==============
;;;
;;;	Compiles the appropriate files in the system, depending on
;;;	whether the .lisp version is newer than the *default-bin-suffix* version

(defun system-compile (&optional (pathname-alist *paren-system-pathnames*))
  (mapc
    (function (lambda (module-path)
      (let ((file (lookup-pathname (car module-path))))
	(if (and file (equal *default-lisp-suffix* (string (pathname-type file))))
	    (progn (format t "~&Compiling ~s~%" (namestring file))
		   (compile-file file)
		   (load (lookup-pathname (car module-path))))
	    (format t "~&Skipping ~s~%" (namestring file))))))
    pathname-alist))


;;; SYSTEM-LOAD
;;; ===========
;;;
;;;	Loads the most recent .lisp or *default-bin-suffix* files in the
;;;	system.

(defun system-load (&optional (pathname-alist *paren-system-pathnames*))
  (mapc
    (function (lambda (module-path)
		(let ((file (lookup-pathname (car module-path))))
		  (if (and file
			   (member (string (pathname-type file))
			      `(,*default-lisp-suffix* ,*default-bin-suffix*)
			      :test #'string-equal))
		      (load file)))))
    pathname-alist))


;;; Load the system unless on a Symbolics machine where the defsystem
;;; replaces this.

#-(and symbolics 3600 rel7)
(progn 
  (load (lookup-pathname :tables-from-grammar))
  (load (lookup-pathname :meta-parser)))


#||
;;; SYSTEM-COPY
;;; ===========
;;;
;;;	Occasionally useful utility for copying the system from one
;;;	location to another.

(defun system-copy (output-pathname)
  (mapc (function (lambda (module)
		    (let* ((file 
			    (merge-pathnames
			     (cdr module)
			     *default-paren-pathname*))
			  (output-file (merge-pathnames output-pathname
							file)))
		      (format t "~&Copying file ~a to ~a.~%" file output-file)
		      (zl:copyf file output-file))))
	*paren-system-pathnames*))
||#


