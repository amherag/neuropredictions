;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Paren -*-


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
;;;=====================================================================



;;;=====================================================================
;;;		   Meta-parser for ParEn System Grammars
;;;
;;;		   Stuart M. Shieber and Barney Pell
;;;
;;; written: 8/31/87 19:24:06
;;; modified:3/24/88 12:55:10		Renamed functions to eliminate
;;;				        LR from names.
;;; 	     4/01/88 18:14:56		Uses *documentation* to determine
;;; 	     				how much to document tables.  As
;;; 	     				in DUMP-TABLES, default to
;;; 	     				almost no doc.
;;;=====================================================================


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :meta-parser)
(in-package :ParEn
	    :use '(:user :lisp))


(export '( make-tables-from-source 
	   *compile-tables* ))

(require :default-parser
	 (lookup-pathname :default-parser))

;;; *COMPILE-TABLES*
;;; ================
;;;
;;;	Flag determines whether the lisp versions of parse table files
;;;	are automatically compiled upon generation.

(defvar *compile-tables* t)


;;; MAKE-TABLES-FROM-SOURCE
;;; ==========================
;;;
;;;	Compiles the grammar given in source format (see the metagrammar
;;;	for the source format) into tables readable by the default
;;;	parser above (or any other parser based on LR-PARSE).  Optional
;;;	keywords and their defaults are as follows:
;;;
;;;		DOCUMENTATION	(T or NIL)			NIL
;;;		VERBOSE		(T or NIL)			T
;;;		TABLE-CONSTRUCTION (:SLR, :LALR1, :BOTH) 	:BOTH

(defun make-tables-from-source (filename &key (documentation *documentation*)
					      (verbose *verbose*)
					      (table-construction
					       *default-table-construction*))
  (let ((*source-file* filename)
	(*package* (find-package :paren))
	(*paren-readtable* (copy-readtable nil))
	(*documentation* documentation)
	(*verbose* verbose)
	(*default-table-construction* table-construction)) 
    (declare (special *source-file* *paren-readtable* *package* *documentation*
		      *verbose* *default-table-construction))
    (let ((parser-fn (make-default-parser (lookup-pathname :metagrammar-tables))))
      (with-open-file (stream *source-file* :direction :input)
	(funcall parser-fn stream)))))


;;; GENERATE-TABLES-FROM-DECLS-AND-PRODS
;;; ====================================
;;;
;;;	Function called from within the metagrammar actions.  It is
;;;	given Lispified versions of the preterminals and the productions
;;;	and actions in a parsed grammar and builds the table file as a
;;;	Lisp file (and possibly compiles it too).  Lots of screwy stuff
;;;	with RT filenames goes on.

(defun generate-tables-from-decls-and-prods (decls prods)
  (declare (special *source-file*))
  ;; Force the package to be the user package. Later we make sure to
  ;; read the temp file back into the same package.
  (let ((*package* (find-package :user))
	;; File to put LISP syntax version of grammar in.
	(temp-file (make-pathname
		     :type "temp"
		     :version :newest
		     :defaults *source-file*)))
    (declare (special *package*))
    ;; Write out the lisp syntax version.
    (with-open-file  #-:ccl(temp temp-file :direction :output)
		     #+:ccl(temp temp-file :direction :output
				 :if-exists :supersede)
		     ;; Write the preterminal declarations.
		     (format temp "~a ~3%" decls)
		     ;; Write the productions (and actions).  Changed
		     ;; from format "~S..." to prin1 because of Lucid
		     ;; bug printing backquoted material.
		     (mapc #'(lambda (ps-elt)
			       (prin1 ps-elt temp)
			       (format temp "~2%"))
			   prods))
    (let* ((grammar-name (pathname-name *source-file*))
	   ;; File to put Lisp version of tables in.
	   (lisp-tables 
	     (make-pathname 
	       :type *default-lisp-suffix*
	       :version :newest
	       :name 
	       ;; Construct the name of the tables file by
	       ;; adding "-tables" to the grammar file name...
	       #-rt (concatenate 'string 
				 grammar-name
				 "-tables")
	       ;; ...unless you're on a brain-damaged machine
	       ;; that allows only 14 character file names, in
	       ;; which case use the first 5 chars + "-tbl"
	       ;; (+ ".lisp" of course)
	       #+rt (concatenate 'string
				 (subseq grammar-name
					 0 (min 5 (length grammar-name)))
				 "-tbl")		       
	       :defaults *source-file*)))
      ;; Generate the tables from the Lisp syntax version of the grammar.
      (generate-tables temp-file lisp-tables)
      ;; Delete the temp file.
      (delete-file temp-file)
      ;; Compile the tables (including actions) into object code (if so
      ;; desired).
      (when *compile-tables*
	(when *verbose* (format t "~&Compiling table file."))
	(compile-file lisp-tables)))))


;;; MAKE-TABLES-FOR-METAGRAMMAR
;;; ==============================
;;;
;;;	Compiles the metagrammar given in source format into tables
;;;	readable by the defalt parser above, which are used for
;;;	compiling all other source files (including the metagrammar
;;;	source itself).

(defun make-tables-for-metagrammar ()
  (make-tables-from-source (lookup-pathname :metagrammar)))











