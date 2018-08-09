;;; -*- Mode: LISP; Syntax: Common-lisp; Package: Paren; Base: 10 -*-


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



;;;================================================================================
;;;                    TABLES-FROM-GRAMMAR.lisp
;;;
;;;               Sandy Wells & Barney Pell
;;;
;;; written:  3/10/88 15:52:54
;;; Jun 29, 1988 -- Sandy -- fixed bug on name of source file in table files.
;;;================================================================================                              

;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :tables-from-grammar)
(in-package :ParEn
	    :use '(:user :lisp))	
(require :oset
	 (lookup-pathname :oset))
(require :empty-st
	 (lookup-pathname :empty-st))
(require :first
	 (lookup-pathname :first))
(require :follow
	 (lookup-pathname :follow))
(require :dump
	 (lookup-pathname :dump))
(require :lr0-sets
	 (lookup-pathname :lr0-sets))
(require :slrpgram
	 (lookup-pathname :slrpgram))
(require :tables
	 (lookup-pathname :tables))
(require :lalr1
	 (lookup-pathname :lalr1))

(export '(generate-tables))


;;; *VERBOSE*
;;; =========
;;;
;;;	  Be verbose about phase of table construction.

(defvar *verbose* t)




;;; *DEFAULT-TABLE-CONSTRUCTION*
;;; ============================
;;;
;;;	Default value for the keyword TABLE-CONSTRUCTION in
;;;	TABLES-FROM-GRAMMAR and GENERATE-TABLES.  Possible values are
;;;	:lalr1, :slr, and :both.

(defvar *default-table-construction* :both)


;;;================================================================================
;;;                    GENERATE-TABLES
;;;       Top level function for reading in a compiled grammar and 
;;;       generating the parse tables.  Reads (augmented) grammar from GRAM, and
;;;       dumps tables into TABLES.  Keyword arguments are VERBOSE, DOCUMENTATION, 
;;;	  and TABLE-CONSTRUCTION.
;;;	  To generate tables from a source-grammar, use MAKE-TABLES-FROM-SOURCE.
;;;
;;;================================================================================                              

(defun GENERATE-TABLES (gram tables &key (verbose *verbose*)
					 (documentation *documentation*)
					 (table-construction *default-table-construction*))
  (let ((*verbose* verbose)
	(*documentation* documentation))
    (declare (special *verbose* *documentation*))
    (tables-from-grammar gram :table-construction table-construction)
    (dump-tables tables)))



;;; TABLES-FROM-GRAMMAR
;;; ===================
;;;
;;;	Do all needed starting with a lisp syntax grammar

(defun tables-from-grammar (file-name 
			    &key 
			    (table-construction *default-table-construction*))

  (slurp-grammar file-name)
  (calculate-empty-string-derivers)
  (calculate-first-sets)
  (calculate-follow-sets)
  (make-lr0-collection)
  (ccase table-construction
    (:lalr1					;if just want lalr1
     (progn
       (when *verbose*
	 (format t "~&Trying to construct LALR(1) tables.~%"))
       (lalr1-do-lookaheads)
       (if (build-parse-tables t t)
	   (when *verbose* (format t "~&Table construction successful.~%"))
	   (format t "~&*** Warning:  Table construction encountered conflicts. ***~%"))))
    
    (:slr					;if just want slr1
     (when *verbose* (format t "~&Trying to construct SLR(1) tables.~%"))
     (if (build-parse-tables nil t)
	 (when *verbose* (format t "~&Table construction successful~%"))
	 (format t "~&*** Warning:  Table construction encountered conflicts. ***~%")))
		 
    
    (:both				       	;try slr, then lalr1 if necessary
     (when *verbose* (format t "~&Trying to construct SLR(1) tables.~%"))
     (if (build-parse-tables nil nil)		;not lalr1, don't continue
	 (when *verbose* (format t "~&Table construction successful.~%"))
	 (progn 
	   (format t "~&SLR(1) table construction failed.  Trying LALR(1).~%")
	   (lalr1-do-lookaheads)		;prepare to do lalr1
	   (if (build-parse-tables t t)		;go for lalr1, try again to build tables
	       (when *verbose* (format t "~&Table construction successful.~%"))
	       (format t "~&*** Warning:  Table construction encountered conflicts. ***~%  ")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test
;(comment-out
;  (progn
;;    (generate-tables "gram6.2")
;    (generate-tables "gram4")
;    (princ "symbols: ") (newline)
;    (cruise-symbols-2)
;    (princ "productions: ") (newline)
;    (print-productions)
;    (princ "lr0 item sets: ") (newline)
;    (collection-print-kernels *item-set-collection*)
;    (princ "lalr(1) tables: ") (newline)
;    (cruise-parse-tables)
;    ))

