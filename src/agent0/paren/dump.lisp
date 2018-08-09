;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ParEn -*-

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
;;;                    DUMP.lisp
;;;
;;;               Sandy Wells
;;;
;;;        Converted to Common LISP by 
;;;             Barney Pell
;;;
;;; converted:  8/14/87 2:31:30
;;;
;;; modified:   8/17/87 7:08:00     Dump lambda list as last table, comment tables. BP
;;;		12/02/87 11:18:52   Cruise-slr-tables -> Cruise-parse-tables.  BP
;;; 		4/01/88 17:52:10    Default to almost no documentation in table files.  BP
;;; 
;;;             Jun 13, 1988 -- SW  Modified format of table files so that they
;;;                                 may be compiled, to compile the client lambdas.
;;;                                 Thats the reason for all the silly setqs.
;;; 
;;;             Jun 29, 1988 -- SW  Fixed buggy name of source file in table files.
;;;================================================================================     
;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :dump)
(in-package :ParEn
	    :use '(:lisp :user))

(require :util
	 (lookup-pathname :util))
(require :g-symbol
	 (lookup-pathname :g-symbol))
(require :empty-st
	 (lookup-pathname :empty-st))
(require :first
	 (lookup-pathname :first))
(require :follow
	 (lookup-pathname :follow))
(require :slrpgram
	 (lookup-pathname :slrpgram))
(require :lr0-sets
	 (lookup-pathname :lr0-sets))




;;; *DOCUMENTATION*
;;; ===============
;;;
;;;	Whether to document parse-tables heavily or not, as used by
;;;	MAKE-TABLES-FROM-SOURCE and DUMP-TABLES.  Either NIL or T.

(defvar *documentation* nil)


;;; DUMP-TABLES
;;; ===========
;;;
;;; 	Dump parsing tables and associated stuff into a file.
;;;
;;; 	The following stuff is dumped in a compilable format.  The
;;;     resulting file will setq some global variables to the contents
;;;     of the parse tables and the action functions when loaded.
;;;
;;;     The files encode the following:
;;; 		An ordered (by grammar symbol index) lexicon.
;;; 		A list of the indices of terminal grammar symbols.
;;; 		Some production info, ordered by production index, 
;;;    		containing the index of the lhs grammar symbol and the length
;;;    		of the rhs of the production.
;;; 		A sparse representation of the action function
;;;    		(eyball one and you'll get the idea...).
;;; 		A similar representation of the goto function.
;;; 		The index of the start state.
;;; 		The index of the end symbol.
;;; 		The client lambda functions, one for each production, in the same order.
	

(defun dump-tables (table-filename)
  (let ((*print-radix* nil)
	;; Force everything to be written with package prefixes relative
	;; to the user package, so that it works to read this back into
	;; the same package.
	(*package* (find-package :user)))
    (with-open-file #-:ccl(stream table-filename :direction :output)
		    #+:ccl(stream table-filename :direction :output
                                  :if-exists :supersede)
      (declare (special *source-file*))

      ;; Print package header information appropriate for the machine.
      #+symbolics 
      (format stream "~&;;; -*- Base: 10; Syntax: Common-lisp; Package: USER -*-~%")
      #+:ccl
      (format stream "~&;;; -*- Package: USER -*-~%")
            
      ;; Print out Date, Time, and Filename
      (format stream "~2%;;; Grammar File: ~A~%;;; Generated: " *source-file*)
      (print-date-and-time stream)
      (format stream "~3%")

      ;; If there were conflicts resolved by precedence rules, print out a warning.
      (when *conflicts*
	  (format
	    stream
	    ";;; *****  WARNING:  This grammar produced CONFLICTS!~%~
;;; -- Consult DOCUMENTATION!!!~3%"))

      ;; Make sure this is read back into the user package (since we so
      ;; carefully write things to be interpreted from the user
      ;; package).
      (format stream "(in-package :user)~2%")

      ;; dump out some special declarations to keep the compiler quiet when the
      ;; the table files are comiled.
      (format stream "~s~2%"
	      '(proclaim '(special *LEXICON* *PRETERMINAL-INDICES* *TERMINAL-INDICES*
			   *PRODUCTION-INFO* *ACTION-TABLE* *GOTO-TABLE*
			   *LR-PARSER-START-STATE-INDEX* *END-SYMBOL-INDEX*
			   *CLIENT-LAMBDAS*)))

      ;; dump out an ordered lexicon
      (format stream ";;; ***LEXICON***~2%(setq ~s ~%'#~s)~3%"
	      '*LEXICON*
	      (mapcar
		#'(lambda (pair)
		    (string
		      (car pair)))
		(reverse *g-symbol-alist*)))
      
      
      ;; dump a list of the pre-terminal grammar symbols
      (format stream
	      ";;; ***PRE-TERMINAL GRAMMAR SYMBOLS***~2%~
               (setq ~s ~%'~S)~3%"
	      '*PRETERMINAL-INDICES*
	      (mapcar #'g-symbol-index *preterms*))

      ;; dump a list of the indices of terminal grammar symbols
      ;; deal with some special cases...
      (format stream
	      ";;; ***TERMINAL GRAMMAR SYMBOLS***~2%~
               (setq ~s ~%'~S)~3%" 
	      '*TERMINAL-INDICES*
	      (mapcar
		#'g-symbol-index
		(delete
		  *empty-string-g-symbol*
		  (delete
		    *augmented-start-g-symbol*
		    ;; Now keep the end-g-symbol
		    ;;(delete
		    ;; *the-end-g-symbol*
		    (delete
		      '()
		      (mapcar #'(lambda (gs)
				  (if (g-symbol-non-terminal? gs)
				      '()
				      gs))
			      (reverse *symbols*)))))))
      
      ;; dump out info on the productions.  the order follows the
      ;; productions indices in the parse tables.  each element is a
      ;; list of the index of the lhs grammar symbol and the length of
      ;; the rhs of the production
      (format stream ";;; ***PRODUCTIONS***~2%~
                      (setq ~s ~%'#~S)~3%"
	      '*PRODUCTION-INFO*
	      (mapcar
		#'(lambda (prod) (list (g-symbol-index (lhs prod))
				       (length (rhs prod))))
		(reverse *productions*)))
      
      ;; dump out a representation of the action function
      (format stream ";;; ***ACTION FUNCTION***~2%~
                      (setq ~s '#(" 
	      '*ACTION-TABLE*)
      (mapcar
	#'(lambda (ae)
	    (little-list-print
			   (oset-item-list ae) stream))
	(vector->list *action-array*))
      (format stream "))~3%")
      
      ;; dump out a representation of the goto function for non-terminals
      (format stream ";;; ***GOTO FUNCTION FOR NON-TERMINALS***~2%~
                      (setq ~s '#("
	      '*GOTO-TABLE*)
      (mapcar
	#'(lambda (ge)
	    (little-list-print
			   (mapcar #'pair-to-list (oset-item-list ge))
			   stream))
	(vector->list *goto-array*))
      (format stream "))~3%")
      
      
      ;; dump the index of the start state
      (format stream ";;; ***START STATE INDEX~2%~
                      (setq ~s ~s)~3%"
	      '*LR-PARSER-START-STATE-INDEX*
	      *start-state-index*)
      
      ;; dump the index of the end symbol
      (format stream ";;; ***END SYMBOL INDEX***~2%~
                      (setq ~s ~s)~3%"
	      '*END-SYMBOL-INDEX*
	      (g-symbol-index *the-end-g-symbol*))
      
      ;; dump the client lambda functions
      ;; switched to prin1 to get around lucid bug.
      (format stream ";;; ***LAMBDA FUNCTIONS***~2%~
                      (setq ~s ~%"
	      '*CLIENT-LAMBDAS*)
      (prin1 `(coerce (list ,@(reverse *lambdas*)) 'vector) stream)
      (format stream ")~3%")

      
      ;; Print out DOCUMENTATION for table, if *DOCUMENTATION* is T.

      (when *DOCUMENTATION*
	
	(format stream "~2%#||~%")		;begin documentation
	
	;; Insert comments about documentation here...
	
	
	(cruise-symbols-0 stream)		;index, symbols,
						;own productions |
						;terminal symbol.


	(format stream "~4%")			;white space...
      
	(cruise-parse-tables stream)		;state, kernel, closure,
						;actions, gotos.

	;; Put other stuff here...


	(format stream "||#~%")			;end documentation
      ))))


;;;	Prints out LIST to STREAM.

(defun little-list-print (list stream)
  (format stream "~&#~:s" list)) 


      

;;; PAIR-TO-LIST
;;; ============
;;;
;;;	Converts a dotted pair to a list.

(defun pair-to-list (p)
       (list (car p) (cdr p)))



;;;;;;;;;;;;;
;;; testing
;(comment-out
;  (progn 
;    (split "gramlam1" "gram1" "lambdas1")
;    (lalr1-tables-from-grammar "gram1")
;    (dump-tables "tables1")))



