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
;;;                    TABLES.lisp
;;;
;;; 
;;; Construct lalr(1) or slr(1) parse tables.  The hard part must already
;;; have been done. Namely: the calculation of nullable symbols, the
;;; first and follow sets, and the lr0 collection.  In the case of lalr(1),
;;; lookahead propagation must also have been done.
;;; 
;;; 
;;;               Sandy Wells
;;;
;;;        Converted to Common LISP by 
;;;             Barney Pell
;;;
;;; converted:  12/02/87 10:55:01
;;; modified:	4/04/88 23:31:09 		Wrote  precedence
;;; 						rules for resolving
;;; 						conflicts.  Also
;;; 						documents conflicts  in
;;; 						tables.  BP
;;; 
;;; Jun 13, 1988 -- SW                          Cleaned up documentation a bit.
;;;================================================================================
;;; 
;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :tables)
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




;;; TABLES.LISP
;;;
;;; on the representation of parsing tables:
;;;
;;; ACTION FUNCTION will be an array, indexed by the state number,
;;; of functions of grammar symbols represented as osets of
;;; 3 element lists containing a g-symbol index, the keyword 
;;; :s, :r, or :a for shift reduce or accept, and an integer encoding the
;;; next state, or production index as appropriate.
;;;
;;; GOTO FOR NON-TERMINALS will be represented by a parallel array
;;; of osets of pairs whose cars are g-symbol indices, and whose
;;; cdrs are state indices.

(defvar *ACTION-ARRAY*)
(defvar *GOTO-ARRAY*)




;;; INTEGER-FUNCTION-ORDER-FUNCTION
;;; ===============================
;;;
;;;	An oset order function for parse table entries

(defun INTEGER-FUNCTION-ORDER-FUNCTION (a b)
       (integer-order-function (car a) (car b)))


;;; BUILD-PARSE-TABLES
;;; ==================
;;;
;;;	Build the parse tables.  The only case in which it returns
;;;	failure is if it detects a conflict and you didn't tell it to
;;;	continue on error.  

(defun  build-parse-tables (doing-lalr1 continue-on-error)
  (let ((error-detected nil))
    (setf *conflicts* nil)			;start out without conflicts
    (setf *action-array* (make-array *item-set-count*))
    (setf *goto-array* (make-array *item-set-count*))
    (do ((i 0 (+ i 1)))
	((= i *item-set-count*))
      (setf (aref *action-array* i)
	    (make-oset :order-fn #'integer-function-order-function))
      (setf (aref *goto-array* i)
	    (make-oset :order-fn #'integer-function-order-function)))
    (flet ((try-to-insert (g-sym-index action-char index item-set)
	     (let ((conflict (not (parse-table-insert! g-sym-index
						       action-char
						       index
						       item-set
						       continue-on-error ))))
	       (when conflict
		 (setq error-detected t)
		 (unless continue-on-error
		   (return-from build-parse-tables nil))))))	;Build parse tables  has failed.
      
      
      (oset-for-each
	#'(lambda (item-set)
	    (oset-for-each
	      #'(lambda (goto-elt)		; car of goto-elt is g-sym, cdr is item-set
		  (if (g-symbol-non-terminal? (car goto-elt))
		      (oset-insert! (cons (g-symbol-index (car goto-elt))
					  (item-set-index (cdr goto-elt)))
				    (aref *goto-array*
					  (item-set-index item-set)))
		      (try-to-insert (g-symbol-index (car goto-elt))
				     :S
				     (item-set-index (cdr goto-elt))
				     item-set)))
	      (item-set-goto-map item-set))
	    (oset-for-each
	      #'(lambda (closure-item)		;Could these be kernel items?
		  (if (dot-at-right-end? closure-item)
		      (if (eq *augmented-start-g-symbol*
			      (lhs (item-production closure-item)))
			  (try-to-insert (g-symbol-index *the-end-g-symbol*)
					 :A 0 item-set)	; accept, bogus 0
			  (oset-for-each
			    #'(lambda (follower)
				(try-to-insert (g-symbol-index follower)
					       :R
					       (car (item-production
						      closure-item))
					       item-set))
			    ;; here is the only difference between slr and lalr1
			    ;; (in the table construction phase)
			    (if doing-lalr1
				(item-look-aheads closure-item)
				(g-symbol-follow-set
				  (lhs (item-production closure-item))))))))
	      (item-set-get-closure! item-set))
	    )
	*item-set-collection*)
      (not error-detected))))			;success or failure


;;; PARSE-TABLE-INSERT!
;;; ===================
;;;
;;;	An auxilliary function for adding an entry to a parse table.  If
;;;	there are conflicts, deal with them and return NIL.  Otherwise,
;;;	return T.

(defun  parse-table-insert! (g-sym-index action-char index item-set 
			     continue-on-error)
  (let* ((state-index (item-set-index item-set))
	 (action-for-item-set (aref *action-array*
				    state-index))
	 (to-insert (list g-sym-index action-char index))
         (res (oset-insert-2! to-insert action-for-item-set)))
    
    (if (car res) T				;no conflict
	(progn
	  (deal-with-conflicts (cdr res) to-insert state-index
			     action-for-item-set continue-on-error)
	  nil))))


;;; *CONFLICTS*
;;; ===========
;;;
;;;	An alist of the form:
;;; 		((state1 . (entry1 entry2)))
;;;	The entries are those which caused conflicts and were not
;;;	selected.

(defvar *CONFLICTS* nil)



;;; DEAL-WITH-CONFLICTS
;;; ===================
;;;
;;;	When there is an action conflict, print a message indicating
;;;	this.  Then, either abandon table construction (if
;;;	CONTINUE-ON-ERROR is NIL), or apply precedence rules to choose
;;;	which entry to store in the action table.  If the one which was
;;;	there already is chosen, then leave it.  Otherwise, insert the
;;;	new one.  Finally, append the state and conflict data onto the
;;;	*CONFLICTS* a-list, so that they can be printed in the
;;;	documentation.  

(defun deal-with-conflicts (old-entry new-entry state-index
			    action-for-item-set continue-on-error)
  (let
    ((g-sym-index (first old-entry))
     (action1 (second old-entry))
     (action2 (second new-entry))
     (state-or-prod1 (third old-entry))
     (state-or-prod2 (third new-entry))
     (chosen-entry old-entry)
     (unchosen-entry new-entry))
  
    (format t  "~&Action conflict -- State: ~S~% Grammar-symbol: ~A~& Entries: ~&~A~A~%"
	    state-index
	    (get-print-name g-sym-index)
	    (print-table-entry old-entry nil)
	    (print-table-entry new-entry nil))
    
    ;; If we encountered this conflict and have no further options
    ;; to resolve it (i.e., we can't try LALR1 instead of SLR),
    ;; then apply the following conflict-resolution rules:
    ;; 	  On SHIFT/REDUCE conflicts, SHIFT.
    ;;    On REDUCE/REDUCE conflicts, REDUCE by the LOWER
    ;;    PRODUCTION.
    ;;    On SHIFT/SHIFT conflicts, SHIFT to the LOWER STATE.
    ;;	  (Note:  There should NEVER be any SHIFT/SHIFT conflicts!)

    (when continue-on-error
      (if (< state-or-prod2 state-or-prod1)	; new-entry is lower state or production
	  (if (or (eq action1 action2)		; SHIFT/SHIFT or REDUCE/REDUCE conflict
		  (eq action1 :r))		; SHIFT/REDUCE conflict (current entry is REDUCE)
	      (setq chosen-entry new-entry))
	  (if (and (eq action1 :r)		; SHIFT/REDUCE conflict (current entry is REDUCE)
		   (eq action2 :s))
	      (setq chosen-entry new-entry)))
      (format t "~&  Choosing entry: ~A~%"
	      (print-table-entry chosen-entry nil))
      ;; If we choose the entry that was already there, then  leave it.
      ;; Otherwise, replace the new one for it.  Add UNCHOSEN-ENTRY 
      ;; to the *CONFLICTS* entry for this state, since it
      ;; will not be displayed in the table.  
      (unless (eq chosen-entry old-entry)
	  (progn
	    (setq unchosen-entry old-entry)
	    (oset-insert-2! chosen-entry
			    (oset-delete old-entry
					 action-for-item-set))))
      ;; *CONFLICTS* is an alist of the form:
      ;; 	((state1 . (entry1 entry2)))
      ;; If there is already a list of conflicts for this state, append
      ;; the unchosen-entry to it.  Otherwise, create this pair.
      (let ((state-conflicts (cdr (assoc state-index *conflicts*))))
	(if state-conflicts
	  (setf state-conflicts
		(cons unchosen-entry state-conflicts))
	  (setq *conflicts*
		`((,state-index . (,unchosen-entry)) . ,*conflicts*))))))
  nil)



;;; PRINT-TABLE-ENTRY
;;; =================
;;;
;;;	Prints action-table entries in the following format:
;;;		G-SYMBOL:   ACTION INDEX
;;;	Where INDEX is the state to be SHIFTed to or the index of the
;;;	production to be REDUCEd by.

(defun print-table-entry (action-elt &optional (stream t))
  (format stream "~&     ~a : ~a ~a~%"
	  (get-print-name (first action-elt))
	  (get-action-string (second action-elt))
	  (third action-elt)))
          


;;; PRINT-CONFLICTS
;;; ===============
;;;
;;;	Prints to STREAM the action-conflicts in STATE which were not
;;;	chosen.  

(defun print-conflicts (state &optional (stream t))
  (let ((conflicts (cdr (assoc state *conflicts*))))
	(when conflicts
	  (format stream
		  "~&~%~%*** CONFLICTS ***~%*** --------- ***~%~%")
	  (mapcar
	    #'(lambda (entry)
		(format stream "~&~A~%"
			(print-table-entry entry nil)))
	    conflicts)
	  (format stream "~%~%~%"))))



;;; GET-PRINT-NAME
;;; ==============
;;;
;;;	Gets the print-name of the g-symbol with index INDEX.

(defun GET-PRINT-NAME (index)
       (g-symbol-print-name (aref *symbol-array* index)))


;;; GET-ACTION-STRING
;;; =================
;;;
;;;	Converts from the keyword used in parsing to represent an
;;;	action into the english for the action, for purposes of clarity
;;;	in testing and documentation.

(defun get-action-string (action-elt)
  (case action-elt
    (:a "accept")
    (:r "reduce")
    (:s "shift")
    (otherwise nil)))



;;; CRUISE-PARSE-TABLES
;;; =================
;;;
;;;   Prints to stream the start-state, actions, and gotos for all of
;;;   the lr0 items.  If closure? is T, then also print the kernel and
;;;   closure for each item.

(defun  CRUISE-PARSE-TABLES (&optional (stream t) (closure? t))
  (format stream  "~%~%====================~%start-state: ~a~%====================~%~%"
	  *start-state-index*)
  (do ((i 0 (+ 1 i)))
      ((= i *item-set-count*))
    (format stream "~&~%state: ~a~%----------~%~%"
	    i)

    (when closure?
      (progn
	(format stream "~&~%")
	(item-set-print-kernel
	  (aref
	    (oset->vector *item-set-collection*)
	    i)
	  stream)))

    (format stream "~&~%actions:~%----------~%")
    (oset-for-each				
      #'(lambda (action-elt)
	  (print-table-entry action-elt stream))
      (aref *action-array* i))
    (print-conflicts i stream)

    (format stream "~%~%gotos:~%----------~%")
    (oset-for-each
      #'(lambda (goto-elt)
          (format stream "~&     ~a : ~a     ~%"
		  (get-print-name (car goto-elt))
		  (cdr goto-elt))
          )
      (aref *goto-array* i))
    (format stream  "~%~%--------------------------------------------------~%")
    )
  nil)


;;;--------------------------------------------------------------------------------
;;; Test routines for the table building are found in SLR.lisp and LALR1.lisp.
;;;--------------------------------------------------------------------------------
