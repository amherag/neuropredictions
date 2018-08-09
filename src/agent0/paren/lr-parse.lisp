;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ParEn -*-
						

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
;;;			   LR Parser Driver
;;;
;;;			      Sandy Wells
;;;
;;;		      converted to Common LISP by
;;;			   Stuart M. Shieber
;;;
;;; converted: 4/25/87 13:07:53
;;; modified: 3/28/88 17:21:52		Declare and bind global
;;; 					variables *LHS* and *STACK* so
;;; 					that the client lambdas can
;;; 					refer to them.  BP
;;;
;;; 	      5/16/88 17:56:51		Corrected documentation on
;;; 	      				lookup-fn to include the fact
;;; 	      				that it also returns T if ATOM
;;; 	      				is a terminal.  BP
;;; 
;;;           Jun 13, 1988 -- SW,       Cleaned up documentation a bit.
;;;                                     Changed the format of the parse
;;;                                     table files so that they may be
;;;                                     compiled as files.  That's why
;;;                                     they have all the setqs in them.
;;; 
;;;           Jun 20, 1988 -- SW,       Changed from returning lookup function
;;;                                     to returning terminal-hash-table.
;;;=====================================================================


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :lr-parse)
(in-package :ParEn
	    :use '(:user :lisp))


(export '( make-lr-parser *lhs* *stack* ))

(require :util
	 (lookup-pathname :util))


;;; *TRACE-LR*
;;; ==========
;;;
;;;	Flag controls display of the step-by-step operation of the parser.

(defvar *trace-lr* nil)


;;; *LHS*, *STACK*
;;; =====  =======
;;; 
;;;     These will be dynamically bound with the current lhs and client stack
;;;     so that the client lambdas may access those things.

(defvar *lhs*)
(defvar *stack*)


;;; These dynamic variables are used temporarily as parse table files are loaded

(defvar *lexicon*)
(defvar *preterminal-indices*)
(defvar *terminal-indices*)
(defvar *production-info*)
(defvar *action-table*)
(defvar *goto-table*)
(defvar *lr-parser-start-state-index*)
(defvar *end-symbol-index*)
(defvar *client-lambdas*)




;;; MAKE-LR-PARSER
;;; ==============
;;;
;;;	Read in a set of parse tables as written by dump.lisp

(defun make-lr-parser (filename)
  
  ;; A lexical closure is constructed holding the following, and more,
  ;; 
  ;; LEXICON is a vector indexed by the "grammar symbol
  ;; indices" that the parser uses of strings or lisp symbols
  ;; which are the instantiations of the grammar symbols
  ;;
  ;; PRETERMINAL-INDICES is a list of the grammar symbol indices for
  ;; those symbols which are preterminals in the grammar, as
  ;; declared in the grammar definition file.
  ;;
  ;; TERMINAL-INDICES is a list of the grammar symbol indices
  ;; indicating which among them are terminals (or preterminals)
  ;; symbols
  ;;
  ;; PRODUCTION-INFO is a vector indexed by the production
  ;; indices used by the parser of two element lists: the first
  ;; elements index the symbols which are the lhs of the
  ;; productions, the second elements indicate the lengths of
  ;; the productions
  ;;
  ;; ACTION-TABLE is a vector indexed by the state indices
  ;; used by the parser.  each state's entry is a vector whose
  ;; elements represent defined entries in the action parsing
  ;; function. these entries are 3 element lists whose first
  ;; elements are the indices of the grammar symbol argument to
  ;; the action parsing function.  the second elements in the
  ;; lists are an encoding of the action function: :s for shift,
  ;; :r for reduce, :a for accept.  the third elements are
  ;; production or next state indices as approprite.  the three
  ;; element lists appear in their surrounding vectors sorted on
  ;; their cars.
  ;;
  ;; GOTO-TABLE is arranged similar to action-table but has
  ;; two element lists instead of three.  the second elements in
  ;; the lists are the index of the state to goto. 
  ;;
  ;; CLIENT-LAMBDAS is a vector of procedures, indexed
  ;; by production index, which correspond to productions
  ;; in the grammar.  The client lambdas are what the
  ;; parser calls to do syntax directed something by side
  ;; effect.
      
  ;; This binds the parse tables to the global variables above by side effect.

  (load filename)

  ;; Now transfer the tables from the globals to some lexical variables.
  ;; These lexical variables are bound up in a closure with the parser.  

  (let* ((lexicon *lexicon*)
	 (preterminal-indices *preterminal-indices*)
	 (terminal-indices *terminal-indices*)
	 (production-info *production-info*)
	 (action-table *action-table*)
	 (goto-table *goto-table*)
	 (lr-parser-start-state-index *lr-parser-start-state-index*)
	 (end-symbol-index *end-symbol-index*)
	 (client-lambdas *client-lambdas*)

	 ;; This stuff gets computed from the transferred stuff above.
	 (terminal-hash-table (make-hash-table :test #'equal :size 30))
	 (next-possibilities-table
	  (map 'vector #'(lambda (action-entry)
			   (map 'list #'(lambda (vector)
					  (elt vector 0))
				action-entry))
	       action-table)))
    
    ;; Put the terminal symbols (as strings) in a hash table for
    ;; reasonably efficient lookup of their associated indices.  Also store
    ;; whether or not the symbol is a terminal (as opposed to preterminal).

    (mapc #'(lambda (index)
	      (setf (gethash (aref lexicon index)
			     terminal-hash-table)
		    ;; Keep the index and whether it's a terminal
		    ;; stored under the symbol name
		    (list index (not (member index preterminal-indices)))))
	  terminal-indices)

	
    (values
	  
     ;; The LR Parser
     ;; 
     ;; Function of two arguments, a function providing a token stream and
     ;; an error function.  
     ;;
     ;;	NEXT-SYM-FN(&optional next-possibilities):
     ;;
     ;;	This function optionally takes a list of the indices of
     ;;	lexicon elements that can occur next in the token stream and
     ;;	returns a token/index pair corresponding to the next token
     ;;	read.  The optional argument can be used to perform a kind of
     ;;	simulated lookahead, choosing the preterminal index of the
     ;;	token depending on the previous context.
     ;;
     ;;	ERR-FN(ctl-string &rest args):
     ;;
     ;;	Called on error in the input token stream with arguments like
     ;;	format.
     ;;
     ;; On a successful parse, the function returns the top of
     ;; the client stack, corresponding to the object the
     ;; client lambdas associated with the expression as a
     ;; whole.  On an unsuccessful parse, the ERR-FN is called
     ;; at some point.  What happens on returning from the
     ;; ERR-FN is relatively random.
     ;;
     ;; symbol-stack and state-stack are the standard
     ;; things for an lr parser.  A parallel stack, the
     ;; client-stack is kept which holds the objects
     ;; associated with the expressions in the symbol stack.
     ;; The client lambdas and client stack are used in the
     ;; following fashion:
     ;;
     ;; When a shift action occurs, the instantiation of the
     ;; input symbol is pushed onto the client stack.
     ;;
     ;; When a reduce action occurs, as many items as are on
     ;; the lhs of the associated production are popped from
     ;; the client stack and the corresponding client lambda is
     ;; applied to the popped items.  The result of the
     ;; application is then pushed onto the client stack.  One
     ;; may of course do whatever one wishes by side effect. 
	  
     #'(lambda (next-sym-fn err-fn)
	 (let ((symbol-stack nil)
	       (state-stack (list lr-parser-start-state-index))
	       (client-stack nil))
	     
	   (do ((input-symbol-stuff (funcall next-sym-fn 
					     (aref next-possibilities-table
						   (car state-stack))))
		(done nil)
		(input-symbol-index) 
		(input-symbol-instantiation)
		(action-entry))
		    
	       (done (car client-stack))
		  
	     (if (null input-symbol-stuff)
		 (funcall err-fn "internal parser error: unknown symbol"))
	     (setf input-symbol-index (cdr input-symbol-stuff))
	     (when *trace-lr*
	       (print-config state-stack symbol-stack input-symbol-stuff lexicon))	    
	     (setf input-symbol-instantiation (car input-symbol-stuff))

	     (setf action-entry (vec-bs-assoc input-symbol-index
					      (aref action-table
						    (car state-stack))))
	     (when (null action-entry) 
	       (print-config state-stack symbol-stack input-symbol-stuff lexicon)	    
	       (funcall err-fn "Symbol ~a (code ~a; spelling ~s) not allowed here."
			(aref lexicon input-symbol-index) 
			input-symbol-index 
			input-symbol-instantiation))
	     (cond ((eq (cadr action-entry) :s)	; shift
		    (when *trace-lr*
		      (format t "--> shifting~%"))
		    (push input-symbol-index symbol-stack)
		    (push (caddr action-entry) state-stack)
		    (push input-symbol-instantiation client-stack)
		    (setf input-symbol-stuff  (funcall next-sym-fn 
						       (aref next-possibilities-table
							     (car state-stack)))))
		   ((eq (cadr action-entry) :r)	; reduce
		    (let* ((prod-index (caddr action-entry))
			   (prod-len (cadr (aref production-info
						 prod-index))))
		      (when *trace-lr*
			(format t "--> reducing by (#~d)~%" 
				prod-index))
		      (popn prod-len symbol-stack)
		      (popn prod-len state-stack)
		      (push (car (aref production-info
				       prod-index)) symbol-stack)	
		      (let ((goto (cadr (vec-bs-assoc (car symbol-stack)
						      (aref
						       goto-table
						       (car state-stack))))))
			(if (null goto) 
			    (funcall err-fn "table error? goto not defined!"))
			(push goto state-stack))
		      ;; Let bind some dynamics so that the user
		      ;; can get at them from within the client
		      ;; lambda.
		      (let ((*lhs* (aref lexicon (car
						  symbol-stack)))
			    (*stack* client-stack))
			;; Execute the client lambda.
			(push (apply (aref client-lambdas prod-index)
				     (popn prod-len client-stack))
			      client-stack))))
		   ((eq (cadr action-entry) :a)	; accept on end symbol
		    (if (= input-symbol-index end-symbol-index) 
			(setf done t)
			(funcall err-fn "extra input?")))
		   (t (funcall err-fn "Unknown action ~s." (cadr action-entry)))))))
	  
     ;; Lexicon Interface
     ;;
     ;; A function that returns two values, INDEX and TERMINAL?
     ;; INDEX will be non nil only if the token was found in
     ;; the terminal-hash-table.  In this case, INDEX will be the index
     ;; of the grammar symbol and TERMINAL? will be true if the 
     ;; token is NOT PRETERMINAL.  
     ;; If the token isn't in the terminal-hash-table, both values
     ;; will be NIL.  
	  
     terminal-hash-table)))


;;; PRINT-CONFIG
;;; ============
;;;
;;;	Prints the current configuration of the LR parser, including the last few
;;;	elements of the stacks.

(defun print-config (state-stack symbol-stack input-symbol-stuff lexicon)
  (let ((print-stack (mapcar #'list (reverse (cdr state-stack))
			     (reverse (mapcar #'(lambda (sym)
						  (aref lexicon sym))
					      symbol-stack)))))
    (format t "~& ~70:<~:{(~a) ~a ~}(~a) ~>| ~a (= ~a)~100t " 
	    (if (< (length print-stack) 5)
		print-stack
		(subseq print-stack
			(- (length print-stack) 5)
			(length print-stack)))
	    (car state-stack)
	    (car input-symbol-stuff)
	    (cdr input-symbol-stuff))))
