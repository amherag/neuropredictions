;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ParEn; Base: 10 -*-


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
;;;                    LALR1.lisp
;;;
;;;               Sandy Wells
;;;
;;;        Converted to Common LISP by 
;;;             Barney Pell
;;;
;;; converted:  12/02/87 11:26:15
;;;================================================================================                              

;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :lalr1)
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
(require :lr0-sets
	 (lookup-pathname :lr0-sets))
(require :slrpgram
	 (lookup-pathname :slrpgram))
(require :tables
	 (lookup-pathname :tables))





;;; larl1.s
;;;
;;; discover and propagate lalr(1) look-aheads among members of lr(0)
;;; collection.

;;; This algorithm for propagating lalr(1) lookaheads is a straightforward
;;; recursive version of the algorithm sketched in section 6.9 of the (older)
;;; dragon book @u(Principles of Compiler Design) by A.V. Aho and J.D Ullman.
;;; The major drawback of this algorithm is that it may be somewhat wasteful
;;; of space.  With modern address spaces who cares?
;;; Basically, it crawls around on the lr(0) item sets and as it goes,
;;; it discovers both lookheads which are "spontaneously" generated for
;;; an item set, and item sets to whom lookaheads propagate.  The doubly
;;; recursive way of implementing this is similar to the method used
;;; in calculating first sets in first.s




;;; LALR1-DO-LOOKAHEADS
;;; ===================
;;;
;;;	The names are getting a bit confusing here.  This
;;;	function transforms the data structure *item-set-collection* from
;;;	being the lr(0) collection to  the lalr(1) collection.


(defun  lalr1-do-lookaheads ()
  (when *verbose*
    (format t "~&Doing LALR(1) lookaheads.~%"))
  ;; introduce a "dummy" terminal symbol which is used as a hack in 
  ;; lookahead calculations
  (let ((dummy-g-symbol (new-g-symbol "<dummy>" -1)))
    ;; the dummy symbol is terminal and must be in its own first set
    (oset-insert! dummy-g-symbol (g-symbol-first-set dummy-g-symbol))
    ;; map over all the kernel items
    (oset-for-each
      #'(lambda (item-set)
	  (oset-for-each
	    #'(lambda (kernel-item)
		;; special case: the end symbol is a lookahead for the start 
		;; production
		(if (equal *start-state-index* (item-set-index item-set))
		    ;; there had better only be one item in this set!
		    (lalr1-add-lookahead *the-end-g-symbol* kernel-item))
		
		;; here we use the hack in dragon 6.9 (fig 6.20) of using lr(1)
		;; closure with a dummy grammar symbol to discover propagated
		;; and spontaneous lookaheads for a lr(0) kernel item.  The
		;; funny-closure-items are in J' of the figure.
		
		(oset-for-each
		  #'(lambda (funny-closure-item)
		      (unless
			;; nothing to do 
			(oset-empty? (item-look-aheads funny-closure-item))
			(let ((goto-item-proto (advance-dot funny-closure-item)))
			  (when
			    goto-item-proto
			    ;; here we go to some expense to locate the goto set
			    ;; for an item.  
			    ;; These should be pre-computed and cached instead. 
			    (let ((goto-item
				    (oset-find
				      goto-item-proto
				      (item-set-kernel
					(find-goto-set
					  item-set
					  (symbol-after-dot funny-closure-item)))
				      "internal error - failed to find goto item")))
			      (oset-for-each
				#'(lambda (lookahead)
				    (if (eq lookahead dummy-g-symbol)
					;; discovered lookahead propagation
					(lalr1-add-depender goto-item kernel-item)
					;; discovered lookahead 
					(lalr1-add-lookahead lookahead goto-item)))
				(item-look-aheads funny-closure-item)))))))
		  ;; the set of "funny" closure items. J'. 
		  (single-item-closure-1 (copy-lr0-item kernel-item)
					 dummy-g-symbol)))
	    (item-set-kernel item-set))
	  (princ "."))
      *item-set-collection*))

  
  ;; New Stuff  1/27/88					      
  (format t "~&")
  (reclose-item-sets)
)					       





;;; LALR1-ADD-DEPENDER
;;; ==================
;;;
;;;	This is used when we discover that lookaheads propagate from
;;;	one lr(0) item set to another during the calculation of lalr(1)
;;;	sets of items.  Add a link to the dependency digraph and
;;;	propagate the lookaheads we already know about.

(defun lalr1-add-depender (propagate-to propagate-from)
  (if
    (oset-insert! propagate-to (item-look-ahead-dependers propagate-from))
    (oset-for-each
      #'(lambda (gs)
        (lalr1-add-lookahead gs propagate-to))
      (item-look-aheads propagate-from))))




;;; LALR1-ADD-LOOKAHEAD
;;; ===================
;;;
;;;	This is used when we discover a lookhead for an lr(0) item set
;;;	during ;;; the calculation of lalr(1) sets.  If the lookahead
;;;	wasn't already there, ;;; add it, and also add it to the
;;;	"dependers": those item sets to whom lookaheads  propagate
;;;	from the item in question.

(defun lalr1-add-lookahead (symbol item)
  (if
    (oset-insert! symbol (item-look-aheads item))
    ;; wasn't already there
    (oset-for-each
      #'(lambda (depender)
        (lalr1-add-lookahead symbol depender))
      (item-look-ahead-dependers item))))






;;; FIND-GOTO-SET
;;; =============
;;;
;;;	Should go away eventually, when item gotos are cached.

(defun  find-goto-set (item-set symbol)
  (cdr (oset-find
         (cons symbol nil)
         (item-set-goto-map item-set)
         "find-goto-set failed to find the goto set")))


;;; COPY-LR0-ITEM
;;; =============
;;;
;;;	

(defun  copy-lr0-item (i)
  (make-item :production (item-production i)
             :after-dot (item-after-dot i)))


;;; RECLOSE-ITEM-SETS
;;; =================
;;;
;;;	Destructively closes the item-sets in *item-set-collection* under lr1
;;;	closure.   

(defun reclose-item-sets ()
  (oset-for-each
    #'(lambda (item-set)
      (closure-1! (item-set-closure item-set))
      (princ "."))
    *item-set-collection*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test
;(comment-out
;  (progn
;;    (tables-from-grammar "gram6")
;    (tables-from-grammar "gram4")
;    (princ "symbols: ") (newline)
;    (cruise-symbols-2)
;    (princ "productions: ") (newline)
;    (print-productions)
;    (princ "lr0 item sets: ") (newline)
;    (collection-print-kernels *item-set-collection*)
;    (princ "lalr(1) tables: ") (newline)
;    (cruise-parse-tables)
;    ))

