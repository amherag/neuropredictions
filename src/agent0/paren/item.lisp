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
;;;                      Item.lisp
;;;
;;;                    Sandy Wells
;;;
;;;             Converted to Common LISP by
;;;                     Barney Pell
;;;
;;;
;;; converted:  8/13/87 4:44:53
;;; 
;;; Jun  3, 1988 -- Sandy Wells: 
;;; 	added a function quote: (lucid compiler choked)
;;; 			#'(lambda (gs)
;;; 
;;;================================================================================

;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :item)
(in-package :ParEn
	    :use '(:lisp :user))

(require :util
	 (lookup-pathname :util))

(require :g-symbol
	 (lookup-pathname :g-symbol))
(require :slrpgram
	 (lookup-pathname :slrpgram))

;;; item.s
;;;
;;; (changed for lalr(1))
;;;
;;; LR(1) ITEMS
;;; ===========
;;; These are  represented by structs:
;;;
;;; AFTER-DOT is a integer which indexes the symbol in the
;;; production that comes after the dot.
;;;
;;; LOOK-AHEADS is an oset of grammar symbols.  The item data structure
;;; essentially stands for the set of lr(1) items which are the same
;;; except for each having one lookahead symbol from the set
;;; look-aheads.
;;;
;;; LOOK-AHEAD-DEPENDERS is an oset of items to whom lalr(1) lookaheads
;;; propagate from this item

(defstruct (item (:print-function item-print))
                  production
                  after-dot
                  (look-aheads (make-oset :order-fn #'g-symbol-order-function))
		  (look-ahead-dependers
                    (make-oset :order-fn #'item-order-function)))


;;; DOT-AT-RIGHT-END? 
;;; =================
;;;	
;;;	Handy predicate.
(defsyntax (dot-at-right-end? item)
        (= (- (length (item-production item)) 1) (item-after-dot item)))


;;; SYMBOL-AFTER-DOT
;;; ================
;;;
;;;	Get the symbol after the dot -- nil if dot is flushright.

(defun symbol-after-dot (item)
       (if (dot-at-right-end? item)
           nil
           (nth (+ 1 (item-after-dot item)) (item-production item) )))



;;; ADVANCE-DOT
;;; ===========
;;;
;;; 	Returns ITEM with the dot moved one to the right, or NIL if dot
;;; 	gets past the end.  Since this is used during lr(0) set
;;; 	construction, it only deals with PRODUCTION and AFTER-DOT slots,
;;; 	the others are filled in as () by default. 
	
(defun advance-dot (item)
       (if (dot-at-right-end? item) nil
           (make-item :production (item-production item)
                      :after-dot (+ 1 (item-after-dot item)))))




;;; NEW-ITEM
;;; ========
;;;
;;; 	Make an item which has the dot at the left end of the rhs.

(defun new-item (production)	
       (make-item :production production
                  :after-dot 1))
				

;;; ITEM-ORDER-FUNCTION
;;; ===================
;;;
;;;	The order function for items. Returns :correct-order,
;;;	:wrong-order, :equal, or error.  This is used during lr(0) sets
;;;	of items construction.  Only the PRODUCTION and AFTER-DOT fields
;;;	are tested, since these characterize lr(0) items.

(defun item-order-function (ia ib)
  (let
    ((prod-in-a (production-index (item-production ia)))
     (prod-in-b (production-index (item-production ib)))
     (item-after-dot-a (item-after-dot ia))
     (item-after-dot-b (item-after-dot ib)))
    (cond ((< prod-in-a prod-in-b)
	   :correct-order)
	  ((> prod-in-a prod-in-b)
	   :wrong-order)
	  ((< item-after-dot-a item-after-dot-b)
	   :correct-order)
	  ((> item-after-dot-a item-after-dot-b)
	   :wrong-order)
	  (t :equal))))


;;; ITEM-PRINT
;;; ==========
;;;
;;;	Print function for ITEM data type.  Prints item to stream in the
;;;	following format:  #<ITEM E -> . T + F >.  When pretty is true,
;;;	prints as:  E -> .T + F .   Only prints the lr(0) parts and the lookaheads.

(defun  item-print (item &optional (stream t) (depth 0) (pretty nil))
  (if (and *print-level* (>= depth *print-level*))
      (format stream "#")
      (progn
	(unless pretty
	  (format stream "#<ITEM "))
	(format stream "~a -> "
		(g-symbol-print-name (lhs (item-production item))))
	(do ((ncdr (rhs (item-production item)) (cdr ncdr))
	     (i 1 (+ 1 i)))
	    ((null ncdr)
	     (progn (if (equal (item-after-dot item) i) (format stream ". "))
		    (unless (oset-empty? (item-look-aheads item))	;(unless (null (item...)))
		      (format stream ",")
		      (oset-for-each
			#'(lambda (gs)
                               (format stream "~a " (g-symbol-print-name gs)))
			(item-look-aheads item)))))
	  (if (equal (item-after-dot item) i) (format stream ". "))
	  (format stream "~a "
		  (g-symbol-print-name (car ncdr))))
	(unless pretty
	  (format stream ">"))
	)))


;;; ITEM-LIST-PRINT
;;; ===============
;;;
;;;	Print out a list of items to stream.

(defun item-list-print (item-list &optional (stream t))
       (mapc
         #'(lambda (item)
           (format stream "~&")
           (item-print item stream))
         item-list)
       (format stream "~%"))





