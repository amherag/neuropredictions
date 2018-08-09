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
;;;                    CLOSURE1.lisp
;;;
;;;                   Sandy Wells
;;;
;;;          Converted to Common LISP by
;;;                  Barney Pell
;;;
;;; converted:  11/24/87 16:59:31
;;;================================================================================


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :closure1)
(in-package :ParEn
	    :use '(:lisp :user))
(require :oset
	 (lookup-pathname :oset))

(require :g-symbol
	 (lookup-pathname :g-symbol))

(require :item
	 (lookup-pathname :item))


;;;================================================================================
;;; closure1.s
;;;
;;; calculate the lr(1) closure of a set of lr(1) items.
;;; currently, find the closure of a set of one lr(1) item.
;;;
;;; an lr(1) item data structure with a set of lookaheads
;;; actually stands for a set of lr(1) items which are the
;;; same except for each having one lookahead from the set.



;;; SINGLE-ITEM-CLOSURE-1
;;; =====================
;;;
;;;	Returns the closure for a single item, given its look-ahead.

(defun single-item-closure-1 (lr0-item look-ahead)
  (let ((eset (make-oset :order-fn #'item-order-function)))
    (closure-1-insert-item! lr0-item look-ahead eset)
    eset))



;;; CLOSURE-1!
;;; ==========
;;;
;;;	Destructively take the lr(1) closure of an item set.  (This is
;;;	actually an oset of items... not an item-set structure). Empty
;;;	out the set and re-insert the contents with closures.

(defun closure-1! (item-set)
  (let* ((iset (oset-copy item-set)))
    (oset-empty! item-set)
    (oset-for-each
      #'(lambda (item)
	  (let ((the-look-aheads (item-look-aheads item)))
	    (setf (item-look-aheads item) 
		  (make-oset :order-fn #'g-symbol-order-function))
	    (oset-for-each
	      #'(lambda (look-ahead)
		  (closure-1-insert-item! item look-ahead item-set))
	      the-look-aheads)))
      iset))
  nil)



;;; CLOSURE-1-INSERT-ITEM!
;;; ======================
;;;
;;;	Insert an LR0-ITEM with its LOOK-AHEAD into ITEM-SET.

(defun  closure-1-insert-item! (lr0-item look-ahead item-set)
  (let* ((insertion-result (oset-insert-2! lr0-item item-set))
         (item-not-there-already (car insertion-result))
         (the-item (cdr insertion-result)))
    (when (or (oset-insert! look-ahead
                            (item-look-aheads the-item))
              item-not-there-already)
          ;; item wasn't already there with that lookahead
          ;; so insert his buddies too
          (mapc #'(lambda (prod)
                  (oset-for-each
                    #'(lambda (gs)
                      (closure-1-insert-item!
                        (new-item prod)
                        gs
                        item-set))
                    (first-seq
                      (append
                        ;; this gets the list corresponding to the part
                        ;; of the item beyond the symbol after the dot
                        (nthcdr (+ 2 (item-after-dot lr0-item))
				(item-production lr0-item))
                                   
                        (list look-ahead)))))
                (if (dot-at-right-end? lr0-item)
                    nil
                    (g-symbol-own-productions (symbol-after-dot
                                                lr0-item)))))))


;;; test:

;(comment-out
;  (progn
;    (slurp-grammar "gram4")
;    (calculate-empty-string-derivers)
;    (calculate-first-sets)
;    (setq f-item (new-item (car (reverse *productions*))))
;    (setq f-i-set (single-item-closure-1
;                      f-item *the-end-g-symbol*))
;    (item-list-print (oset-item-list f-i-set))
;))

