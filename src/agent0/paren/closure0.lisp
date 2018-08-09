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
;;;                    CLOSURE0.lisp
;;;
;;;                   Sandy Wells
;;;
;;;          Converted to Common LISP by
;;;                  Barney Pell
;;;
;;; converted:  8/13/87 4:52:50
;;;================================================================================


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :closure0)
(in-package :ParEn
	    :use '(:lisp :user))
(require :oset
	 (lookup-pathname :oset))

(require :g-symbol
	 (lookup-pathname :g-symbol))

(require :item
	 (lookup-pathname :item))



;;; CLOSURE
;;; =======
;;;
;;;	Calculate the closure of an lr(0) set of items.  I is an oset
;;;	of items.  This is  non-destructive.

(defun closure (I)
       (let ((eset (make-oset :order-fn #'item-order-function)))
         (oset-for-each
           #'(lambda (x) (closure-insert-item! x eset))
           I)
         eset))


;;; CLOSURE-INSERT-ITEM!
;;; ====================
;;;
;;;	Add an item to an oset of items. Add his pals too if he wasn't
;;;	there already 

(defun closure-insert-item! (item set)
       (if (oset-insert! item set)
           (progn
             (mapc #'(lambda (production)
		       (closure-insert-item! (new-item production) set))
                   (if (dot-at-right-end? item)
                       nil
                       (g-symbol-own-productions (symbol-after-dot item))))
             t)
           nil))
