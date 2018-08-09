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
;;;                    LR0-SETS.lisp
;;;
;;;                  Sandy Wells
;;;
;;;          converted to Common LISP by 
;;;               Barney Pell
;;;
;;; converted:  8/13/87 5:44:30
;;; modified: 
;;;             Jun 13, 1988 -- Sandy changed names of internal grammar symbols to have bang
;;; 
;;;================================================================================


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :lr0-sets)
(in-package :ParEn
	    :use '(:lisp :user))

(require :util
	 (lookup-pathname :util))
(require :oset
	 (lookup-pathname :oset))
(require :item
	 (lookup-pathname :item))
(require :closure0
	 (lookup-pathname :closure0))
(require :closure1
	 (lookup-pathname :closure1))



;;; lr0-sets.lisp

;;; Representation for sets of items, and
;;; computing the canonical lr(0) collection of sets of items.
;;; Currently leaves the closures lying around on the sets
;;; of items, they could be flushed just after they are used.
;;; Gets hold of the grammar via the symbol with the name <START>
;;; and the application of G-SYMBOL-OWN-PRODUCTIONS to SYMBOLS.
;;; The grammar should have been previously internalized
;;; using SLURP-GRAMMAR (see slrp-gram.lisp).

(defvar *item-set-count*)
(defvar *item-set-collection*)
(defvar *start-state-index*)




;;; ITEM-SET
;;; ========
;;;
;;;	 A type for sets of items. The KERNEL will be an o-set of items,
;;;	 the CLOSURE might be an o-set, or might be nil if we are trying
;;;	 to save space.  GOTO-MAP will be a oset of pairs whose cars are
;;;	 grammar symbols and whose cdrs are item-sets.

(defstruct item-set
  index
  kernel
  (closure nil)
  goto-map)


;;; GOTO-MAP-ORDER-FUNCTION
;;; =======================
;;;
;;;	The ordering function for GOTO-MAPPINGS.

(defun goto-map-order-function (a b)
       (g-symbol-order-function (car a) (car b)))


;;; NEW-ITEM-SET
;;; ============
;;;
;;;	Makes a new item set, which has the kernel KERNEL.

(defun new-item-set (kernel)
       (make-item-set :kernel kernel
                      :goto-map (make-oset
                                  :order-fn #'goto-map-order-function)))



;;; ITEM-SET-ORDER-FUNCTION
;;; =======================
;;;
;;;	Item sets can be identified by looking at their kernels.

(defun item-set-order-function (a b)
       (oset-order-function (item-set-kernel a) (item-set-kernel b)))




;;; MAKE-LR0-COLLECTION
;;; ===================
;;;
;;;	Result is an oset of item-sets which comprise the canonical
;;;	lr(0) sets of items.


(defun make-lr0-collection ()
  (when *verbose*
    (format t "~&Making LR(0) sets of items.~&"))
  (let ((lr0-set (make-oset :order-fn #'item-set-order-function))
	(start-prod (car (g-symbol-own-productions
			   *augmented-start-g-symbol*)))
	(initial-kernel (make-oset :order-fn #'item-order-function))
	(initial-state 'bogus))
    (oset-insert! (new-item start-prod) initial-kernel)
    (setf initial-state (new-item-set initial-kernel))
    (lr0-insert-item-set! initial-state lr0-set)
    (setf *item-set-count* 0)
    (oset-for-each #'(lambda (is)
		       (setf (item-set-index is)
			     (post-inc *item-set-count*)))
		   lr0-set)
    (setf *start-state-index* (item-set-index initial-state))
    (when *verbose*
      (format t "~&~d item sets created.~%" *item-set-count*))
    (setf *item-set-collection* lr0-set)
    nil))




;;; LR0-INSERT-ITEM-SET!
;;; ====================
;;;
;;;	 ITEM-SET should be of that type. COLLECTION should be an o-set
;;;	 of item-sets. Returns a pointer to the item set in the
;;;	 collection.

(defun lr0-insert-item-set! (item-set collection)
  (let ((insertion-result (oset-insert-2! item-set collection)))
    (when (car insertion-result) ; item wasn't already there
          (princ ".")
          (mapc
            #'(lambda (subset) ; subset is an oset of items with same after dot
              (let ((goto-set (goto subset)))
                (unless (oset-empty? goto-set)
                        (oset-insert!
                          (cons (symbol-after-dot
                                  (car (oset-item-list subset)))
                                (lr0-insert-item-set! (new-item-set goto-set)
                                                      collection))
                          (item-set-goto-map item-set)))))
            (oset-select-subsets
              (item-set-get-closure! item-set)
              #'symbol-after-dot)))
    (cdr insertion-result)))




;;; ITEM-SET-GET-CLOSURE!
;;; =====================
;;;
;;; 	Returns the oset of items which is the closure of the item set,
;;; 	calculating it if need be from the kernel. Caches the closure in
;;; 	the closure slot

(defun item-set-get-closure! (item-set)
       (if (null (item-set-closure item-set))
           (setf (item-set-closure item-set)
                 (closure (item-set-kernel item-set))))
       (item-set-closure item-set))



;;; ITEM-SET-FLUSH-CLOSURE
;;; ======================
;;;
;;;	Release the storage associated with the closure part of an item
;;;	set.

(defun item-set-flush-closure (item-set)
       (setf (item-set-closure item-set) nil))


;;; GOTO
;;; ====
;;;
;;;	SUBSET is an oset of items which all have the same after dot
;;;	SYMBOL is an oset of items.  Gives back an empty set if the dots
;;;	are all the way to the right of the input set.

(defun goto (subset)
       (let ((result (make-oset :order-fn #'item-order-function)))
         (oset-for-each
           #'(lambda (item)
             (let ((next (advance-dot item)))
               (if next (oset-insert! next result))))
           subset)
         result))


;;; COLLECTION-PRINT-KERNELS
;;; ========================
;;;
;;;	Print all the kernels in the LR0 collection of item-sets.

(defun collection-print-kernels (set-of-item-sets &optional (stream t))
  (format stream  "~&~%start state index: ~S~%~%" *start-state-index*)
  (oset-for-each
    #'(lambda (item-set)
	(format stream  "~&-----------------------------------------~%~%state: ~S~%----------~%~%~%" 
		(item-set-index item-set))
	(item-set-print-kernel item-set stream)
	(format stream "~&~%~%gotos:~%----------~%")
	(oset-for-each
	  #'(lambda (gmelt)
	      (format stream "~&     ~a -> ~a "
		      (g-symbol-print-name (car gmelt))
		      (item-set-index (cdr gmelt))))
	  (item-set-goto-map item-set))
	(format stream "~%~%" )
	)
    set-of-item-sets)
  nil)

(defvar *verbose-item-set-printing* t)
		 

;;; ITEM-SET-PRINT-KERNEL
;;; =====================
;;;
;;;	Prints the kernel of the item-set to stream.  If
;;;	*verbose-item-set-printing* is
;;;	on, then prints the closure as well.

(defun item-set-print-kernel (item-set &optional (stream t))
  (let ((kernel (item-set-kernel item-set))
	(closure (item-set-get-closure! item-set)))
    (format stream "~&kernel:~%----------~%")
    (oset-for-each
	    #'(lambda (item)
		(format stream "     ")
		(item-print item stream 0 t)		;pretty print item
		(format stream "~%"))
	    kernel)
      
    (when *verbose-item-set-printing*
	(progn
	  (format stream "~%~%~%closure:~%----------~%")
	  (oset-for-each
	    #'(lambda (item)
		(format stream "     ")
		(item-print item stream 0 t)		;pretty print item
		(format stream "~%"))
	    (oset-difference closure kernel))))	;full closure - kernel
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (load "lr0-sets.s")
;(comment-out
;  (progn
;    (slurp-grammar "gram1")
;    (make-lr0-collection)
;    (collection-print-kernels *item-set-collection*)))
