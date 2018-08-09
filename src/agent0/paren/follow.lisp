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
;;;                    FOLLOW.lisp
;;;
;;;               Sandy Wells
;;;
;;;        Converted to Common LISP by 
;;;             Barney Pell
;;;
;;; converted:  8/14/87 11:42:00
;;;================================================================================                              


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :follow)
(in-package :ParEn
	    :use '(:lisp :user))

(require :util
	 (lookup-pathname :util))

(require :slrpgram
	 (lookup-pathname :slrpgram))

(require :empty-st
	 (lookup-pathname :empty-st))

(require :first
	 (lookup-pathname :first))


;;; follow.s
;;;
;;; compute follow on a grammar symbol


;;; CALCULATE-FOLLOW-SETS
;;; =====================
;;;
;;;	Compute the follow on a grammar symbol.

(defun calculate-follow-sets ()
  (when *verbose*
    (format t "~&Calculating follow sets.~%"))
  (compute-follow-dependers)
  (follow-insert-first-sets)
  nil)


;;; COMPUTE-FOLLOW-DEPENDERS
;;; ========================
;;;
;;;	Compute the follow dependers for each production.

(defun  compute-follow-dependers ()
  (mapc
    #'(lambda (prod)
      (compute-follow-dependers-aux prod (rhs prod)))
    *productions*))



;;; COMPUTE-FOLLOW-DEPENDERS-AUX
;;; ============================
;;;
;;;	Called initially on a production with PROD being the production
;;;	and PROD-CDR being the rhs of the production. Returns true only
;;;	if the PROD-CDR derives the empty string, or is the empty
;;;	string.  Fills in follow set dependencies by side effect.

(defun  compute-follow-dependers-aux (prod prod-cdr)
  (cond
    ((null prod-cdr) t)
    ((compute-follow-dependers-aux prod (cdr prod-cdr))
     ;; then we fix up dependencies...
     (oset-insert! (car prod-cdr)
                   (g-symbol-follow-dependers (lhs prod)))
     ;; return indication of whether tail derives empty string
     (g-symbol-derives-empty-string (car prod-cdr))
     )
    (t nil)))


;;; FOLLOW-INSERT-FIRST-SETS
;;; ========================
;;;
;;;	

(defun  follow-insert-first-sets ()
  (follow-insert-symbol *the-end-g-symbol* *augmented-start-g-symbol*)
  (mapc #'(lambda (prod) (follow-insert-first-sets-aux (rhs prod)))
        *productions*))



;;; FOLLOW-INSERT-FIRST-SETS-AUX
;;; ============================
;;;
;;;	Called on successive tails of the rhs of each production

(defun  follow-insert-first-sets-aux (prod-rest)
  (cond ((null prod-rest) nil)
        ((null (cdr prod-rest)) nil)
        ;; prod-rest has at least two items
        (t (oset-for-each
             #'(lambda (symbol)
               (unless (eq symbol *empty-string-g-symbol*)
                       (follow-insert-symbol symbol (car prod-rest))))
             (first-seq (cdr prod-rest)))
           (follow-insert-first-sets-aux (cdr prod-rest)))))


;;; FOLLOW-INSERT-SYMBOL
;;; ====================
;;;
;;;	Inserts SYMBOL-TO-INSERT into WHOSE-FOLLOW-SET.  Both are
;;;	g-symbols.

(defun  follow-insert-symbol (symbol-to-insert whose-follow-set)
  (when (oset-insert! symbol-to-insert
                      (g-symbol-follow-set whose-follow-set))
        ;; do it to his dependers too..
        (oset-for-each
          #'(lambda (depender)
            (follow-insert-symbol symbol-to-insert depender))
          (g-symbol-follow-dependers whose-follow-set))))


;;; PRINT-FOLLOW-SET
;;; ================
;;;
;;;	Prints a follow set to STREAM.

(defun print-follow-set (x &optional (stream t))
       (oset-for-each
         #'(lambda (ee)
           (format stream "~S~%" (g-symbol-print-name ee)))
         x))


;;; CRUISE-FOLLOW-SETS
;;; ==================
;;;
;;;	For each symbol, print its follow set to STREAM.

(defun cruise-follow-sets (&optional (stream t))
       (format stream "~%")
       (mapc
         #'(lambda (sym)
           (format t "~S : ~%"
		   (g-symbol-print-name sym))
           (print-follow-set
             (g-symbol-follow-set sym))
           (format t "~%--------------------~%"))
         *symbols*)
       nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (load "follow.s")
;(comment-out
;  (slurp-grammar "gram2")
;  (calculate-empty-string-derivers)
;  (calculate-first-sets)
;  (calculate-follow-sets)
;  (cruise-follow-sets)
;  )
