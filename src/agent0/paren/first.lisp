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
;;;                    FIRST.lisp
;;;
;;;               Sandy Wells
;;;
;;;        Converted to Common LISP by 
;;;             Barney Pell
;;;
;;; converted:  8/13/87 6:08:00
;;;================================================================================                              


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :first)
(in-package :ParEn
	    :use '(:lisp :user))

(require :util
	 (lookup-pathname :util))

(require :oset
	 (lookup-pathname :oset))

(require :g-symbol
	 (lookup-pathname :g-symbol))

(require :empty-st
	 (lookup-pathname :empty-st))

(require :slrpgram
	 (lookup-pathname :slrpgram))


;;; first.s
;;;
;;; calculate the first sets of the grammar symbols
;;; basic design from John Bear :
;;;    University of Texas at Austin Tech Report GRG 220
;;;    "A Breadth-First Syntactic Component"
;;; I added empty string handling: Sandy Wells


;;; CALCULATE-FIRST-SETS
;;; ====================
;;;
;;;	

(defun  calculate-first-sets ()
  (if *verbose*
      (format t "~&Calculating first sets.~%"))
  ;; the start set of a terminal symbol is the symbol itself
  (mapc #'(lambda (gs)
          (unless (g-symbol-non-terminal? gs)
                  (oset-insert! gs (g-symbol-first-set gs))))
        *symbols*)
  (mapc #'(lambda (prod)
          (calculate-first-sets-aux (lhs prod) (rhs prod)))
        *productions*)
  nil)


;;; CALCULATE-FIRST-SETS-AUX
;;; ========================
;;;
;;;	

(defun  calculate-first-sets-aux (prod-lhs prod-rhs-cdr)
  ;; see if we've empty stringed ourselves out of rhs
  (if (null prod-rhs-cdr)
      (first-set-insert! *empty-string-g-symbol* prod-lhs)
      (let ((rhs-cdr-first (car prod-rhs-cdr)))
        (cond
          ;; check for terminal symbol or empty string
          ((or (not (g-symbol-non-terminal? rhs-cdr-first))
               (eq rhs-cdr-first *empty-string-g-symbol*))
           (first-set-insert! rhs-cdr-first prod-lhs))
          ;; must be non terminal
          (t (first-set-add-depender! prod-lhs rhs-cdr-first)
             (if (g-symbol-derives-empty-string rhs-cdr-first)
                 (calculate-first-sets-aux prod-lhs (cdr prod-rhs-cdr))))))))



;;; FIRST-SET-INSERT!
;;; =================
;;;
;;;	 Add a symbol to the first set of another symbol. If it isn't the
;;;	 empty string, and wasn't there already, add it to the first
;;;	 sets of the guys who's first sets contain this guys (the
;;;	 dependers).

(defun  first-set-insert! (to-insert insertee)
  (if (and (oset-insert! to-insert (g-symbol-first-set insertee))
           (not (eq *empty-string-g-symbol* to-insert)))
      (oset-for-each
        #'(lambda (depender)
          (first-set-insert! to-insert depender))
        (g-symbol-first-set-dependers insertee))))


;;; FIRST-SET-ADD-DEPENDER!
;;; =======================
;;;
;;;	

(defun first-set-add-depender! (new-depender gs)
  (if (oset-insert! new-depender (g-symbol-first-set-dependers gs))
      (oset-for-each
        #'(lambda (sym)
          (first-set-insert! sym new-depender))
        (g-symbol-first-set gs))))


;;; FIRST-SEQ
;;; =========
;;;
;;;	FIRST-SEQ (sequence of symbols) returns {s | seq =*=> s...}

(defun first-seq (seq)
    (if (null seq) 
	(make-oset :order-fn #'g-symbol-order-function)
	(let ((firsts (g-symbol-first-set (car seq))))
	   (if (g-symbol-derives-empty-string (car seq))
	      (oset-union
                 (oset-delete *empty-string-g-symbol* firsts)
		 (first-seq (cdr seq)))
	      firsts))))
		      

;;; PRINT-FIRST-SET
;;; ===============
;;;
;;;	Print out to STREAM a first-set X.

(defun print-first-set (x &optional (stream t))
       (oset-for-each
         #'(lambda (ee)
           (format stream "~S" (g-symbol-print-name ee)))
         x))


;;; CRUISE-FIRST-SETS
;;; =================
;;;
;;;	Print out to STREAM the first-set for every symbol.

(defun cruise-first-sets (&optional (stream t))
       (mapc
         #'(lambda (sym)
           (format stream "~S : " (g-symbol-print-name sym))
           (print-first-set
             (g-symbol-first-set sym) stream)
           (format stream "~%--------------------~%"))
         *symbols*)
       nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test
;(comment-out
; (slurp-grammar "gram2")
; (calculate-empty-string-derivers)
; (calculate-first-sets)
; (cruise-first-sets)
;)
