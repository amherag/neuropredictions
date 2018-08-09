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
;;;                    G-SYMBOL
;;;
;;;               Sandy Wells
;;;
;;;    converted to Common LISP by
;;;       Barney Pell
;;;
;;; converted:  8/12/87 2:50:07
;;;================================================================================                                
;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :g-symbol)
(in-package :ParEn
	    :use '(:lisp :user))

(require :util
	 (lookup-pathname :util))

(require :oset
	 (lookup-pathname :oset))


;;; g-symbol.s
;;;
;;; Representation of GRAMMAR SYMBOLS.
;;; PRINT-NAME is a string.  
;;; INDEX is a unique integer associated with the symbol.
;;; OWN-PRODUCTIONS is a list of
;;; the productions that the symbol appears on the left side of.
;;; RHS-PRODUCTIONS is a list of the productions the symbol appears
;;; on the right side of.
;;; FIRST-SET is the set of terminal grammar symbols which can
;;; legally start a string derived from the symbol.
;;; FIRST-SET-DEPENDERS is used in the computation of the first-set.
;;; DERIVES-EMPTY-STRING is a quick way of telling if the empty
;;; string is in the first-set of the symbol.
;;; FOLLOW-SET is the set of terminal symbols which may appear after
;;; the symbol in strings of the language.
;;; FOLLOW-DEPENDERS is the set of grammar symbols whose follow sets
;;; must contain this guys follow set.
;;; sets will be represented by o-sets.
;;; a hack -- a g-symbol is non-terminal is own-productions is nil

(defstruct (g-symbol (:print-function print-g-symbol))
                  print-name
                  index
                  own-productions
                  rhs-productions
                  first-set
                  first-set-dependers
                  derives-empty-string
                  follow-set
                  follow-dependers)

;;; PRINT-G-SYMBOL
;;; ==============
;;;
;;;	Print function for g-symbols.

(defun print-g-symbol (g-symbol stream depth)
  (if (and *print-level* (>= depth *print-level*))
      (format stream "#")
      (format stream "#<G-SYMBOL  (NAME: ~S) (INDEX: ~S)>"
	      (g-symbol-print-name g-symbol)(g-symbol-index g-symbol))))


(defsyntax (g-symbol-non-terminal? sym)
        (not (null (g-symbol-own-productions sym))))



;;; NEW-G-SYMBOL
;;; ============
;;;
;;;	Creates a new g-symbol named PRINT-NAME with index INDEX.

(defun new-g-symbol (print-name index)
       (make-g-symbol
         :print-name print-name
         :index index
         :own-productions nil
         :rhs-productions nil
         :first-set (make-oset :order-fn #'g-symbol-order-function)
         :first-set-dependers (make-oset :order-fn #'g-symbol-order-function)
         :follow-set (make-oset :order-fn #'g-symbol-order-function)
         :follow-dependers (make-oset :order-fn #'g-symbol-order-function)))


;;; G-SYMBOL-ORDER-FUNCTION
;;; =======================
;;;
;;;	G-symbols are ordered by increasing  index.

(defun g-symbol-order-function (sa sb)
       (cond ((< (g-symbol-index sa) (g-symbol-index sb)) :correct-order)
             ((> (g-symbol-index sa) (g-symbol-index sb)) :wrong-order)
             (t :equal)))

;;; G-SYMBOL-ADD-PRODUCTION
;;; =======================
;;;
;;;	Add PRODUCTION to the list of productions for G-SYMBOL.

(defun g-symbol-add-production (g-symbol production)
       (setf (g-symbol-own-productions g-symbol)
	     (cons production (g-symbol-own-productions g-symbol))))






