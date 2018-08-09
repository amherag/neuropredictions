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
;;;                    EMPTY-ST.lisp
;;;
;;;                  Sandy Wells
;;;
;;;           Converted to Common LISP by
;;;              Barney Pell
;;;
;;; converted:  8/13/87 5:59:15
;;;================================================================================


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :empty-st)
(in-package :ParEn
	    :use '(:lisp :user))

(require :slrpgram
	 (lookup-pathname :slrpgram))

(require :g-symbol
	 (lookup-pathname :g-symbol))


;;; empty-st.s
;;;
;;; cruise the productions and figure out which ones derive the empty string


;;; CALCULATE-EMPTY-STRING-DERIVERS
;;; ===============================
;;;
;;;	Cruise the productions and figure out which ones derive the
;;;	empty string.

(defun calculate-empty-string-derivers ()
  (when *verbose*
    (format t "~&Calculating nullable symbols:~%  "))
  (mapc
    #'(lambda (prod)
;;;      (princ "prod: ") (print-production prod)
      (if
;; replaced the following two lines with check for no rhs
;;        (and (eq? (car (rhs prod)) *empty-string-g-symbol*)
;;             (eq? (cdr (rhs prod)) nil))
	(null (rhs prod))
        (process-symbol-which-derives-empty-string (lhs prod))))
    *productions*)
  (when *verbose* (format t "~%"))
  nil)


;;; PROCESS-SYMBOL-WHICH-DERIVES-EMPTY-STRING
;;; =========================================
;;;
;;;	Unless we have already figured out that GS derives the empty
;;;	string, then make sure each production in the RHS of GS derives
;;;	the empty string.

(defun process-symbol-which-derives-empty-string (gs)
  (unless (g-symbol-derives-empty-string gs)
          (when *verbose*
	    (format t " ~a" (g-symbol-print-name gs)))
          (setf (g-symbol-derives-empty-string gs) t)
          (mapc
            #'(lambda (prod)
              (if (string-vanishes (rhs prod))
                  (process-symbol-which-derives-empty-string (lhs prod))))
            (g-symbol-rhs-productions gs))))


;;; STRING-VANISHES
;;; ===============
;;;
;;;	Returns T if every G-SYMBOL in GSLIST derives the empty string,
;;;	and NIL otherwise.

(defun string-vanishes (gslist)
  (cond ((null gslist) t)
        ((not (g-symbol-derives-empty-string (car gslist))) nil)
        (t (string-vanishes (cdr gslist)))))



;;; (slurp-grammar "gram3")
;;; (calculate-empty-string-derivers)
;;; (compile2 "empty-st")



