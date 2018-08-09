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
;;;          UTIL.lisp
;;;          
;;;          By Sandy Wells
;;;          and Barney Pell
;;;
;;; Jun 13, 1988 -- SW                cleaned up documentation a bit.
;;; Jun 29, 1988 -- SW                fixed bug in vec-bs-assoc on zero length vectors
;;;================================================================================


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :util)
(in-package :ParEn
	    :use '(:lisp :user))


;;;================================================================================
;;;  DEFSYNTAX MACRO   
;;;  (As in SCHEME)  -- By Sandy Wells and Barney Pell
;;;
;;;================================================================================ 

(defun build-binding-alist (pattern invocation)
;  (format t "p: ~s in: ~s ~%" pattern invocation)
  (cond ((null pattern) nil)
	((atom pattern) (list (cons pattern invocation)))
	(t (append (build-binding-alist (car pattern) (car invocation))
		   (build-binding-alist (cdr pattern) (cdr invocation))))))
       
;;; Replaced by SUBLIS
;(defun do-substitutions (binding-alist form)
;  (if (atom form)
;      (let ((association (assoc form binding-alist)))
;	(if (null association)
;	    form
;	    (cdr association)))
;      (cons (do-substitutions binding-alist (car form))
;	    (do-substitutions binding-alist (cdr form)))))
	    
	

(defmacro defsyntax ((name &rest pattern) expansion)
  `(defmacro ,name (&rest invocation)
     (sublis (build-binding-alist ',pattern invocation)
		       ',expansion)))


;;;================================================================================
;;;  Generally useful stuff...
;;;================================================================================


(defsyntax (alias name1 name2)
  (defsyntax (name1 . args) (name2 . args)))



;;; Changed 8/19/87 20:26:25 -- SMS

(defsyntax (post-inc x)
        (prog1 x (incf x)))


(defun pushprop (nam val prop)
       (setf (get nam prop)
                (cons val (get nam prop))))
                

(defsyntax (timer proc)
        (let ((start (runtime))
              (end 0))
          proc
          (setf end (runtime))
          (if (< end start)  ; wrapped midnight
              (/ (+ (- (* 24 60 60 100) start) end) 100)
              (/ (- end start) 100))))



(defsyntax (list->vector list)
	   (coerce list 'vector))


(defsyntax (vector->list vector)
	   (coerce vector 'list))

(defsyntax (symbol->string symbol)		;Return print name of symbol.
	   (symbol-name symbol))

(defsyntax (keyword->string keyword)		;For now, leave keywords as they are.
	   keyword)				


;;; LIST-REF
;;; ========
;;;
;;;	Returns the Nth element of LIST.

(defun list-ref (list n)
  (cond
    ((null list)
     nil)
    ((zerop n)
     (car list))
    (t
     (list-ref (cdr list) (- n 1)))))


;;; PRINT-DATE-AND-TIME
;;; ===================
;;;
;;;	Prints the date and time, in the format:
;;;	DATE: 03/18/68 TIME: 05:56:28

(defun print-date-and-time (stream)
  (let ((user::*print-radix* nil))
    (multiple-value-bind
      (second minute hour date month year)
	(get-decoded-time)
      (format
	stream
	"DATE: ~A/~A/~A  TIME:  ~A:~A:~A"  
	month date year hour minute second)))) 
      

;;; POPN
;;; ====
;;;
;;;	Pops the first n items from list-sym and return in reverse order
;;;	in a list.

(defmacro popn (n list-sym)
  (let ((i (gensym))
	(res (gensym)))
    `(do ((,i 0 (+ 1 ,i))
	  (,res nil))
	 ((= ,i ,n) ,res)
       (push (pop ,list-sym) ,res))))



;;; VEC-BS-ASSOC
;;; ============
;;;
;;;	A function for looking up table entries using binary search. The
;;;	cars of the vector elements are the assoc key and should be in increasing
;;;	order.

(defun vec-bs-assoc (num vec)
  (let* ((count (length vec))
	 (last (- count 1)))
    (if (or (= count 0)
	    (< num (car (aref vec 0)))
	    (> num (car (aref vec last))))
	nil
	(vec-bs-assoc-aux num vec 0 last))))

(defun vec-bs-assoc-aux (num vec start end)
       (cond ((= num (car (aref vec start)))
              (aref vec start))
             ((= start end) nil)
             (t (let ((mid (floor (/ (+ start end) 2))))
                  (if (> num (car (aref vec mid)))
                      (vec-bs-assoc-aux num vec (+ 1 mid) end)
                      (vec-bs-assoc-aux num vec start mid))))))

