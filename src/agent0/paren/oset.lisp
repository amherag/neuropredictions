;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: ParEn -*-


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
;;;			      OSET.LISP
;;;			     By Sandy Wells
;;;
;;;           converted to Common LISP by
;;;               Barney Pell
;;;
;;; converted:  8/12/87 2:32:31
;;;================================================================================


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :oset)
(in-package :ParEn
	    :use '(:lisp :user))
(require :util
	 (lookup-pathname :util))



;;; OSET.LISP
;;;
;;; A simple ordered set facility.  Items kept in these sets must
;;; have an order function: these are supplied for integers and
;;; osets themselves.  Items are kept in sorted lists, smallest
;;; first.  Could be re-done with binary search trees.
;;; See INTEGER-ORDER-FUNCTION for how order functions are supposed to
;;; work.
;;;
;;; Constructor will default to make a set that orders integers.


;;; OSET
;;; ====
;;;
;;;	Osets are structures with three fields:
;;;     Item-list, Order-fn, and Cardinality.

(defstruct oset
  (item-list nil)
  (order-fn #'integer-order-function)
  (cardinality 0))


;;; OSET-EMPTY? 
;;; ===========
;;;
;;;	This macro returns T if OSET is empty, NIL otherwise.

(defsyntax (oset-empty? oset) (null (oset-item-list oset)))




;;; INTEGER-ORDER-FUNCTION
;;; ======================
;;;
;;;	This order function for integers provides an example of how the
;;;	order function is supposed to work.

(defun integer-order-function (a b)
       (cond ((< a b) :correct-order)
             ((> a b) :wrong-order)
             (t :equal)))

;;; OSET-INSERT!
;;; ============
;;;
;;;	Destructively insert an item into a set.
;;;	Returns the item if it wasn't there already, else nil.

(defun oset-insert! (item set)
       (let ((pair (oset-insert-2! item set)))
         (if (car pair) (cdr pair) nil)))



;;; OSET-INSERT-2!
;;; ==============
;;;
;;;	Returns a pair whose car is nil if nothing is inserted
;;;     and whose cdr is a pointer to the item either found or inserted
;;;     into the set (so is eq to a member of the set).


(defun oset-insert-2! (item set)
       (let ((ilist (oset-item-list set))
             (odf (oset-order-fn set))
             order)
         (cond ((or (null ilist)
                    (eq :correct-order
                         (progn (setf order (funcall odf item (car ilist)))
                                order)))
                (progn (setf (oset-item-list set) (cons item ilist))
                       (setf (oset-cardinality set)
                             (1+ (oset-cardinality set)))
                       (cons t item)))
               ((eq :equal order) (cons nil (car ilist))) ; item already there
               (t (oset-insert-aux-2 item set ilist (cdr ilist) odf)))))



;;; OSET-INSERT-AUX-2
;;; =================
;;;
;;;	ILIST isn't null, and ITEM goes somewhere after the car of
;;;	ILIST.

(defun oset-insert-aux-2 (item set ilist ilist-cdr odf)
       (let (order)
         (cond ((or (null ilist-cdr)
                    (eq :correct-order
                         (progn (setf order (funcall odf item (car ilist-cdr)))
                                order)))
                (progn (setf (cdr ilist) (cons item ilist-cdr))
                       (setf (oset-cardinality set)
                             (1+ (oset-cardinality set)))
                       (cons t item)))
               ((eq :equal order) (cons nil (car ilist-cdr))) ;; already there
               (t (oset-insert-aux-2 item set (cdr ilist) (cdr ilist-cdr) odf)
                  ))))



;;; OSET-INSERT-LIST!
;;; =================
;;;
;;;	Insert a LIST of items into an OSET. Returns the OSET.

(defun oset-insert-list! (list oset)
       (mapc #'(lambda (x) (oset-insert! x oset)) list)
       oset)



;;; OSET-ORDER-FUNCTION
;;; ===================
;;;
;;;	Its easy to define a generic order function on osets if they 
;;;	have the same order function making for easy osets of osets.

(defun  oset-order-function (oset-a oset-b)
  (assert (eq (oset-order-fn oset-a)
	      (oset-order-fn oset-b))
	  ()
	  "incompatible types of sets: oset-order-function")
  (cond
    ((< (oset-cardinality oset-a) (oset-cardinality oset-b))
     :correct-order)
    ((> (oset-cardinality oset-a) (oset-cardinality oset-b))
     :wrong-order)
    ;; same cardinality, same type, so march down the lists...
    (t (oset-order-aux (oset-item-list oset-a)
		       (oset-item-list oset-b)
		       (oset-order-fn oset-a)))))


;;; OSET-ORDER-AUX
;;; ==============
;;;
;;;	Used by OSET-ORDER-FUNCTION.

(defun oset-order-aux (ilista ilistb odf)
       (if (null ilista)
           :equal
           (let ((item-order (funcall odf (car ilista) (car ilistb))))
             (if (eq :equal item-order)
                 (oset-order-aux (cdr ilista)
                                 (cdr ilistb)
                                 odf)
                 item-order))))


;;; OSET-EQUAL
;;; ==========
;;;
;;;	Returns T if OSETA is EQ to OSETB, NIL otherwise.

(defun oset-equal (oseta osetb)
       (eq :equal (oset-order-function oseta osetb)))


;;; OSET-SELECT-SUBSETS
;;; ===================
;;;
;;;	Yields a list of disjoint subsets whose union is the set.  For 
;;;     each subset the value of selection-fn applied to the members is
;;;     the same in the sense of eqv.

(defun oset-select-subsets (set selection-fn)
       (do ((r-ilist (reverse (oset-item-list set)) (cdr r-ilist))
            (alist nil)
            sfv ssl item)
           ((null r-ilist)
            (mapcar #'(lambda (x) (make-oset :item-list (cdr x)
                                           :cardinality (+ -1 (length x))
                                           :order-fn (oset-order-fn set)))
                    alist))
           (setf item (car r-ilist))
           (setf sfv (funcall selection-fn item))
           (setf ssl (assoc sfv alist :test #'eql))
           (if ssl (setf (cdr ssl) (cons item (cdr ssl)))
               (push (cons sfv (list item)) alist))))


;;; OSET-FOR-EACH
;;; =============
;;;
;;;	Apply PROCEDURE to each element of OSET.

(defun oset-for-each (procedure oset)
       (mapc procedure (oset-item-list oset)))


;;; OSET-MEMQ
;;; =========
;;;
;;;	Returns T if ELT is a member of OSET (eq), NIL otherwise.

(defun oset-memq (elt oset)
       (member elt (oset-item-list oset) :test 'eq))


;;; OSET-COPY
;;; =========
;;;
;;;	Returns a copy of OSET.

(defun oset-copy (oset)
   (make-oset
      :item-list (copy-list (oset-item-list oset))
      :order-fn (oset-order-fn oset)
      :cardinality (oset-cardinality oset)))	      

	
;;; OSET-UNION
;;; ==========
;;
;;;	Returns the oset-union (OSET1 U OSET2).  If the sets use
;;;	different order functions, then returns an error.


(defun oset-union (oset1 oset2)
   (assert (eq (oset-order-fn oset1) (oset-order-fn oset2))
	    ()
	    "Mismatched order functions in oset union.")
   (oset-insert-list! (oset-item-list oset1)
      (oset-copy oset2)))


		 
;;; OSET-DELETE
;;; ===========
;;;
;;;	Non-destructively delete ITEM from OSET.  Returns the oset which
;;;	is (OSET - ITEM), or OSET if ITEM is not in OSET.

(defun oset-delete (item oset)
   (let ((new-oset (oset-copy oset)))
      (if (oset-memq item oset)
          (progn
             (setf (oset-cardinality new-oset)
		   (- (oset-cardinality oset) 1))
             (setf (oset-item-list new-oset)
	           (delete item (oset-item-list new-oset)))))
      new-oset))			         

;;; OSET-DIFFERENCE
;;; ===============
;;;
;;;	Returns the oset-difference (OSET1 - OSET2).

(defun oset-difference (oset1 oset2)
  (oset-for-each
    #'(lambda (item)
	(setf oset1
	      (oset-delete item oset1)))
    oset2)
  oset1)

;;; OSET->VECTOR
;;; ============
;;;
;;;	Convert the item-list of  OSET into a vector, and return that
;;;	list.  Note that cardinality and order function are lost, so
;;;	that this vector is now static.

(defun oset->vector (oset)
  (list->vector
    (oset-item-list oset)))


;;; OSET-FIND
;;; =========
;;;
;;;	This should be primitive, and not insert if not there.  Third
;;;	arg is optional error message. The result  is eq to member
;;;	of the set.

(defun  oset-find (element set &rest rest)
  (let
    ((insertion-result (oset-insert-2! element set))
     (error-string
	(if (null rest)
	    "oset-find failed to find element"
	    (car rest))))
    (if (car insertion-result)
      (error error-string))
    (cdr insertion-result)))


;;; OSET-EMPTY!
;;; ===========
;;;
;;;	Empties the oset.

(defun oset-empty! (oset)
  (setf (oset-cardinality oset) 0)
  (setf (oset-item-list oset) nil))
