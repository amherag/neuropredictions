;;;-*- Mode:Common-Lisp; Package:PICTURES; Base:10 -*-
;;;
;;;
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick Hogan, Eric Mielke

(in-package "PICTURES")


(DEFPARAMETER  *vector-cache* nil)

(export '(
	  t11 t12 t21 t22 t31 t32
	  make-transform
	  compose-transform
	  copy-transform
	  move-transform
	  radians
	  rotate-transform
	  scale-transform
	  scale-point
	  transform-point
	  transform-point-seq
	  )
	'pictures)

;;; Transform Class Definition:

(defclass transform ()
  (
   (t11		:type     float
                :initarg  :t11
		:reader   t11
                :initform 1.0
		:documentation "Position (1,1) in transform matrix")
   
   (t12		:type     float
                :initarg  :t12
 		:reader   t12
		:initform 0.0
		:documentation "Position (1,2) in transform matrix")
   
   (t21		:type     float
                :initarg  :t21
		:reader   t21
                :initform 0.0
		:documentation "Position (2,1) in transform matrix")
   
   (t22		:type     float
                :initarg  :t22
		:reader   t22
                :initform 1.0
		:documentation "Position (2,2) in transform matrix")
   
   (t31		:type     float
                :initarg  :t31
		:reader   t31
                :initform 0.0
		:documentation "Position (3,1) in transform matrix")
   
   (t32		:type     float
                :initarg  :t32
		:reader   t32
                :initform 0.0
		:documentation "Position (3,2) in transform matrix")
   )
  (:documentation "Represents a 3x3 homogeneous coordinate system transform matrix"))


;Function: make-transform
;  Create a new transform object.  With no initargs, this creates an identity transform.

(defun make-transform (&rest initargs
                         &key &allow-other-keys)
  (declare (values transform))


  (apply #'make-instance 'transform initargs))


;Private Variable: temp-transform
;  Used in compose-transform to hold temporary result
(defvar *temp-transform* (make-transform))


;Function: compose-transform
;  Change the RESULT transform to be the product of (TRANSFORM-1 x
;  TRANSFORM-2).  If RESULT is not given, then the result replaces
;  TRANSFORM-2.  If both TRANSFORM-2 and RESULT are nil, then a new
;  transform is created to hold the result.  A nil transform represents the
;  identity transform.  The new value of RESULT is returned.

(defun compose-transform (transform-1 transform-2
                          &optional (result transform-2))
  (declare (type (or null transform) transform-1 transform-2 result))
  (declare (values transform))

  (cond ((null transform-1)				; T-1 is the identity
         (copy-transform transform-2 result))		;   Just use T-2
        ((null transform-2)				; T-2 is the identity
         (copy-transform transform-1 result))		;   Just use T-1
        ((eq transform-2 result)			; T2 and RESULT are the same
         (with-slots (t11 t12 t21 t22 t31 t32) transform-2
           (post-mult transform-1 t11 t12 t21 t22 t31 t32 *temp-transform*)
           (copy-transform *temp-transform* result)))	;   Use temporary result
        (t						; Otherwise, compose them
         (with-slots (t11 t12 t21 t22 t31 t32) transform-2
           (post-mult transform-1 t11 t12 t21 t22 t31 t32 result)))))


;Function: copy-transform
;  Copy transform-1 into transform-2.  Either or both can be nil.  Return the new
;  transform-2.

(defun copy-transform (transform-1 transform-2)
  (declare (type (or null transform) transform-1 transform-2))
  (declare (values transform-2))

  (cond ((eq transform-1 transform-2))			; They are already identical!
        (transform-1					; T-1 is not identity
         (unless transform-2
           (setf transform-2 (make-transform)))		;   Must make T-2 first
         (with-slots (t11 t12 t21 t22 t31 t32) transform-1
           (with-slots ((y11 t11) (y12 t12) (y21 t21) (y22 t22) (y31 t31) (y32 t32)) transform-2
             (psetf y11 t11 y12 t12 y21 t21 y22 t22 y31 t31 y32 t32))))
        (t						; T-1 is the identity
         (if transform-2
             (with-slots (t11 t12 t21 t22 t31 t32) transform-2	; Make T-2 the identity
               (psetf t11 1.0 t12 0.0 t21 0.0 t22 1.0 t31 0.0 t32 0.0))
             (setf transform-2 (make-transform)))))		; Or create one if not there yet

  transform-2)					; Return T-2


(defmethod move-transform ((transform transform) delta-x delta-y)
  (declare (values transform))

  (with-slots (t31 t32) transform			; Just translate the transform
    (psetq t31 (+ t31 delta-x)
           t32 (+ t32 delta-y)))
  transform)						; Return the modified transform


;Method: print-object
;   Print a transform object

(defmethod print-object :after ((transform transform) stream)
  (declare (values transform))
  (with-slots (t11 t12 t21 t22 t31 t32) transform
    (format stream "[|~6,2f  ~6,2f  ~6,2f||~6,2f  ~6,2f  ~6,2f||~6,2f  ~6,2f  ~6,2f|]"
            t11 t12 0.0 t21 t22 0.0 t31 t32 1.0)))


;Macro: radians
;  Convert degrees to radians using the same floating point precision

(defmacro radians (degrees)
 
 `(* ,degrees (/ pi  180)))


;Method: rotate-transform
;  Modify the TRANSFORM, rotating the previous transformation by the given ANGLE
;  (in radians) around the given fixed point. The new value of the TRANSFORM is
;  returned.

(defmethod rotate-transform ((transform transform) angle
                               &optional (fixed-x 0) (fixed-y 0))
  (declare (values transform))

  (let* ((cos-angle (cos angle))			; Implementation note:
         (sin-angle (sin angle))			;  (cis angle) is NOT faster on Explorer!
         (origin-fixed (and (zerop fixed-x) (zerop fixed-y)))
         (trans-x (if origin-fixed				; Translate only if fixed-point is not origin
                      0.0
                      (+ (* fixed-x (- 1 cos-angle)) (* fixed-y sin-angle))))
         (trans-y (if origin-fixed
                      0.0
                      (- (* fixed-y (- 1 cos-angle)) (* fixed-x sin-angle)))))

    (post-mult transform					; Translate to origin, rotate, translate back
               cos-angle	sin-angle
               (- sin-angle) 	cos-angle
               trans-x	trans-y)))


;Method: scale-transform
;  Modify the TRANSFORM, scaling the previous transformation by the given scale
;  factors around the given fixed point. The new value of the TRANSFORM is
;  returned.

(defmethod scale-transform ((transform transform) scale-x scale-y
                  &optional (fixed-x 0) (fixed-y 0))
  (declare (values transform))

  (let* ((origin-fixed (and (zerop fixed-x) (zerop fixed-y)))	; Translate only if fixed point is not origin
         (trans-x (if origin-fixed
                      0.0
                      (* fixed-x (- 1 scale-x))))
         (trans-y (if origin-fixed
                      0.0
                      (* fixed-y (- 1 scale-y))))
	 )
	 (post-mult transform					; Translate to origin, scale, translate back
               scale-x	0.0
               0.0	scale-y
               trans-x	trans-y)))


;Function: scale-point
;  Return the result of applying TRANSFORM to the given X-DISTANCE and Y-DISTANCE.

(defun scale-point (transform x-distance y-distance)
  (declare (type (or null transform) transform))
  (declare (type wcoord x-distance y-distance))
  (declare (values new-x-distance new-y-distance))

  (if transform							; Identity?
      (with-slots (t11 t12 t21 t22) transform				;  No,
        (let ((x-scale (sqrt (+ (* t11 t11) (* t12 t12))))		; Compute scale factors
              (y-scale (sqrt (+ (* t21 t21) (* t22 t22)))))
          (values (* x-distance x-scale)				; new-x-distance
                  (* y-distance y-scale))))				; new-y-distance
      (values x-distance y-distance)))				;  Yes, old-x, old-y



;Function: transform-point
;  Return the result of applying TRANSFORM to the given point.

(DEFMETHOD  transform-point ((transform transform) x y)
  (declare (type (or null transform) transform))
  (declare (type wcoord x y))
  (declare (values new-x new-y))
  (with-slots (t11 t12 t21 t22 t31 t32) transform	;  No,
    (values (+ (* x t11) (* y t21) t31)		; new-x
	    (+ (* x t12) (* y t22) t32)))	; new-y
  )						;  Yes, old-x, old-y

(DEFMETHOD  transform-point (( transform t) x y)
  (declare (type (or null transform) transform))
  (declare (type wcoord x y))
  (declare (values new-x new-y))				; new-y
      (values x y))

;Function: transform-point-seq
;  Destructively changes the point-seq by applying TRANSFORM to the
;  given points.  


(defun transform-point-seq (transform  point-vector &optional (result point-vector))
  
  (declare (type (or null transform) transform))
  (declare (type (or null vector) point-vector )) 
  (with-vector transformed-vector
	       (LET* ((vector-length (LENGTH point-vector)))	; How many pairs are there?
		 
		 (IF   transform		; Identity transform?
		   (with-slots (t11 t12 t21 t22 t31 t32) transform	; No, transform the points
		     (let ((x11 t11)		; Store transform in local vars for efficiency
			   (x12 t12)
			   (x21 t21)
			   (x22 t22)
			   (x31 t31)
			   (x32 t32))
		       
		       (let (x-i y-i)		; Transform the vectors
			 (IF (AND (= x11 x22 1) (= x12 x21 0))
			     (do ((i 0 (+ i 2)))
				 ((>= i  vector-length) nil)
			       (setf x-i (ELT point-vector i)	; Save next point in temporaries
				     Y-i (ELT point-vector (+ 1 i)))
			       (vector-push-extend
				 (+  x-i x31)
				 transformed-vector)
			       (vector-push-extend
				 (+  y-i  x32)
				 transformed-vector)
			       )
			     
			     (do ((i 0 (+ i 2)))
				 ((>= i  vector-length) nil)
			       (setf x-i (ELT point-vector i)	; Save next point in temporaries
				     Y-i (ELT point-vector (+ 1 i)))
			       (vector-push-extend
				 (+ (* x-i x11) (* y-i x21) x31)
				 transformed-vector)
			       (vector-push-extend
				 (+ (* x-i x12) (* y-i x22) x32)
				 transformed-vector)
			       )))
		       )
		     (copy-to-point-seq transformed-vector result))
		   (copy-to-point-seq point-vector result)))))

(DEFUN get-global-vector ()
  "return a reusable vector from the a global *vector-cache*. If the fillpointer for a vertor is 0, it is available"
  (DOLIST (VECTOR *vector-cache*  (PROGN (PUSH (cons (make-array '(10) :adjustable t :fill-pointer 0) 1) *vector-cache*)
					 (CAAR *vector-cache*)))
    (WHEN (= (cdr vector) 0)
       (SETF (CDR vector) 1)(RETURN (car vector)))))

(DEFUN return-global-vector (avector)
  (SETF (FILL-POINTER avector) 0)
  (SETF (CDR (ASSOC avector *vector-cache*)) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Private Function: post-mult
;  Change the RESULT transform to be the product of (X x Y), where Y is
;  a homogeneous matrix defined by Y11, Y12, ...  If RESULT is nil, create a new
;  transform fo the result.  The new value of RESULT is returned.

(defun post-mult (x y11 y12 y21 y22 y31 y32
                          &optional (result x))
  (declare (type transform x))
  (declare (type (or null transform) result))
  (declare (values transform))

  (unless result			; Create a result transform if necessary
    (setf result (make-transform)))

  (with-slots ((x11 t11) (x12 t12) (x21 t21) (x22 t22) (x31 t31) (x32 t32)) X			; X x Y = Z
      (with-slots ((z11 t11) (z12 t12) (z21 t21) (z22 t22) (z31 t31) (z32 t32)) result

      (let ((temp11 x11)				; Use temporaries in case RESULT and X are the same
            (temp21 x21)
            (temp31 x31))

        (psetq z11 (+ (* temp11 y11) (* x12 y21))	; Compute first row
               z12 (+ (* temp11 y12) (* x12 y22))

               z21 (+ (* temp21 y11) (* x22 y21))	; Compute second row
               z22 (+ (* temp21 y12) (* x22 y22))

               z31 (+ (* temp31 y11) (* x32 y21) y31)	; Compute third row
               z32 (+ (* temp31 y12) (* x32 y22) y32)))))

  result)					; Return RESULT transform















