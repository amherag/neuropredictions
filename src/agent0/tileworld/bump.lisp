;;;
;;; bump.lisp
;;;
;;; A little module for creating "bumps" as a pair of embedded windows.
;;; 


(in-package 'xtile)

(defstruct bump
  parent				;parent window that bump is in
  x y					;location of bump in parent window
  ratio					;inner height over outer height
  border-h				;horizontal space between inner&outer
  border-v				;vertical space etc.
  upper-left-gc				;gcontext for drawing upper-left part
  lower-right-gc			;gcontext for drawing lower-right part
  center-gc				;gcontext for drawing center part
  outer-window				;window for outer part
  inner-window				;window for inner part
  outer-height				;height of outer window
  outer-width				;width of outer window
  inner-height				;height of inner window
  inner-width				;height of outer window
)

(defun fill-out-bump-description (b)
  (when (and (bump-inner-height b) (bump-ratio b))
	(setf (bump-outer-height b) (/ (bump-inner-height b) 
				       (bump-ratio b))))
  (when (and (bump-outer-height b) (bump-ratio b))
	(setf (bump-inner-height b) (* (bump-outer-height b) 
				       (bump-ratio b))))
  (when (and (bump-outer-height b) (bump-inner-height b))
	(setf (bump-border-v b) (/ (- (bump-outer-height b)
				      (bump-inner-height b))
				   2)))
  (when (bump-border-v b)
	(setf (bump-border-h b) (bump-border-v b)))
  (when (and (bump-inner-width b) (bump-border-h b))
	(setf (bump-outer-width b) (+ (bump-inner-width b)
				      (* 2 (bump-border-h b)))))
  (when (and (bump-outer-width b) (bump-border-h b))
	(setf (bump-inner-width b) (- (bump-outer-width b)
				      (* 2 (bump-border-h b)))))
  (when (and (bump-outer-height b) 
	     (bump-outer-width b) 
	     (bump-parent b) 
	     (bump-x b) (bump-y b)
	     (not (bump-outer-window b)))
	(setf (bump-outer-window b)
	      (xtw-borderless-window (bump-parent b)
				     (round (bump-x b))
				     (round (bump-y b))
				     (round (bump-outer-width b))
				     (round (bump-outer-height b)))))
  (when (and (bump-outer-height b)
	     (bump-outer-width b)
	     (bump-outer-window b)
	     (bump-inner-height b)
	     (bump-inner-width b)
	     (not (bump-inner-window b)))
	(setf (bump-inner-window b)
	      (xtw-borderless-window (bump-outer-window b)
				     (round (- (bump-outer-width b)
					       (bump-inner-width b))
					    2)
				     (round (- (bump-outer-height b)
					       (bump-inner-height b))
					    2)
				     (round (bump-inner-width b))
				     (round (bump-inner-height b))))
	(setf (window-background (bump-inner-window b))
	      (if (eq (gcontext-fill-style (bump-center-gc b)) :tiled)
		  (gcontext-tile (bump-center-gc b))
		(gcontext-foreground (bump-center-gc b))))
	)
  )


(defun draw-bump (b)
  (draw-b-outwin b)
  (clear-inner-window b)
  )

(defun draw-b-outwin (b)
  (draw-rectangle (bump-outer-window b) (bump-lower-right-gc b)
		  0 0 
		  (drawable-width (bump-outer-window b))
		  (drawable-height (bump-outer-window b))
		  t)
  (dotimes (i (1+ (round (- (bump-outer-width b) (bump-inner-width b)) 2)))
	   (draw-line (bump-outer-window b) (bump-upper-left-gc b)
		      i 0 
		      i (round (- (bump-outer-height b) i)))
	   (draw-line (bump-outer-window b) (bump-upper-left-gc b)
		      0 i
		      (round (- (bump-outer-width b) i)) i)
	   )
  )

(defun clear-inner-window (b)
  (draw-rectangle (bump-inner-window b) (bump-center-gc b)
		  0 0
		  (drawable-width (bump-inner-window b))
		  (drawable-height (bump-inner-window b))
		  t))

(defun set-bump-width (b inner-width)
  (cond ((not (eql (bump-inner-width b) inner-width))
	 (setf (bump-inner-width b) inner-width)
	 (setf (bump-outer-width b) 
	       (+ (bump-inner-width b) (* 2 (bump-border-h b))))
	 (setf (drawable-width (bump-outer-window b)) 
	       (round (bump-outer-width b)))
	 (setf (drawable-width (bump-inner-window b)) 
	       (round (bump-inner-width b))))
	((clear-inner-window b))))
