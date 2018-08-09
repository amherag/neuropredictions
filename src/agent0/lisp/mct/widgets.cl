
;;;  WIDGETS.CL
;;;
;;;

;;;  Defines widgets and creation functions for
;;;  MCT, Mark's CLX Toolkit


(defparameter *mct-default-border-width* 2)
(defparameter *mct-default-inner-border* 2)

(defun print-widget (w stream depth)
  (declare (ignore depth))
  (format stream "[~A (~A ~A) (~A x ~A)]"
	  (widget-type w)
	  (widget-x w) (widget-y w)
	  (widget-width w) (widget-height w)))

(defstruct (widget (:print-function print-widget))
  type
  name
  x y
  width height
  line lines
  (orientation :vertical)
  kids
  font
  setf
  getf
  downf upf
  down-name up-name
  fill expand-x expand-y
  min-width min-height
  min-e-width min-e-height
  (border-width *mct-default-border-width*)
  (inner-border 0)
  parent window window2
  state
  lit-gc dark-gc
  up-gc center-gc down-gc
  text-gc itext-gc
  data					; user-defined data
  )


(defun space (amount)
  (make-widget
   :type :space
   :name amount
   :expand-x nil
   :expand-y nil))

(defun espace (&optional (amount 1))
  (make-widget
   :type :space
   :name amount
   :expand-x t
   :expand-y t))

(defun label (string &key font
		     (inner-border *mct-default-inner-border*)
		     (border-width *mct-default-border-width*)
		     (expand-x nil) (expand-y nil))
  (make-widget
   :type :label
   :font font
   :inner-border inner-border
   :border-width border-width
   :expand-x expand-x
   :expand-y expand-y
   :name string))

(defun hlabel (string &key font
		      (inner-border *mct-default-inner-border*)
		      (border-width *mct-default-border-width*)
		      (expand-x nil) (expand-y nil))
  (hframe (espace)
	  (label string :font font :inner-border inner-border :border-width
		 border-width :expand-x expand-x :expand-y expand-y)
	  (espace)))
  

(defun text (&key getf font
		  (inner-border *mct-default-inner-border*)
		  (border-width *mct-default-border-width*)
		  (expand-x t) (expand-y t)
		  line lines
		  data)
  (make-widget
   :type :text
   :name nil				; (funcall getf w)
   :getf getf
   :line (or line 0)
   :lines lines				; (or lines (numlines (funcall getf)))
   :inner-border inner-border
   :border-width border-width
   :expand-x expand-x
   :expand-y expand-y
   :data data
   :font font))

(defun stext (&key getf font (lines 5) (start 0)
		   (inner-border *mct-default-inner-border*)
		   (border-width *mct-default-border-width*)
		   (expand-x t) (expand-y t)
		   data)
  (let* ((up-button
	  (button "/\\" :inner-border 0
		  :font *small-font*
		  :downf #'(lambda (w)
			     (let ((tw (text-widget w)))
			       (when (> (widget-line tw) 0)
				     (decf (widget-line tw))
				     (redisplay-text tw))))))
	 (down-button
	  (button "\\/" :inner-border 0
		  :font *small-font*
		  :downf #'(lambda (w)
			     (let ((tw (text-widget w)))
			       (when (< (widget-line tw)
					(- (numlines (funcall
						      (widget-getf tw)
						      tw))
					   (widget-lines tw)))
				     (incf (widget-line tw))
				     (redisplay-text tw))))))
	 (buttons
	  (vframe up-button (space 3) down-button))
	 (text (text :getf getf :font font
		     :line start :lines lines
		     :data data
		     :expand-x expand-x
		     :expand-y expand-y)))
    (recalc-text text)
    (hframe (space 5) buttons (space 3) text (space 5))))


;;;  TEXT-WIDGET
;;;
;;;  returns the text widget which is a sibling of this arrow-button's
;;;  enclosing frame

(defun text-widget (w)
  (let ((tw (fourth (widget-kids (widget-parent (widget-parent w))))))
    (if (and (widget-p tw) (eq (widget-type tw) :text))
	tw
      (error "text-widget: ~A is not a text widget" tw))))


;;;  EDIT
;;;
;;;  Doesn't work yet

(defun edit (&key getf setf font
		  (inner-border *mct-default-inner-border*)
		  (border-width *mct-default-border-width*)
		  (expand-x t) (expand-y t))
  (make-widget
   :type :edit
   :name nil				; (funcall getf)
   :getf getf
   :setf setf
   :inner-border inner-border
   :border-width border-width
   :expand-x expand-x
   :expand-y expand-y
   :font font))

(defun button (name &key f function downf upf font data
		    (state :up)
		    (inner-border *mct-default-inner-border*)
		    (border-width *mct-default-border-width*)
		    (expand-x nil) (expand-y nil))
  (make-widget
   :type :button
   :name name
   :downf (or f function downf)
   :upf upf
   :inner-border inner-border
   :border-width border-width
   :expand-x expand-x
   :expand-y expand-y
   :data data
   :state state
   :font font))

(defun toggle (up-name down-name &key upf downf font data
		    (state :up)
		    (inner-border *mct-default-inner-border*)
		    (border-width *mct-default-border-width*)
		    (expand-x nil) (expand-y nil))
  (make-widget
   :type :button
   :name (if (eq state :up) up-name down-name)
   :up-name up-name
   :down-name down-name
   :downf downf
   :upf upf
   :inner-border inner-border
   :border-width border-width
   :expand-x expand-x
   :expand-y expand-y
   :data data
   :state state
   :font font))
  


(defun frame (&key (orientation :vertical) fill kids font data)
  (make-widget
   :type :frame
   :fill fill
   :orientation orientation
   :expand-x nil			; autoset to t if contains any
   :expand-y nil			; x- or y- expandable objects
   :font font
   :data data
   :kids kids))

(defun vframe (&rest kids)
  (frame :orientation :vertical :fill t :kids kids))

(defun hframe (&rest kids)
  (frame :orientation :horizontal :fill t :kids kids))


