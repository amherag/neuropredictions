
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

(defun text (&key getf font
		  (inner-border *mct-default-inner-border*)
		  (border-width *mct-default-border-width*)
		  (expand-x t) (expand-y t))
  (make-widget
   :type :text
   :name (funcall getf)
   :getf getf
   :inner-border inner-border
   :border-width border-width
   :expand-x expand-x
   :expand-y expand-y
   :font font))

(defun edit (&key getf setf font
		  (inner-border *mct-default-inner-border*)
		  (border-width *mct-default-border-width*)
		  (expand-x t) (expand-y t))
  (make-widget
   :type :edit
   :name (funcall getf)
   :getf getf
   :setf setf
   :inner-border inner-border
   :border-width border-width
   :expand-x expand-x
   :expand-y expand-y
   :font font))

(defun button (name &key f function downf upf font
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
   :state state
   :font font))

(defun toggle (up-name down-name &key upf downf font
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
   :state state
   :font font))
  


(defun frame (&key (orientation :vertical) fill kids font)
  (make-widget
   :type :frame
   :fill fill
   :orientation orientation
   :expand-x nil			; autoset to t if contains any
   :expand-y nil			; x- or y- expandable objects
   :font font
   :kids kids))

(defun vframe (&key fill kids font)
  (frame :orientation :vertical :fill fill :kids kids :font font))

(defun hframe (&key fill kids font)
  (frame :orientation :horizontal :fill fill :kids kids :font font))



