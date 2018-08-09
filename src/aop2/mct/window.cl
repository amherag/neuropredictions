

;;;  WINDOW.CL
;;;
;;;

;;;  Functions to install and remove windows, and all their associated
;;;  subwindows, to handle widgets


(defun bump-lit-gc () *grey75-gc*)
(defun bump-dark-gc () *black-gc*)
(defun bump-up-gc () *grey60-gc*)
(defun bump-center-gc () *grey50-gc*)
(defun bump-down-gc () *grey25-gc*)
(defun bump-text-gc () *text-gc*)

(defvar *wi-list* nil)
(defun mct-register-window (wi)
  (setf *wi-list* (cons wi *wi-list*)))
(defun mct-unregister-window (wi)
  (setf *wi-list* (remove wi *wi-list*)))

(defvar *wi-to-map* nil)

(defstruct mct-wi
  name
  kid
  gc
  x y width height
  min-width min-height
  max-width max-height
  hash
  window)

(defstruct winfo
  type					;:bump-inner or :bump-outer
  widget)				;the field it goes with


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  MCT-INSTALL-WINDOW
;;;
;;;  Call this function with :kid equal to a partially-determined widget
;;;  (such as the output from the constructor functions defined in
;;;   the source file "widgets").  It will create and map a window to
;;;  the *display*, and install its handler functions in the main event
;;;  loop.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mct-install-window (&key name kid foreground background
				min-width min-height
				(init-width 0) (init-height 0)
				(x 0) (y 0)
				(font *font*)
				max-width max-height)
  (let* ((wi (make-mct-wi :name name
			  :hash (make-hash-table)
			  :gc (mct-gcontext :foreground foreground
					    :background background
					    :invid-p t)
			  :kid (set-min-width kid :font font)))
	 (min-w (if min-width (max min-width (widget-min-width kid))
		  (widget-min-width kid)))
	 (min-h (if min-height (max min-height (widget-min-height kid))
		  (widget-min-height kid))))
    
    ;; User can specify initial size, minimum size, and maximum size.
    ;; If initial-size is not given, defaults to minimum size
    ;; If minimum-size is not given, defaults to size needed to pack
    ;;  all widgets.
    
    (setf (mct-wi-min-width wi) min-w
	  (mct-wi-min-height wi) min-h
	  (mct-wi-max-width wi) (if max-width (max min-w max-width))
	  (mct-wi-max-height wi) (if max-width (max min-h max-height))
	  
	  ;; initial size
	  (mct-wi-width wi) (max min-w init-width)
	  (mct-wi-height wi) (max min-h init-height)
	  
	  ;; window
	  (mct-wi-window wi) (mct-window (mct-wi-width wi) (mct-wi-height wi)
					 x y background))
    
    (setf (widget-x kid) 0
	  (widget-y kid) 0
	  (widget-width kid) (mct-wi-width wi)
	  (widget-height kid) (mct-wi-height wi))
    (construct-child kid)
    (setf (mct-wi-kid wi) kid)
    
    (setf (xlib:window-event-mask (mct-wi-window wi))
	  '(:exposure :button-press :button-release :key-press
		      :structure-notify))
    
    (mct-set-wm-props (mct-wi-window wi) name name)
    
    (push (mct-wi-window wi) *wi-to-map*)
    (make-children kid (mct-wi-window wi) (mct-wi-hash wi))
;    (redisplay-subwidget kid)
    (dolist (wi *wi-to-map*)
	    (xlib:map-window wi))
    (setf *wi-to-map* nil)
    (xlib:map-subwindows (mct-wi-window wi))
    (mct-add-handler 'mct-window-handler)
    (mct-register-window wi)
    wi
    ))


(defun resize-window (w new-width new-height)
  (when (or (not (= (widget-width w) new-width))
	    (not (= (widget-height w) new-height)))
	(setf (widget-width w) new-width
	      (widget-height w) new-height)
	(construct-child w)
	(do-all-widgets w #'recompute-windows-from-widget)
;;;	(redisplay-subwidget w)
	))

(defun do-all-widgets (w function)
  (if (eq (widget-type w) :frame)
      (dolist (kid (widget-kids w)) (do-all-widgets kid function)))
  (funcall function w)
  )


;;;  CONSTRUCT-CHILD
;;;
;;;  This recursively descends the widget w, determining locations for
;;;  each widget within its parents, depending on whether :fill
;;;  is set in the parent frame.  It sets everything depending on the
;;;  (widget-width w) and (widget-height w), which must be at least equal
;;;  to w's min-width and min-height.

(defun construct-child (w)
  (when
   (and w (widget-p w) (eq (widget-type w) :frame))
   (let ((x 0) (y 0))
     (cond ((eq (widget-orientation w) :horizontal)
	    (dolist (kid (widget-kids w))
		    (setf (widget-x kid) x
			  (widget-y kid) 0)
		    (incf x (if (and (widget-fill w)
				     (widget-expand-x kid))
				(round
				 (* (widget-min-width kid)
				    (- (widget-width w)
				       (- (widget-min-width w)
					  (widget-min-e-width w))))
				 (widget-min-e-width w))
			      (widget-min-width kid)))
		    (setf (widget-height kid)
			  (if (widget-expand-y kid)
			      (widget-height w)
			    (widget-min-height kid)))
		    (when (widget-lines kid)
			  (setf (widget-lines kid)
				(round (- (widget-height kid)
					  (font-ascent (widget-font kid))
					  (widget-inner-border kid))
				       (font-height (widget-font kid))))
			  ;; Perhaps code to force bottom back on screen
			  )
		    (setf (widget-width kid) (- x (widget-x kid)))
		    (construct-child kid)))
	   ((eq (widget-orientation w) :vertical)
	    (dolist (kid (widget-kids w))
		    (setf (widget-x kid) 0
			  (widget-y kid) y)
		    (incf y (if (and (widget-fill w)
				     (widget-expand-y kid))
				(round
				 (* (widget-min-height kid)
				    (- (widget-height w)
				       (- (widget-min-height w)
					  (widget-min-e-height w))))
				 (widget-min-e-height w))
			      (widget-min-height kid)))
		    (setf (widget-width kid)
			  (if (widget-expand-x kid)
			      (widget-width w)
			    (widget-min-width kid)))
		    (setf (widget-height kid)
			  (- y (widget-y kid)))
		    (when (widget-lines kid)
			  (setf (widget-lines kid)
				(round (- (widget-height kid)
					  (font-ascent (widget-font kid))
					  (widget-inner-border kid))
				       (font-height (widget-font kid))))
			  ;; Perhaps code to force bottom back on screen
			  )
		    (construct-child kid)))
	   (t (error "Incorrect orientation~%"))))
   w))


;;;  SET-MIN-WIDTH
;;;
;;;  This calculates the min-width and min-height for the widget w and all
;;;  of its children.
;;;
;;;  Recall this function whenever the size of any subobject changes
;;;
;;;  Orientation is the orientation of the parent

(defun set-min-width (w &key font (orientation :vertical))
  (when (and w (widget-p w))
	(unless (widget-font w) (setf (widget-font w) font))
	
	(set-widget-gcontexts-to-font w)
	(when (eq (widget-type w) :frame)
	      (setf (widget-min-width w) 0
		    (widget-min-height w) 0
		    (widget-min-e-width w) 0
		    (widget-min-e-height w) 0)
	      (mapcar #'(lambda (kid)
			  (set-min-width
			   kid :font (widget-font w)
			   :orientation (widget-orientation w))
			  (setf (widget-min-width w)
				(cond ((eq (widget-orientation w) :vertical)
				       (max (widget-min-width w)
					    (widget-min-width kid)))
				      ((eq (widget-orientation w) :horizontal)
				       (+ (widget-min-width w)
					  (widget-min-width kid)))
				      (t (error "wrong orientation"))))
			  (setf (widget-min-height w)
				(cond ((eq (widget-orientation w) :horizontal)
				       (max (widget-min-height w)
					    (widget-min-height kid)))
				      ((eq (widget-orientation w) :vertical)
				       (+ (widget-min-height w)
					  (widget-min-height kid)))
				      (t (error "wrong orientation"))))
			  (when (widget-expand-x kid)
				(setf (widget-expand-x w) t)
				(when (eq (widget-orientation w) :horizontal)
				      (incf (widget-min-e-width w)
					    (widget-min-width kid))))
			  (when (widget-expand-y kid)
				(setf (widget-expand-y w) t)
				(when (eq (widget-orientation w) :vertical)
				      (incf (widget-min-e-height w)
					    (widget-min-height kid)))))
		      (widget-kids w)))
	
	(or (widget-min-width w)
	    (setf (widget-min-width w)
		  (calc-min-width w (widget-font w) orientation)))
	(or (widget-min-height w)
	    (setf (widget-min-height w)
		  (calc-min-height w (widget-font w) orientation))))
  w)
	
(defun calc-min-width (w font orientation)
  (let ((min
	 (case (widget-type w)
	       ((:label :button)
		(+ (text-width font (widget-name w))
		   (* 2 (widget-inner-border w))
		   5))
	       (:space (if (eq orientation :vertical)
			   (progn
			     (setf (widget-expand-x w) nil)
			     0)
			 (widget-name w)))
	       (:text (+ (text-width font (longest-line (widget-name w)))
			 (* 2 (widget-inner-border w))
			 5))
	       (:edit (+ (text-width font (widget-name w))
			 (* 2 (widget-inner-border w))
			 5)))))
    (or min 0)))


(defun calc-min-height (w font orientation)
  (or
   (case (widget-type w)
	 ((:label :button :edit)
	  (+ (font-height font) (* 2 (+ (widget-inner-border w)
					(widget-border-width w)))))
	 (:space (if (eq orientation :horizontal)
		     (progn
		       (setf (widget-expand-y w) nil)
		       0)
		   (widget-name w)))
	 (:text (+ (* (length (widget-name w)) (font-height font))
		   (* 2 (+ (widget-inner-border w)
			   (widget-border-width w))))))
   0))


(defun make-children (w parent hash)
  (when (and w (widget-p w))
	(let ((p parent))
	  (when (widget-p parent)
		(setf (widget-parent w) parent)
		(setf p (widget-window parent)))
	  (case (widget-type w)
		(:label
		 (unless (widget-window w)
			 (setf (widget-window w)
			       (mct-borderless-window
				p
				(widget-x w) (widget-y w)
				(widget-width w) (widget-height w)
				:background (xlib:gcontext-foreground
					     (widget-center-gc w))))
			 (setf (gethash (widget-window w) hash)
			       (make-winfo :type :label :widget w))
			 (push (widget-window w) *wi-to-map*))
		 )
		(:button (make-bump w p hash (widget-state w)))
		(:text   (make-bump w p hash :down))
		(:frame
		 (unless (widget-window w)
			 (setf (widget-window w)
			       (mct-borderless-window
				p
				(widget-x w) (widget-y w)
				(widget-width w) (widget-height w)
				:background (xlib:gcontext-foreground
					     (widget-center-gc w))))
			 (setf (gethash (widget-window w) hash)
			       (make-winfo :type :frame :widget w))
			 (push (widget-window w) *wi-to-map*))
		 (dolist (kid (widget-kids w))
			 (make-children kid w hash)))
		))))

(defun make-bump (w parent hash &optional (direction :in))
  (unless (widget-window w)
	  (setf (widget-window w)
		(mct-borderless-window
		 parent
		 (widget-x w) (widget-y w)
		 (widget-width w) (widget-height w)
		 :background (xlib:gcontext-foreground
			      (if (eq direction :up)
				  (widget-up-gc w)
				(widget-down-gc w)))))
	  (push (widget-window w) *wi-to-map*))
  (unless (widget-window2 w)
	  (setf (widget-window2 w)
		(mct-borderless-window
		 (widget-window w)	; Outer window is parent of inner
		 (widget-border-width w)
		 (widget-border-width w)
		 (- (widget-width w)
		    (* 2 (widget-border-width w)))
		 (- (widget-height w)
		    (* 2 (widget-border-width w)))
		 :background (xlib:gcontext-foreground
			      (if (eq direction :up)
				  (widget-up-gc w)
				(widget-down-gc w)))))
	  (push (widget-window2 w) *wi-to-map*))
  
  (setf (xlib:window-backing-store (widget-window w))
	:when-mapped)
  (setf (xlib:window-backing-store (widget-window2 w))
	:when-mapped)
  
  (setf (gethash (widget-window w) hash)
	(make-winfo :type :bump-outer :widget w))
  (setf (gethash (widget-window2 w) hash)
	(make-winfo :type :bump-inner :widget w))
  )

;  (setf (xlib:window-background (widget-window2 w))
;	(if (eq (xlib:gcontext-fill-style (widget-up-gc w)) :tiled)
;	    (xlib:gcontext-tile (widget-up-gc w))
;	  (xlib:gcontext-foreground (widget-up-gc w)))))

;(defun draw-widget (w)
;  (draw-w-outwin w (if (eq (widget-type w) :button) :up :down))
;  (clear-window (widget-window2 w)))

(defun draw-w-outwin (w &optional (direction :up))
  (let ((ul (if (eq direction :up) (widget-lit-gc w) (widget-dark-gc w)))
	(lr (if (eq direction :down) (widget-lit-gc w) (widget-dark-gc w)))
	(wi (widget-window w))
	(width (widget-width w))
	(height (widget-height w))
	(bw (widget-border-width w)))

    (xlib:draw-rectangle wi lr 0 (- height bw) width height t)
    (xlib:draw-rectangle wi lr (- width bw) 0 width height t)

    (xlib::fill-polygon wi ul (list 0 0 width 0 (- width bw) bw bw
				   bw bw (- height bw) 0 height)
		       nil :non-convex)))


(defun clear-window (w &key (which :window) (direction :up))
  (xlib:draw-rectangle (if (eq which :window) (widget-window w)
			 (widget-window2 w))
		       (case direction
			     (:up (widget-up-gc w))
			     (:down (widget-down-gc w))
			     (:center (widget-center-gc w)))
		       0 0
		       (if (eq which :window)
			   (widget-width w)
			 (- (widget-width w) (* 2 (widget-border-width w))))
		       (if (eq which :window)
			   (widget-height w)
			 (- (widget-height w) (* 2 (widget-border-width w))))
		       t))


(defun recompute-windows-from-widget (w)
  (when (widget-window w)
	(xlib:with-state ((widget-window w))
	(setf (xlib:drawable-x (widget-window w)) (widget-x w)
	      (xlib:drawable-y (widget-window w)) (widget-y w)
	      (xlib:drawable-width (widget-window w)) (widget-width w)
	      (xlib:drawable-height (widget-window w)) (widget-height w))))
  (when (widget-window2 w)
	(xlib:with-state ((widget-window w))
	(setf (xlib:drawable-x (widget-window2 w)) (widget-border-width w)
	      (xlib:drawable-y (widget-window2 w)) (widget-border-width w))
	(setf (xlib:drawable-width (widget-window2 w))
	      (- (widget-width w) (* 2 (widget-border-width w))))
	(setf (xlib:drawable-height (widget-window2 w))
	      (- (widget-height w) (* 2 (widget-border-width w)))))))

(defun mct-window (width height &optional
			 (x (screen-center width t))
			 (y (screen-center height nil))
			 (background *black*))
  (xlib:create-window
   :parent *root*
   :x x
   :y y
   :width width :height height
   :background background
   :border *white*
   :border-width 2
   :bit-gravity :north-west
   :colormap *colormap*
   :event-mask '(:exposure :button-press :button-release :structure-notify)))

(defun mct-set-wm-props (window long-name short-name)
  (xlib:set-wm-properties window
			  :name long-name
			  :icon-name short-name
			  :resource-name short-name
			  :resource-class short-name
			  :command '(command)
			  :x (xlib:drawable-x window)
			  :y (xlib:drawable-y window)
			  :width (xlib:drawable-width window)
			  :height (xlib:drawable-height window)
			  :min-width (xlib:drawable-width window)
			  :min-height (xlib:drawable-height window)
;			  :max-width (xlib:drawable-width window)
;			  :max-height (xlib:drawable-height window)
			  :input :on
			  :initial-state :normal))

(defun mct-borderless-window (parent x y width height
				     &key (background *black*))
  (xlib:create-window
   :parent parent :x x :y y :width width :height height
   :border-width 0
   :background background
   :gravity :north-west
   :colormap *colormap*
   :event-mask '(:exposure :button-press :button-release :structure-notify)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  MCT-WINDOW-HANDLER
;;;
;;;  When installed, catches events which affect windows on *wi-list*
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mct-window-handler (&rest event-slots &key window event-key
				 &allow-other-keys)
  (dolist
   (wi *wi-list*)
   (cond ((eq event-key :exposure)
	  (cond ((eq window (mct-wi-window wi))
		 (apply 'mct-exposure-handler 
			(cons wi event-slots)))
		((let ((hash (mct-wi-hash wi)))
		   (let ((winfo (gethash window hash)))
		     (if winfo (let ((type (winfo-type winfo))
				     (w (winfo-widget winfo)))
				 (case type
				       (:bump-inner
					(apply 
					 #'inner-bumpwin-exposure-handler
					 (cons w event-slots)))
				       (:bump-outer
					(apply
					 #'outer-bumpwin-exposure-handler
					 (cons w event-slots)))
				       (:label
					(apply
					 #'label-exposure-handler
					 (cons w event-slots)))
				       (:frame
					(apply
					 #'frame-exposure-handler
					 (cons w event-slots)))))))))))
	 ((and (eq event-key :button-press) (eql *event-level* 1))
	  (button-press (mct-wi-kid wi) window))
	 ((and (eq event-key :configure-notify)
	       (eq window (mct-wi-window wi)))
	  (resize-window (mct-wi-kid wi)
			 (xlib:drawable-width window)
			 (xlib:drawable-height window))))))


(defun mct-button-up-handler (&rest event-slots &key window event-key
				    &allow-other-keys)
  (declare (ignore event-slots window))
  (when (and (eq event-key :button-release) (>= *event-level* 2))
	(setf *quit* *event-level*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  EXPOSURE HANDLERS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;  MCT-EXPOSURE-HANDLER
;;;
;;;

(defun mct-exposure-handler (wi &rest event-slots &key x y width height
			       &allow-other-keys)
  (declare (ignore event-slots))
;  (xlib:draw-rectangle (mct-wi-window wi) (mct-wi-gc wi) x y width height t)
  (redisplay-subwidget (mct-wi-kid wi))
  )


;;;  REDISPLAY-SUBWIDGET
;;;
;;;

(defun redisplay-subwidget (w)
  (when (and w (widget-p w))
	(case (widget-type w)
	      (:text (redisplay-text w))
	      (:button (redisplay-button-text w))
	      (:label (redisplay-label-text w))
	      (:space)
	      (:frame (redisplay-frame w)))))

(defun redisplay-label-text (w)
  (xlib:draw-glyphs (widget-window w)
		    (if *color* (widget-itext-gc w) (widget-text-gc w))
		    (widget-inner-border w)
		    (+ (widget-inner-border w)
		       (font-ascent (widget-font w)))
		    (widget-name w)))

(defun redisplay-button-text (w)
  (let ((x (round (- (widget-width w) (* 2 (widget-border-width w))
		     (xlib:text-extents (widget-font w)
					(widget-name w))) 2))
	(y (+ (widget-inner-border w)
	      (font-ascent (widget-font w)))))
    (xlib:draw-glyphs (widget-window2 w)
		      (if *color* (widget-itext-gc w)
			(if (eq (widget-state w) :up)
			    (widget-text-gc w)
			  (widget-itext-gc w)))
		      x y (widget-name w))))

;;;  REDISPLAY-TEXT
;;;
;;;  Redraw a text widget

(defun redisplay-text (w)
  (let ((cnt 0))
    (recalc-text w)
    (xlib:clear-area (widget-window2 w))
    (dolist (line (widget-name w))
	    (xlib:draw-glyphs
	     (widget-window2 w) (widget-itext-gc w)
	     (widget-inner-border w)
	     (+ (widget-inner-border w)
		(font-ascent (widget-font w))
		(* cnt (font-height (widget-font w))))
	     line)
	    (incf cnt))))


;;;  REDISPLAY-LABEL
;;;
;;;

(defun redisplay-label (w)
;  (clear-window w :direction :center)
  (redisplay-label-text w))


;;;  REDISPLAY-FRAME
;;;
;;;

(defun redisplay-frame (w)
;  (clear-window w :direction :center)
;  (dolist (kid (widget-kids w))
;	  (redisplay-subwidget kid))
  )



;;;  OUTER-BUMPWIN-EXPOSURE-HANDLER
;;;
;;;

(defun outer-bumpwin-exposure-handler (w &rest event-slots &key count
					 &allow-other-keys)
  (declare (ignore event-slots))
  (when (zerop count)
	(draw-w-outwin w (if (eq (widget-type w) :button)
			     (widget-state w) :down))))


;;;  INNER-BUMPWIN-EXPOSURE-HANDLER
;;;
;;;

(defun inner-bumpwin-exposure-handler (w &rest event-slots &key count
					 &allow-other-keys)
  (declare (ignore event-slots))
  (when (zerop count)
	(if (eq (widget-type w) :button)
	    (clear-window w :which :window2
			  :direction (widget-state w)))
;	(clear-window w :which :window2
;		      :direction (if (eq (widget-type w) :button)
;				     (widget-state w) :down))
	(redisplay-subwidget w)
	))


;;;  LABEL-EXPOSURE-HANDLER
;;;
;;;

(defun label-exposure-handler (w &rest event-slots &key count
				 &allow-other-keys)
  (declare (ignore event-slots))
  (when (zerop count)
	(redisplay-label w)
	))


;;;  FRAME-EXPOSURE-HANDLER
;;;
;;;

(defun frame-exposure-handler (w &rest event-slots &key count
				 &allow-other-keys)
  (declare (ignore event-slots))
  (when (zerop count)
	(redisplay-frame w)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  ACTION CALLBACKS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;  BUTTON-PRESS
;;;
;;;  The user clicked in window.  If we find it somewhere in w, we
;;;  take appropriate action for that click.  Recursive on frames.

(defun button-press (w window)
  (when (and w (widget-p w))
	(cond ((or (eq window (widget-window w))
		   (eq window (widget-window2 w)))
	       (handle-click w window))
	      ((eq (widget-type w) :frame)
	       (mapcar #'(lambda (x) (button-press x window))
		       (widget-kids w))))))

;;; HANDLE-CLICK
;;;
;;; We know the click actually occurred

(defun handle-click (w window)
  (case (widget-type w)
	(:frame)
	(:text (redisplay-text w))
	(:button (press-button w))
	(:space)
	(:label)
	(:edit (edit-field w))
	))


;;;  PRESS-BUTTON
;;;
;;;  Flashes the button pressed

(defun press-button (w)
  (setf (widget-state w) :down)

  (when (widget-down-name w)
	(setf (widget-name w) (widget-down-name w)))
  (outer-bumpwin-exposure-handler w :count 0)
  (inner-bumpwin-exposure-handler w :count 0)
  (mct-funcall (widget-downf w) w)

  (mct-add-handler 'mct-button-up-handler)
  (mct-remove-handler 'mct-window-handler)
  (mct-event-loop (incf *event-level*))
  (setf *quit* 0)
  (mct-remove-handler 'mct-button-up-handler)
  (mct-add-handler 'mct-window-handler)
  (decf *event-level*)

  (mct-funcall (widget-upf w) w)

  (when (widget-up-name w)
	(setf (widget-name w) (widget-up-name w)))
  (setf (widget-state w) :up)
  (outer-bumpwin-exposure-handler w :count 0)
  (inner-bumpwin-exposure-handler w :count 0))
  

(defun mct-funcall (f &rest args)
  (when f (apply f args)))


(defun recalc-text (w)
  (setf (widget-name w) (lines (mct-funcall (widget-getf w) w)
			       (widget-line w)
			       (widget-lines w))))

;;;  LINES
;;;
;;;  Breaks a string into lines

(defun lines (string &optional (start 0) (count (numlines string)))
  (unless (> start (numlines string))
  (let (out)
    (with-input-from-string
     (s string)
     (dotimes (i start)
	      (read-line s))
     (do ((i 0 (incf i))
	  (line (read-line s nil) (read-line s nil)))
	 ((or (= i count) (null line)) (reverse out))
	 (push line out))))))

(defun numlines (string)
  (1+ (count #\Newline string)))



(defun edit-field (w)
  (declare (ignore w)))

(defun font-height (font)
  (+ (font-ascent font) (font-descent font)))

(defun font-ascent (font)
  (xlib:max-char-ascent font))

(defun font-descent (font)
  (xlib:max-char-descent font))

(defun text-width (font text)
  (xlib:text-width font text))

(defun longest-line (lines)
  (let ((maxlen 0) maxline)
    (dolist (line lines)
	    (when (> (length line) maxlen)
		  (setf maxlen (length line))
		  (setf maxline line)))
    maxline))

(defun mct-close-window (w)
  (format t "Goodbye!!!  (but I really won't go)~%")
  )


;;;  SET-WIDGET-GCONTEXTS-TO-FONT
;;;
;;;  Sets the gcontexts stored in the widget to ones which use the
;;;  desired font

(defun set-widget-gcontexts-to-font (w)
  (let ((f (widget-font w)))
    (if *color*
	(setf (widget-lit-gc w) *grey75-gc*
	      (widget-dark-gc w) *black-gc*
	      (widget-up-gc w) *grey60-gc*
	      (widget-center-gc w) *grey50-gc*
	      (widget-down-gc w) *grey25-gc*
	      (widget-text-gc w) (mct-gcontext :font f)
	      (widget-itext-gc w) (mct-gcontext :invid-p t :font f))
      (setf (widget-lit-gc w) *grey75-gc*
	    (widget-dark-gc w) *grey25-gc*
	    (widget-up-gc w) *white-gc*
	    (widget-center-gc w) *white-gc*
	    (widget-down-gc w) *black-gc*
	    (widget-text-gc w) (mct-gcontext :font f)
	    (widget-itext-gc w) (mct-gcontext :invid-p t :font f)))))
