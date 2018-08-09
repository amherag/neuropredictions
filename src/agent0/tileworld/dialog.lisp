;;;
;;; dialog.lisp
;;;
;;; Core dialog-box definitions for x tileworld dialog boxes.
;;;

(in-package 'xtile)

;;
;; The form of a dialog box in tileworld will be as follows:
;; A bunch of fields, each with a label on the left, and a value-bump
;; on the right.  One field per line.
;; A bunch of buttons, each with a label in it. One button per line.
;;

(defvar *dialog-list*)
(defparameter *dialog-font-name* "*typewriter-medium-*-20*")
(defparameter *dialog-label-border* 5)
(defparameter *dialog-value-border* 2)

(defun dl-fnt-bl () *font-baseline*)
(defun dl-lbl-bdr () *dialog-label-border*)
(defun dl-vlu-bdr () *dialog-value-border*)
(defun dl-bg () *grey25-gc*)		;dialog background graphics context
(defun dl-lbl-gc () *gcontext*)
(defun dl-vlu-gc () *gcontext*)
(defun dl-fnt () *font*)
(defun dl-fnt-ht () *font-height*)
  
;;
;; spec structures are only for initial specification of what
;; the dialog box looks like.
;;

(defstruct dialog-spec
  name					;Title of dialog
  fields				;list of field-spec structures
  buttons				;list of button-spec structures
  )
(defstruct field-spec
  label					;Label to appear on the left
  getf					;Function to provide value string
  testf					;Test for legality of new values
  putf					;Use a given new value string
  field					;actual field - internal use
  )
(defstruct button-spec
  label					;Label to appear in button
  actionf				;Function to call when pressed
  )

;;
;; Here are the actual dialog, field, and button structures.
;;

(defstruct dialog
  name
  fields				;list of dialog fields
  buttons				;list of dialog buttons
  width					;width of the dialog
  height				;height of the dialog
  window				;window that it takes up
  hash					;hash table for window->winfo
  )

(defstruct winfo
  type					;:bump-inner or :bump-outer
  field					;the field it goes with
  )

(defstruct field
  x
  y					;where field starts vertically
  label					;label on the field
  value-bump				;the negative bump holding the value
  value					;value currently in place
  getf       ;provides value string
  testf      ;checks value string
  putf       ;applies value string
  )

(defstruct button
  x 
  y 
  label 
  bump
  actionf
  )

(defun create-new-dialog (dspec)
  (let ((di (make-dialog :name (dialog-spec-name dspec)
			:fields (construct-fields (dialog-spec-fields dspec))
			:buttons (construct-buttons (dialog-spec-buttons dspec))
			:hash (make-hash-table)
			)))
    (setf (dialog-width di) (max (max-field-width (dialog-fields di))
				(max-button-width (dialog-fields di))))
    (setf (dialog-height di)
	  (* (+ (length (dialog-fields di)) (length (dialog-buttons di)))
	     *font-height*))
    (setf (dialog-window di)
	  (xtw-default-window (dialog-width di) (dialog-height di)))
    (setf (window-event-mask (dialog-window di))
	  '(:exposure :button-press :key-press))
    (xtw-set-wm-props (dialog-window di)
		      (dialog-name di)
		      (dialog-name di))
    (make-dialog-children di)
    (map-window (dialog-window di))
    (map-subwindows (dialog-window di))
    (dolist (f (dialog-fields di))
	    (map-window (bump-inner-window (field-value-bump f))))
    (dolist (b (dialog-buttons di))
	    (map-window (bump-inner-window (button-bump b))))
    ;(draw-dialog di)
    ;(display-finish-output *display*)
    (xtw-add-handler 'dialog-handler)
    (setf *dialog-list* (cons di *dialog-list*))
    di
    ))

(defun construct-buttons (bspecs)
  (if bspecs
      (let ((bspec (car bspecs))
	    button)
	(setf button (make-button :label (button-spec-label bspec)
				  :actionf (button-spec-actionf bspec)
				  ))
	(cons button (construct-buttons (cdr bspecs))))))

(defun max-button-width (buttons)
  (let ((maxwidth 0)
	(wid 0))
    (loop
     (if (null buttons) (return))
     (setf wid (+ (text-width *font* (button-label (car buttons)))
		  0))
     (setf buttons (cdr buttons)))
    maxwidth))


(defun construct-fields (fspecs)
  (if fspecs
      (let ((fspec (car fspecs))
	    field)
	(setf field (make-field :label (field-spec-label fspec)
				:getf (field-spec-getf fspec)
				:testf (field-spec-testf fspec)
				:putf (field-spec-putf fspec)))
	(setf (field-spec-field fspec) field)
	(setf (field-value field)
	      (funcall (field-getf field)))
	(cons field (construct-fields (cdr fspecs))))))

(defun max-field-width (fields)
  (let ((maxwidth 0) 
	(wid 0))
    (loop
     (if (null fields) (return))
     (setf wid (+ (text-width *font* (field-label (car fields)))
		  (text-width *font* (field-value (car fields)))))
     (if (> wid maxwidth) (setf maxwidth wid))
     (setf fields (cdr fields)))
    maxwidth))

(defun make-dialog-children (d)
  (let* ((pw (dialog-window d))
	 (fields (dialog-fields d))
	 (buttons (dialog-buttons d))
	 (hash (dialog-hash d))
	 (n 0)
	 (y 5)
	 (x 0)
	 )
    (dolist (field fields)
	    (let* ((b (make-bump))
		   (label-text-wid (text-width *font* (field-label field)))
		   (label-border *dialog-label-border*)
		   (label-wid (+ label-text-wid (* 2 label-border)))
		   (label-ht (+ *font-height* (* 2 label-border)))
		   (value-text-wid (text-width *font* (field-value field)))
		   (value-border *dialog-value-border*)
		   (value-wid (+ value-text-wid (* 2 value-border)))
		   (value-ht (+ *font-height* (* 2 value-border)))
		   )
	      (setf (field-y field) y)
	      (setf (field-value-bump field) b)
	      (setf (bump-parent b) pw)
	      (setf (bump-x b) label-wid)
	      (setf (bump-y b) y)
	      (setf (bump-inner-height b) value-ht)
	      (setf (bump-inner-width b) value-wid)
	      (setf (bump-ratio b) 0.8)
	      (setf (bump-upper-left-gc b) *grey50-gc*)
	      (setf (bump-center-gc b) *black-gc*)
	      (setf (bump-lower-right-gc b) *grey50-gc*)
	      (fill-out-bump-description b)
	      (setf (window-backing-store (bump-outer-window b)) :when-mapped)
	      (setf (window-backing-store (bump-inner-window b)) :when-mapped)
	      (setf y (+ y (max (bump-outer-height b) label-ht)))
	      (setf x (max x (+ (drawable-x (bump-outer-window b)) (bump-outer-width b))))
	      (setf (gethash (bump-inner-window b) hash)
		    (make-winfo :type :bump-inner :field field))
	      (setf (gethash (bump-outer-window b) hash)
		    (make-winfo :type :bump-outer :field field))
	      )
	    (setf n (1+ n)))
    (dolist (button buttons)
	    (let* ((b (make-bump))
		   (label-text-wid (text-width *font* (button-label button)))
		   (label-border *dialog-value-border*)
		   (label-wid (+ label-text-wid (* 2 label-border)))
		   (label-ht (+ *font-height* (* 2 label-border)))
		   )
	      (setf (button-y button) y)
	      (setf (button-bump button) b)
	      (setf (bump-parent b) pw)
	      (setf (bump-x b) 5)
	      (setf (bump-y b) y)
	      (setf (bump-inner-height b) label-ht)
	      (setf (bump-inner-width b) label-wid)
	      (setf (bump-ratio b) 0.8)
	      (setf (bump-upper-left-gc b) *grey50-gc*)
	      (setf (bump-center-gc b) *grey25-gc*)
	      (setf (bump-lower-right-gc b) *black-gc*)
	      (fill-out-bump-description b)
	      (setf (window-backing-store (bump-outer-window b)) :when-mapped)
	      (setf (window-backing-store (bump-inner-window b)) :when-mapped)
	      (setf y (+ y (max (bump-outer-height b) label-ht)))
	      (setf x (max x (+ (drawable-x (bump-outer-window b)) (bump-outer-width b))))
	      (setf (gethash (bump-inner-window b) hash)
		    (make-winfo :type :bump-inner :field button))
	      (setf (gethash (bump-outer-window b) hash)
		    (make-winfo :type :bump-outer :field button))
	      )
	    (setf n (1+ n)))
    (setf (drawable-height pw) (+ 5 (round y)))
    (setf (drawable-width pw) (+ 10 (round x)))
    ))

(defun draw-dialog (d)
  (let* ((pw (dialog-window d))
	 (fields (dialog-fields d))
	 (n 0)
	 (gc (create-gcontext :drawable pw
			      :background *black* :foreground *white*
			      :font *font*)))
    (map-window pw)
    (map-subwindows pw)
    (display-finish-output *display*)
    (draw-rectangle pw *grey25-gc* 0 0 (drawable-width pw) 
		    (drawable-height pw) t)
    (loop
     (if (null fields) (return))
     (let ((field (car fields)))
       (draw-glyphs pw gc *dialog-label-border* 
		    (+ (round (field-y field))
		       *font-baseline* *dialog-label-border*)
		    (field-label field))
       (map-window (bump-inner-window (field-value-bump field)))
       (draw-bump (field-value-bump field))
       (draw-glyphs (bump-inner-window (field-value-bump field)) gc
		    *dialog-value-border*
		    (round (+ *font-baseline* *dialog-value-border*))
		    (field-value field)))
     (setf fields (cdr fields))
     (setf n (1+ n)))))

(defun dialog-handler (&rest event-slots &key window event-key
			     &allow-other-keys)
  (dolist (di *dialog-list*)
	  (cond ((eq event-key :exposure)
		 (cond ((eq window (dialog-window di))
			(apply 'dialog-exposure-handler 
			       (cons di event-slots)))
		       ((let ((hash (dialog-hash di)))
			  (let ((winfo (gethash window hash)))
			    (if winfo (let ((type (winfo-type winfo))
					  (f (winfo-field winfo)))
				      (if (eq type :bump-inner)
					  (apply 
					   #'inner-bumpwin-exposure-handler
					   (cons f event-slots))
					(apply
					 #'outer-bumpwin-exposure-handler
					 (cons f event-slots))))))))))
		((and (eq event-key :button-press) (eql *event-level* 1))
		 (dolist (f (dialog-fields di))
			 (if (eq window (bump-inner-window 
					 (field-value-bump f)))
			     (edit-field di f)))
		 (dolist (b (dialog-buttons di))
			 (if (eq window (bump-inner-window (button-bump b)))
			     (press-button b)))))))

(defun press-button (b)
  (let ((w (bump-inner-window (button-bump b))))
    (unmap-window w)
    (map-window w)
    ;(draw-rectangle w *white-gc*
    ;0 0 (drawable-width w) (drawable-height w))
    ;(display-finish-output *display*)
    ;(inner-bumpwin-exposure-handler b :count 0)
    (funcall (button-actionf b))
    ))

(defun find-window-field (dialog window)
  (do ((fields (dialog-fields dialog) (cdr fields)))
      ((null fields))
      (let* ((field (car fields))
	     (bump (field-value-bump field)))
	(if (or (eq window (bump-outer-window bump))
		(eq window (bump-inner-window bump)))
	    (return field)))))

(defun find-window-button (dialog window)
  (do ((buttons (dialog-buttons dialog) (cdr buttons)))
      ((null buttons))
      (let* ((button (car buttons))
	     (bump (button-bump button)))
	(if (or (eq window (bump-outer-window bump))
		(eq window (bump-inner-window bump)))
	    (return button)))))

(defun dialog-exposure-handler (d &rest event-slots &key x y width height
				  &allow-other-keys)
  (declare (ignore event-slots))
  (draw-rectangle (dialog-window d) (dl-bg) x y width height t)
  (dolist (f (dialog-fields d))
	  (if (or (<= (field-y f) y (+ (field-y f) (dl-fnt-bl) 
				      (dl-lbl-bdr)))
		  (<= y (field-y f) (+ y height)))
	      (draw-glyphs (dialog-window d) (dl-lbl-gc)
			   (dl-lbl-bdr)
			   (+ (round (field-y f))
			      (dl-fnt-bl) (dl-lbl-bdr))
			   (field-label f)))))

(defun outer-bumpwin-exposure-handler (f &rest event-slots &key count
					 &allow-other-keys)
  (declare (ignore event-slots))
  (when (zerop count)
	(draw-b-outwin (if (field-p f) 
			   (field-value-bump f)
			 (if (button-p f)
			     (button-bump f))
			 ))))

(defun inner-bumpwin-exposure-handler (f &rest event-slots &key count
					 &allow-other-keys)
  (declare (ignore event-slots))
  (when (zerop count)
	(let (bump text)
	  (if (field-p f) 
	      (progn (setf bump (field-value-bump f))
		     (setf text (field-value f)))
	    (progn (setf bump (button-bump f))
		   (setf text (button-label f))))
	  (draw-glyphs (bump-inner-window bump)
		       (dl-vlu-gc)
		       (dl-vlu-bdr)
		       (round (+ (dl-fnt-bl) (dl-vlu-bdr)))
		       text)
	  (if (and (field-p f) (> (drawable-width (bump-inner-window bump))
				  (round (+ (text-width (dl-fnt) text)
					    (* 2 (dl-vlu-bdr))))))
	      (draw-rectangle (bump-inner-window bump)
			      (dl-vlu-gc) (+ (dl-vlu-bdr)
					     (text-width (dl-fnt)
							 text))
			      (dl-vlu-bdr) (text-width (dl-fnt) " ")
			      (dl-fnt-ht) t)))))

(defun xtw-borderless-window (parent x y width height)
  (create-window
   :parent parent :x x :y y :width width :height height
   :border-width 0
   :background *black*
   :colormap *colormap*
   :event-mask '(:exposure :button-press)))

(defvar *dlg-edit-mode* nil)

(defun edit-field (d f)
  (let ((*dlg-edit-mode* t)
	(*busy* nil)
	(handler #'(lambda (&rest event-slots &key event-key
				  &allow-other-keys)
		     (if (and *dlg-edit-mode* (eq event-key :key-press))
			 (apply 'handle-key `(,d ,f . ,event-slots))))))
    (xtw-add-handler handler)
    (rdrw-field-bump d f)
    (xtw-event-loop nil)
    (xtw-remove-handler handler)
    ))

(defun rdrw-field-bump (dialog field &optional no-prompt)
  (declare (ignore dialog))
  (let* ((value-text-wid 
	  (text-width *font* (field-value field)))
	 (extra-text-wid (text-width *font* " "))
	 (value-wid (+ value-text-wid (if no-prompt 0 extra-text-wid)
		       (* 2 *dialog-value-border*)))
	 (bump (field-value-bump field)))
    (set-bump-width bump value-wid)
    (draw-glyphs (bump-inner-window bump) *gcontext*
		 *dialog-value-border*
		 (round (+ *font-baseline* *dialog-value-border*))
		 (field-value field))
    (unless no-prompt
	    (draw-rectangle (bump-inner-window bump) *gcontext*
			    (+ *dialog-value-border* value-text-wid)
			    *dialog-value-border*
			    extra-text-wid *font-height* t))
    (display-finish-output *display*)
    ))

(defun handle-key (dialog field &rest event-slots &key code &allow-other-keys)
  (declare (ignore event-slots))
  (let* ((char (keycode->character *display* code 0))
	 (charstring (string char))
	 (oldstring (field-value field))
	 (newstring (concatenate 'string oldstring charstring))
	 )
    (cond ((eq char #\Rubout)
	   (if (not (equal oldstring ""))
	       (setf (field-value field)
		     (remove-if #'(lambda (_) (declare (ignore _)) t) 
				oldstring :from-end t :count 1)))
	   (rdrw-field-bump dialog field)
	   nil)
	  ((eq char #\return)
	   (when (funcall (field-testf field) (field-value field))
		 (funcall (field-putf field) (field-value field))
		 (setf (field-value field)
		       (funcall (field-getf field)))
		 (rdrw-field-bump dialog field t)
		 (if *dlg-edit-mode* (setf *quit* t))
		 t))
	  ((setf (field-value field) newstring)
	   (rdrw-field-bump dialog field)
	   nil))))

(defun update-fields (d)
  (dolist (f (dialog-fields d))
	  (setf (field-value f) (funcall (field-getf f)))
	  (rdrw-field-bump d f t)))

(defun redisplay-field-from-spec (fspec)
  (let* ((field (field-spec-field fspec))
	 (bump (field-value-bump field))
	 (iw (bump-inner-window bump)))
    (setf (field-value field)
	  (funcall (field-getf field)))
    (clear-inner-window bump)
    (draw-glyphs iw (dl-vlu-gc)
		 (dl-vlu-bdr)
		 (round (+ (dl-fnt-bl) (dl-vlu-bdr)))
		 (field-value field))))

