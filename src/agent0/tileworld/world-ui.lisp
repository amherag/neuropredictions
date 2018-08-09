;;
;; World user-interface functions.
;;

(in-package 'xtile)

(defvar *arrow-gc*)
(defvar *object-window*)
(defvar *object-window-mapped*)
(defvar *object*)
(defvar *objwin-border*)
(defvar *object-window-width*)
(defvar *object-window-height*)

(defun xtw-with-world-ui (form)
  (let ((*world-window* nil)
	(*world-pixmaps* nil)
	(*old-world-board-size* nil)
	(*world-dialog* nil)
	(*object-window* nil)
	(*object-window-mapped* nil)
	(*object* nil)
	)
    (unwind-protect
	(eval form)
      (when *world-window*
	    (destroy-window *world-window*)
	    (setf *world-window* nil)))))

;;;
;;; The world user-interface's reaction to a change in experiment 
;;; parameters is to resize the old window if the new world size
;;; is different from the old one, to clear the old display, and
;;; to note that no characters or labels in the new world have been
;;; displayed yet.
;;;

(defun xtw-world-ui-new-param ()
  ;;(if (or (not (boundp '*world*)) (null *world*))
  ;;      (setf *world* wnnn))		;just a dummy kind of thing, fossil
  ;;;(setf *world* *world-old-core*)	;fossil
  (setf (tw-rows *world*) *world-board-size*)
  (setf (tw-cols *world*) *world-board-size*)
  (if *world-window* (resize-world-win))
  (setf *grid-cache* nil)
  (setf *grid-label-cache* nil)
  (if *world-dialog* (xtw-world-new-dialog)) ;update fields in dialog
  (setf *old-world-board-size* *world-board-size*)
  )

(defun resize-world-win ()
  (when (and (boundp '*old-world-board-size*)
	     *old-world-board-size*
	     (not (eql *old-world-board-size* *world-board-size*)))
	(setf *world-window-width* (* *world-board-size* *cell-width*))
	(setf *world-window-height* (* *world-board-size* *cell-height*))
	(setf (drawable-width *world-window*) *world-window-width*)
	(setf (drawable-height *world-window*) *world-window-height*)
	;(clear-area *world-window*
	;	    :x 0 :y 0
	;	    :width *world-window-width*
	;	    :height *world-window-height*
	;	    )
	)
  )

;;;
;;; xtw-world-ui-init
;;;
;;; executed by xtw-world-prepare-for-display
;;; if *world-window* is nil.
;;;
;;; xtw-world-ui-init sets up values in a logical way, with the most
;;; fundamental values set first, and values that depend on them set
;;; up later in terms of them.  the entire world display window could
;;; be changed just by changing *label-font-name*.
;;;

(defun xtw-world-ui-init ()
  (setf *label-font* (open-font *display* *label-font-name*))
  (setf *label-baseline* (max-char-ascent *label-font*)) 
  (setf *label-descent* (max-char-descent *label-font*))
;;;*label-descent* would normally be (max-char-descent *label-font*)
;;;but we know labels will be numbers only, which have no descenders,
  (setf *label-descent* 1)		;so this is small.
  (setf *label-text-height* (+ *label-baseline* *label-descent*))
;;;Width is twice the size of an "O" since we need room for 2 numbers
;;;(at least) in a square.
  (setf *label-text-width* (* 2 (text-width *label-font* "O")))
  (setf *label-hborder* 0)
  (setf *label-vborder* 0)
  (setf *label-width* (round (+ *label-text-width* (* 2 *label-hborder*))
				*hole-inner-size*))
  (setf *label-height* (round (+ *label-text-height* (* 2 *label-vborder*))
				 *hole-inner-size*))

  (setf *char-font-name* "*typewriter-medium-*-20-*")
  (setf *char-font* (open-font *display* *char-font-name*))
  (setf *char-baseline* (max-char-ascent *char-font*))
  (setf *char-descent* (max-char-descent *char-font*))
  (setf *char-descent* 0)
  (setf *char-text-height* (+ *char-descent* *char-baseline*))
  (setf *char-text-width* (text-width *char-font* "O"))
  (setf *char-hborder* 2)
  (setf *char-vborder* 2)
  (setf *char-width* (+ *char-text-width* (* 2 *char-hborder*)))
  (setf *char-height* (+ *char-text-height* (* 2 *char-vborder*)))

;;;Squares must be tall and wide enought to hole both labels and chars.
  (setf *square-size* (max *label-width* 
			   *label-height* 
			   *char-width*
			   *char-height*
			   *min-square-size*))
;;;And our cells *are* actually square.
  (setf *cell-width* *square-size*)
  (setf *cell-height* *square-size*)

;;;Now borders have to be recalculated, since there might be more room.

  (setf *label-vborder* (round (- *cell-height* *label-text-height*) 2))
  (setf *label-hborder* (round (- *cell-width* *label-text-width*) 2))

  (setf *char-vborder* (round (- *cell-height* *char-height*) 2))
  (setf *char-hborder* (round (- *cell-width* *char-width*) 2))

;;;Finally, we're ready to do the world window.

  (setf *world-window-width* (* (tw-cols *world*) *cell-width*))
  (setf *world-window-height* (* (tw-rows *world*) *cell-height*))
  (setf *world-window-x* (screen-center *world-window-width* t))
  (setf *world-window-y* (screen-center *world-window-height* nil))
  (setf *world-window* (create-window
			:parent (screen-root *screen*)
			:x *world-window-x*
			:y *world-window-y*
			:width *world-window-width*
			:height *world-window-height*
			:background *black*
			:border *white*
			:border-width 2
			:colormap (screen-default-colormap *screen*)
			:event-mask '(:exposure :button-press)))
  (setf *world-gc* (create-gcontext
			 :drawable *world-window*
			 :background *black*
			 :foreground *white*
			 :exposures :off ;cuz we do copy-area calls
			 :font *font*))
  (setf *char-gc* (create-gcontext
			      :drawable *world-window*
			      :background *black*
			      :foreground *white*
			      :font *char-font*))
  (setf *label-gc* (create-gcontext
			       :drawable *world-window*
			       :background *black*
			       :foreground *white*
			       :font *label-font*))
  (set-wm-properties *world-window*
		     :name "Tile World"
		     :icon-name "tile world"
		     :resource-name "tileworld"
		     :resource-class "Tileworld"
		     :command (list* 'tileworld)
		     :x *world-window-x*
		     :y *world-window-y*
		     :width *world-window-width*
		     :height *world-window-height*
		     :min-width *world-window-width*
		     :min-height *world-window-height*
		     :max-width *world-window-width*
		     :max-height *world-window-height*
		     :input :off
		     :initial-state :normal)
  ;;; object window setup
  (setf *objwin-border* (max (round *char-width* 2)
			     (round *char-height* 2)))
  (setf *object-window-width* (+ (text-width *font* "Object Identification")
				 (* 2 *objwin-border*)))
  (setf *object-window-height* (+ (* 5 *font-height*)
				  (* 2 *objwin-border*)))
  (setf *object-window* (xtw-default-window *object-window-width*
					    *object-window-height*))
  (xtw-set-wm-props *object-window* "Object Information" "object information")

  ;;; create pixmaps for grid-cell contents
  (xtw-prepare-pixmaps)

;;;Finally, map the window and add the world-user-interface handler.
  (map-window *world-window*)
  (xtw-add-handler 'xtw-world-handler)
)  

(defun xtw-world-handler (&rest event-slots &key window event-key 
				&allow-other-keys)
  (cond ((eq window *world-window*)
	 (case event-key
	       ((:exposure) (apply 'xtw-world-exposure-handler event-slots)) 
	       ((:button-press) (apply 'xtw-world-button-press-handler 
				       event-slots))))
	((eq window *object-window*)
	 (cond ((eq event-key :exposure)
		(apply 'objwin-expose event-slots))
	       ((eq event-key :button-press)
		(apply 'objwin-button event-slots))))))


(defun xtw-world-exposure-handler (&rest event-slots 
					 &key x y width height count
					 &allow-other-keys)
  (declare (ignore event-slots))
  (xtw-world-prepare-for-display *world*)
  (do ((j (truncate x *cell-width*) (1+ j)))
      ((> j (min (truncate (+ x width) *cell-width*)
		 (1- (tw-cols *world*)))))

      (do ((i (truncate y *cell-height*) (1+ i)))
	  ((> i (min (truncate (+ y height) *cell-height*)
		     (1- (tw-rows *world*)))))

	  (let ((ch (aref (tw-grid *world*) i j))
		(label (xtw-cell-label *world* i j)))
	    (xtw-world-cell-display i j ch)
	    (setf (aref *grid-cache* i j) ch)
	    (xtw-world-cell-label i j label)
	    (setf (aref *grid-label-cache* i j) label)
	    )))
  (unless (zerop count)
	  (process-event *display* :timeout nil :handler #'xtw-world-handler))
  t)

;;;
;;; xtw-world-prepare-for-display
;;;
;;; must be called before any attempt to display in the world.
;;; makes sure everything needed for world-display exists.
;;;

(defun xtw-world-prepare-for-display (world)  
  (unless *world-window* (xtw-world-ui-init))
  (if (null *grid-cache*)
      (setf *grid-cache* (make-array (list (tw-rows world) (tw-cols world))
				     :element-type 'standard-char
				     :initial-element #\x)))
  (if (null *grid-label-cache*)
      (setf *grid-label-cache* 
	    (make-array (list (tw-rows world) (tw-cols world))
			:element-type 'string
			:initial-element "")))
  )

;;;;;
;;;;; xtw-world-display
;;;;;
;;;;; Given a world, presumably at the end of a tw-step, displays the
;;;;; state of the world in an X window.
;;;;;
;;;;; Compares the characters in the world's tw-grid with an internal 
;;;;; cache, and only does graphics for the characters that have changed.
;;;;;
;;;;; Also scans tw-grid-which-hole and completely redoes hole score
;;;;; displays.
;;;;;
;;;;; New addition: handle exposure events.
;;;;; 

(defvar *grid-cache* nil)		;For caching cell id characters
(defvar *grid-label-cache* nil)		;For caching labels in cells

(defun xtw-world-display (world)
  (xtw-world-prepare-for-display world)
  (handle-exposure-events)
  (dotimes 
   (i (tw-rows world))
   (dotimes
    (j (tw-cols world))
    (let ((ch (aref (tw-grid world) i j))
	  (label (xtw-cell-label world i j)))
      (unless (eql ch (aref *grid-cache* i j))
	      (xtw-world-cell-display i j ch)
	      (setf (aref *grid-cache* i j) ch)
	      )
      (unless (equal label (aref *grid-label-cache* i j))
	      (xtw-world-cell-label i j label)
	      (setf (aref *grid-label-cache* i j) label)
	      )
      )
    )
   )
  (display-finish-output *display*)
  )

(defun xtw-cell-label (world row col)
  (let* ((grid-which-hole (tw-grid-which-hole world))
	 (hole (aref grid-which-hole row col)))
    (if hole 
	(let ((cells (obj-cells hole)))
	  (if cells
	      (let* ((first-cell (car cells))
		     (crow (cell-row first-cell))
		     (ccol (cell-col first-cell)))
		(if (and (eql crow row) (eql ccol col))
		    (format nil "~s" (obj-score hole))
		  ""))
	    ""))
      "")))

(defun xtw-world-cell-display (row col ch)
  (if (assoc ch *world-pixmaps*)
      (copy-area (cdr (assoc ch *world-pixmaps*)) *world-gc* 
		 0 0 *square-size* *square-size*
		 *world-window* 
		 (* col *cell-width*) (* row *cell-height*))
    (progn
      (clear-area *world-window* 
		  :x (* col *cell-width*)
		  :y (* row *cell-height*)
		  :width *cell-width* :height *cell-height*)
      
      (draw-glyph *world-window* *char-gc*
		  (+ (* col *cell-width*) *char-hborder*)
		  (+ (* row *cell-height*) 
		     *char-vborder* 
		     *char-baseline*)
		  ch)))
  (if (eql ch bare-hole-char)
      (xtw-draw-hole-divisions row col)))

(defun xtw-draw-hole-divisions (row col)
  (let ((hole (aref (tw-grid-which-hole *world*) row col)))
    (and (eql bare-hole-char (aref (tw-grid *world*) row (1- col)))
	 (not (eql (aref (tw-grid-which-hole *world*) row (1- col))
		   hole))
	 (draw-line *world-window* *world-gc* 
		    (* col *cell-width*) (* row *cell-height*)
		    (* col *cell-width*) (1- (* (1+ row) *cell-height*))))
    (and (eql bare-hole-char (aref (tw-grid *world*) row (1+ col)))
	 (not (eql (aref (tw-grid-which-hole *world*) row (1+ col))
		   hole))
	 (draw-line *world-window* *world-gc* 
		    (* (1+ col) *cell-width*) (* row *cell-height*)
		    (* (1+ col) *cell-width*) (1- (* (1+ row) *cell-height*))))
    (and (eql bare-hole-char (aref (tw-grid *world*) (1- row) col))
	 (not (eql (aref (tw-grid-which-hole *world*) (1- row) col)
		   hole))
	 (draw-line *world-window* *world-gc* 
		    (* col *cell-width*) (* row *cell-height*)
		    (1- (* (1+ col) *cell-width*)) (* row *cell-height*)))
    (and (eql bare-hole-char (aref (tw-grid *world*) (1+ row) col))
	 (not (eql (aref (tw-grid-which-hole *world*) (1+ row) col)
		   hole))
	 (draw-line *world-window* *world-gc* 
		    (* col *cell-width*) (* (1+ row) *cell-height*)
		    (1- (* (1+ col) *cell-width*)) (* (1+ row) *cell-height*)))
    ))


(defun xtw-world-cell-label (row col label)
  (draw-glyphs *world-window* *label-gc*
	       (+ (* col *cell-width*) *label-hborder*)
	       (+ (* row *cell-height*)
		  *label-vborder*
		  *label-baseline*)
	       label))

(defun xtw-make-arrow-gc ()
  (setf *arrow-gc*
	(create-gcontext
	 :drawable *world-window*
	 :cap-style :round
	 :foreground *black*
	 :join-style :round
	 :line-width (round (* .1 *square-size*))
	 )))

(defun xtw-prepare-pixmaps ()

  (xtw-make-arrow-gc)

  (dolist (x '(*hole-pm* 
	       *nothing-pm* 
	       *obstacle-pm* 
	       *tile-pm* 
	       *agent-pm* 
	       *agent-up-pm* 
	       *agent-down-pm* 
	       *agent-left-pm* 
	       *agent-right-pm*))
	   (set x (xtw-create-pixmap)))
  
  (setf 
   *world-pixmaps* 
   `((,bare-hole-char . ,*hole-pm*)
     (,nothing-char . ,*nothing-pm*)
     (,obstacle-char . ,*obstacle-pm*)
     (,bare-tile-char . ,*tile-pm*)
     (#\a . ,*agent-pm*)
     (#\^ . ,*agent-up-pm*)
     (#\v . ,*agent-down-pm*)
     (#\< . ,*agent-left-pm*)
     (#\> . ,*agent-right-pm*)
     ))
  (dolist (pm (list *agent-pm* *agent-up-pm* *agent-down-pm*
		    *agent-left-pm* *agent-right-pm*
		    *nothing-pm* *obstacle-pm* *tile-pm*))
	  (xtw-fill-pixmap pm *grey50-gc*))
  (xtw-fill-pixmap *hole-pm* *black-gc*)
  (xtw-draw-square-bump *hole-pm* 1 (/ (- 1 *hole-inner-size*) 2) *grey50-gc* *black-gc* 
			*grey25-gc*)
  (xtw-draw-square-bump *obstacle-pm* .92 .1 *grey50-gc* *white-gc*
			*grey75-gc*)
  (xtw-draw-square-bump *tile-pm* .7 .1 *grey25-gc* *grey75-gc* 
			*grey50-gc*)

  (xtw-draw-still-agent *agent-pm*)
  (xtw-draw-still-agent *agent-up-pm*)
  (xtw-draw-still-agent *agent-down-pm*)
  (xtw-draw-still-agent *agent-left-pm*)
  (xtw-draw-still-agent *agent-right-pm*)

  (xtw-draw-arrow *agent-up-pm* t -1)
  (xtw-draw-arrow *agent-down-pm* t 1)
  (xtw-draw-arrow *agent-left-pm* nil -1)
  (xtw-draw-arrow *agent-right-pm* nil 1)

  ;(xtw-draw-moving-agent *agent-up-pm* 90)
  ;(xtw-draw-moving-agent *agent-down-pm* 270)
  ;(xtw-draw-moving-agent *agent-left-pm* 180)
  ;(xtw-draw-moving-agent *agent-right-pm* 0)
  
  (xtw-draw-grid-lines)
  )

(defun xtw-draw-arrow (pm vertical-p sign)
  (let* ((size .8)
	 (spread .5)
	 (halfsize (* (/ size 2) *square-size*))
	 (halfspread (* (/ spread 2) *square-size*))
	 (middle (/ *square-size* 2))
	 (x1 (round (- middle (* sign halfsize))))
	 (y1 (round (- middle halfspread)))
	 (x2 (round (+ middle (* sign halfsize))))
	 (y2 (round middle))
	 (x3 (round (- middle (* sign halfsize))))
	 (y3 (round (+ middle halfspread))))
    (draw-lines pm *arrow-gc*
		(if vertical-p
		    (list y1 x1 y2 x2 y3 x3)
		  (list x1 y1 x2 y2 x3 y3)))))

(defun xtw-draw-still-agent (pm)
  (xtw-draw-filled-arc pm .7 0 360 *white-gc*)
  (xtw-draw-filled-arc pm .5 0 360 *grey75-gc*)
  )

(defun xtw-draw-moving-agent (pixmap angle)
  (xtw-draw-filled-arc pixmap .7 0 360 *white-gc*)
  (xtw-draw-filled-arc pixmap .7 (- angle 50) 20 *black-gc*)
  (xtw-draw-filled-arc pixmap .7 (+ angle 30) 20 *black-gc*)
  (xtw-draw-filled-arc pixmap .5 0 360 *grey75-gc*)
  (xtw-draw-filled-arc pixmap .8 (+ angle 150) 10 *black-gc*)
  (xtw-draw-filled-arc pixmap .8 (+ angle 200) 10 *black-gc*)
)

(defun xtw-draw-filled-arc (pixmap radius start-angle size-angle gc)
  (let* ((radius (* radius (/ *square-size* 2)))
	 (start (round (- (/ *square-size* 2) radius)))
	 (size (round (* 2 radius))))
    (draw-arc pixmap gc start start size size (deg-to-rad start-angle)
	      (deg-to-rad size-angle) t)))

(defun deg-to-rad (d)
  (* (/ 
      (cond ((<= d 360) d)
	    ((- d 360)))
      180) pi))

(defun xtw-draw-round-bump (pixmap outer-size border dark-gc light-gc top-gc)
  (let* ((elbow-room (/ (- 1 outer-size) 2))
	 (inner-size (- outer-size (* 2 border)))
	 (outside-start (round (* elbow-room *square-size*)))
	 (inside-start (round (* (+ elbow-room border) *square-size*)))
	 (outside-size (round (* outer-size *square-size*)))
	 (inside-size (round (* inner-size *square-size*))))
    (draw-arc pixmap dark-gc
	      outside-start outside-start
	      outside-size outside-size
	      (deg-to-rad 225) (deg-to-rad 180)
	      t)
    (draw-arc pixmap light-gc
	      outside-start outside-start
	      outside-size outside-size
	      (deg-to-rad 45) (deg-to-rad 180)
	      t)
    (draw-arc pixmap top-gc
	      inside-start inside-start
	      inside-size inside-size
	      0 (deg-to-rad 360)
	      t)))

(defun xtw-draw-square-bump (pixmap outer-size border dark-gc light-gc top-gc)
  (let* ((elbow-room (/ (- 1 outer-size) 2))
	 (inner-size (- outer-size (* 2 border)))
	 (outside-start (round (* elbow-room *square-size*)))
	 (inside-start (round (* (+ elbow-room border) *square-size*)))
	 (outside-size (round (* outer-size *square-size*)))
	 (inside-size (round (* inner-size *square-size*))))
    (draw-rectangle pixmap dark-gc
		    outside-start outside-start
		    outside-size outside-size
		    t)
    (dotimes (i outside-size)
	     (draw-line pixmap light-gc
			(+ i outside-start) outside-start
			0 (- outside-size (1+ i))
			t))
    (draw-rectangle pixmap top-gc
		    inside-start inside-start
		    inside-size inside-size
		    t)))

(defun xtw-draw-grid-lines ()
  (mapc #'(lambda (pixmap)
	    (draw-lines pixmap *black-gc*
			`(,(1- *square-size*) 0 0 0 0 ,(1- *square-size*))))
	(list *nothing-pm* *obstacle-pm* *tile-pm*
	      *agent-pm* *agent-up-pm* *agent-down-pm*
	      *agent-left-pm* *agent-right-pm*)))

(defun xtw-create-pixmap ()
  (create-pixmap 
   :width *square-size* 
   :height *square-size* 
   :depth (drawable-depth *dummy-window*)
   :drawable *world-window*))

(defun xtw-fill-pixmap (pixmap gcontext)
  (draw-rectangle pixmap gcontext 0 0 *square-size* *square-size* t))

;;; Hole stat display.

(defun xtw-world-button-press-handler (&rest event-slots &key x y
					     &allow-other-keys)
  (declare (ignore event-slots))
  (cond ((setf *object* (find-hole-displayed-at x y))
	 (if *object-window-mapped*
	     (objwin-expose :count 0)
	   (progn (map-window *object-window*)
		  (setf *object-window-mapped* t))))
	(t
	 (unmap-window *object-window*)
	 (setf *object-window-mapped* nil))))

(defun objwin-expose (&rest event-slots &key count &allow-other-keys)
  (declare (ignore event-slots))
  (clear-area *object-window*
	      :x 0 :y 0
	      :width *object-window-width*
	      :height *object-window-height*)
  (when (zerop count)
	(if *object* (tw-show-obj 0 0 *object*)
	  (tw-print 0 0 "No object."))))

(defun objwin-button (&rest event-slots)
  (declare (ignore event-slots))
  (unmap-window *object-window*)
  (setf *object-window-mapped* nil))

(defun find-hole-displayed-at (pixelx pixely)
  (let ((gridx (truncate pixelx *cell-width*))
	(gridy (truncate pixely *cell-height*))
	(holes (tw-hole-list *world*))
	(result-hole nil))
    (dolist (hole holes)
	    (dolist (cell (obj-cells hole))
		    (if (and (eql (cell-col cell) gridx)
			     (eql (cell-row cell) gridy))
			(setf result-hole hole))))
    result-hole))
	    
;;; This is taken straight out of the old twdisp.l.  tw-print is changed.
;;; tw-show-obj modified a bit to handle objects with no cells.
(defun tw-show-obj (row col obj)
  (let (j)
    (tw-print row col (format nil "~15A" (obj-name obj)))
    (tw-print (+ row 1) col
	      (if (obj-cells obj)
		  (format nil "Row ~2A Col ~4A" 
			  (cell-row (first (obj-cells obj)))
			  (cell-col (first (obj-cells obj))))
		(format nil "No more cells.")))
    (setf j 2)
    (when (not (null (obj-timeout obj)))
	  (tw-print (+ row j) col
		    (format nil "Out ~11A" (obj-timeout obj)))
	  (setf j (+ j 1)))
    (when (eq (obj-type obj) 'hole)
	  (tw-print (+ row j) col
		    (format nil "Sco ~11A" (obj-score obj)))
	  (setf j (+ j 1)))
    (tw-print (+ row j) col (format nil "~15A" ""))
    (+ j 1)))

(defun tw-print (row col str)
  (draw-glyphs *object-window* *gcontext*
	       (+ (* col *char-width*) *objwin-border*)
	       (+ (* row *char-height*) *objwin-border*
		  *font-baseline*)
	       str))
