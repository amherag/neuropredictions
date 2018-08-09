;;;  WORLD.LISP
;;;  George John
;;;  gjohn@cs.stanford.edu
;;;
;;;  The World Agent
;;;
;;;  In a real demo of this system we might have actual aop-piloted
;;;  planes flying around a field.  That not being the case, we 
;;;  settle for a demo on the computer's screen.  Using the AOP
;;;  model, we consider an agent's moving in a particular direction
;;;  to be sending a message to the world, telling it that the agent
;;;  is now at the new location.  We consider an agent's surveying
;;;  its environment to be requesting the world to inform it of
;;;  any other agents in its area.

(defagent world
    :timegrain 10			; in seconds (not used yet)
    :beliefs '((0 (ok)))
    :commit-rules
        '(    

	  ;; If I am informed of a plane's whereabouts, then
	  ;; I store the information about the plane
          ((?agent inform (?time (plane ?name ?x ?y)))
           ()
           ?agent
           (do ?time (add-plane '?name ?x ?y)))

          ;; Redraw the world
          (()
           ()
           world
           (do now (redraw-world)))


	  ;; If I am requested to provide radar data for a 
	  ;; plane at some time, then I commit to that agent
	  ;; to provide radar data to it at that time
	  ((?agent request (do ?time (radar ?name)))
	   ()
	   ?agent
	   (do ?time (radar ?agent ?name)))
	  )
)


;;;  PRIVATE ACTIONS
;;;
;;;  Data Structures: This is an odd thing to speak of in
;;;  AOP -- normally an agent will not have data structures
;;;  such as these within its private actions.  However, 
;;;  since !? variables have not been implemented, this is 
;;;  necessary to be able to substitute lisp code that handles
;;;  "forall".
;;;
;;;     *planes* = list of (name x y heading)
;;;     *copters* = list of (name x y heading bladeangle)
;;;     *airfields* = list 0f (name x y)
;;;     *towers* = list of (name x y)
;;;
;;;  Private actions:
;;;  The following functions are used as private actions by the
;;;  world agent.  The rest of the functions are not considered
;;;  private actions, but are merely support functions for
;;;  manipulating the data structures and for graphics.
;;;

;;; Determine what type of machine aop is running on 


;;; ADD-PLANE : given the name of a plane, its x and y coordinates
;;; and its heading, store the plane in the database *planes* and
;;; draw the plane on the screen.  If there was an earlier entry
;;; in *planes* with the same name then use the old x,y and new
;;; x,y to recalc the heading, then remove the old entry
(defun add-plane (name x y)
  (let ((oldplane (assoc name *planes*))
	(newheading 0))
    (if oldplane
	(let ((dx (- x (second oldplane)))
	      (dy (- y (third oldplane))))
	  (setq dy (if (equal dy 0) .000001 dy))
	  (setq newheading (let ((newheading (atan (/ dx dy))))
			     (if (> dy 0) 
				 (+ pi newheading)
			       newheading)))))
    (setq *planes* (cons (list name x y newheading) 
			 (delete-if #'(lambda (x) 
					(equal (car x) name)) 
				    *planes*)))
    (if oldplane (draw-plane oldplane :erase t :show-name t))
    ;; (draw-plane (list name x y newheading) :show-name t)
    ))


;;; RADAR : given the name of an agent and the name of its plane,
;;; look through the list of planes and copters and inform the agent
;;; as to the whereabouts of all friends and enemies within its radar
(defun radar (agent name)
  (let ((plane (assoc name *planes*))
	(x (second plane))
	(y (third plane)))
    (dolist (plane *planes*)
	    (let ((px (second plane))
		  (py (third plane))
		  (pname (first plane)))
	      (if (and (not (equal pname name)) 
                       (< (distance (- px x) (- py y)) *radar-radius*))
		  (inform agent (list 'close-friend px py) (now)))))
    (dolist (copter *copters*)
	    (let ((cx (second copter))
		  (cy (third copter))
		  (cname (first copter)))
	      (if (< (distance (- cx x) (- cy y)))
                (progn (format "~%CloseGuy at ~d ~d" cx cy)
		  (inform agent (list 'close-enemy cx cy) (now)))
                )))))


;;; SHOW-WORLD : draws the world to the screen.  Necessary to 
;;; repeat every timestep for the macintosh
(defun redraw-world ()
  (dolist (plane *planes*)
    (draw-plane plane :show-name t))
  (dolist (copter *copters*)
    (draw-helicopter copter :show-name t))
  (dolist (tower *towers*)
    (draw-tower tower))
  (dolist (airfield *airfields*)
    (draw-airfield airfield)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                        XWINDOWS FUNCTIONS                           ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(not :ccl)
(progn

;;; load the clx package
(require :clx)
(require :process)

;;; Variables for XWindows
(defvar *d*)                    ;; display
(defvar *s*)                    ;; screen
(defvar *w*)                    ;; window
(defvar *white*)		;; graphics context 
(defvar *black*)	        ;; graphics context
(defvar *f1*)                   ;; fonts

;;; Open the display
(defun my-open-display ()
  (let ((a (system:getenv "DISPLAY")) (b (system:getenv "HOST")))
    (if a (xlib:open-display (subseq a 0 (position #\: a)))
          (xlib:open-display (subseq b 0 (position #\. b))))))


;;; INIT-XWINDOWS : Opens the display, creates graphic contexts for
;;; drawing the objects on the screen
(defun init ()
  (setq *d* (my-open-display))
  (setf (xlib:display-after-function *d*) #'xlib:display-finish-output)
  (setq *s* (xlib:display-default-screen *d*))
  (setq *f1* (xlib:open-font *d* "9x15"))          ;;font 
  (setq *plane* (xlib:create-gcontext :drawable (xlib:screen-root *s*)
				      ;; context to draw planes
				      :foreground 0 :background 1 :font *f1*
				      :line-width 3))
  (setq *plane-erase*
	(xlib:create-gcontext :drawable (xlib:screen-root *s*)
			      ;; context to erase planes
			      :foreground 1 :background 0 :font *f1*
			      :line-width 3))
  (setq *blade* (xlib:create-gcontext :drawable (xlib:screen-root *s*)
				      ;; context to draw copter blades
				      :foreground 0 :background 1 :font *f1*
				      :line-width 2))
  (setq *blade-erase*
	(xlib:create-gcontext :drawable (xlib:screen-root *s*)
			      ;; context to erase copter blades
			      :foreground 1 :background 0 :font *f1*
			      :line-width 2))
  (setq *white* (xlib:create-gcontext :drawable (xlib:screen-root *s*)
				      :foreground 0 :background 1 :font *f1*))
  (setq *black* (xlib:create-gcontext :drawable (xlib:screen-root *s*)
				      :foreground 1 :background 0 :font *f1*)))


;;; OPEN-WINDOW : given (x,y) as upper left corner and width,height of
;;; window, opens a window of the requested dimensions
(defun open-window (x y width height)
  (setq *w* (xlib:create-window :parent (xlib:screen-root *s*)
				:x x :y y :width width :height height 
				:background 1
				:bit-gravity :north-west
				:backing-store :always)) 
  (xlib:map-window *w*))


;;; END : called at the end of the program to close the display
(defun end ()
  (xlib:close-display *d*))

(defun draw-lines (&rest x) (apply #'xlib:draw-lines x))
(defun draw-glyphs (&rest x) (apply #'xlib:draw-glyphs x))

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                        MACINTOSH GRAPHICS                           ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+:ccl
(progn

(require 'quickdraw)

(defvar *w*)
(setq *plane* 'blackColor)
(setq *plane-erase* 'whiteColor)
(setq *blade* 'blackColor)
(setq *blade-erase* 'whiteColor)
(setq *white* 'blackColor)
(setq *black* 'whiteColor)

(defun init() )

;;; OPEN-WINDOW : given (x,y) as upper left corner and width,height of
;;; window, opens a window of the requested dimensions
(defun open-window (x y width height)
  (cond ((or (not (boundp '*w*)) (null (ask *w* (ownp 'wptr))))
         (setq *w* (oneof *window*  
                             :window-title "Agent0 Plane Demo"
                             :window-position (make-point x y)
                             :window-size (make-point width height)
                             ))))
  (ask *w* (erase-rect 0 0 1000 1000))
  (ask *w* (window-select))
  (ask *w* (set-window-font '("Monaco" 9)))
  )


;;; WRITING XWINDOWS FUNCTIONS IN QUICKDRAW
(defun draw-lines (wind context points 
                        &key 
                        (relative-p nil)
                        (fill-p nil))
  (case context
    ('blackcolor (ask wind (set-pen-mode :patcopy)))
    ('whitecolor (ask wind (set-pen-mode :notpatcopy))))
  (ask wind (start-polygon))
  (ask wind (move-to (first points) (second points))) 
  (dotimes (i (/ (length (cddr points)) 2))
    (setq points (cddr points))
    (let ((x (first points))
          (y (second points)))
      (case relative-p
        (nil (ask wind (line-to x y)))
        (t (ask wind (line x y))))))
  (let ((p (ask wind (get-polygon))))
    (case fill-p
      (nil (ask wind (frame-polygon p)))
      (t (ask wind (paint-polygon p))))  
    (kill-polygon p))
  (ask wind (set-pen-mode :patcopy)))
         

(defun draw-glyphs (wind context x y str)
  (case context
    ('blackcolor
     (ask wind (move-to x y))
     (princ str wind))
    ('whitecolor
     (ask wind (erase-rect x (- y 7) (+ x 10) y)))))

)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                        AIRPLANE GRAPHICS                            ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; DRAW-TOWER : given a tower, draws it on the screen
(defun draw-tower (tower)
  (let ((name (first tower)) (x (second tower)) (y (third tower)))
    (draw-lines *w* *white* `(,x ,y 0 -6 6 0 0 6 12 0 0 -8 -8 0 0 -4 -14 0 0 4 -8 0 0 8 -5 -24 -5 24 5 -8 5 8 12 0)
		     :relative-p t :fill-p t)
    (draw-glyphs *w* *white* (- x 10) (+ y 12) (symbol-name name))))


;;; DRAW-AIRFIELD : given an airfield, draw it on the screen
(defun draw-airfield (airfield)
  (let ((name (first airfield)) (x (second airfield)) (y (third airfield)))
    (draw-lines *w* *white* `(,x ,y 8 8 -3 3 -8 -8 -8 8 -3 -3 8 -8 -2 -2 -8 8 -3 -3 8 -8 -8 -8 3 -3 8 8 8 -8 3 3 -8 8 2 2 8 -8 3 3 -8 8)
		     :relative-p t :fill-p t)
    (draw-glyphs *w* *white* (- x 7) (+ y 24) (symbol-name name))))


;;; DRAW-PLANE : given a plane, if erase then erase it from the
;;; screen, if not erase then draw it on the screen.  If show-name
;;; then draw the name of the plane next to it on screen.
(defun draw-plane (plane &key (show-name nil) (erase nil))
  (let* ((name (first plane)) (x (second plane)) (y (third plane))
	 (theta (fourth plane))  
	 (s (sin theta)) (c (cos theta))
	 (pls (* plane-length s)) (plc (* plane-length c))
	 (pws (* plane-width s)) (pwc (* plane-width c))
	 (x1 (- x pls)) (y1 (- y plc)) (x2 (+ x pls)) (y2 (+ y plc))
	 (xc (- x (* plane-semi s))) (yc (- y (* plane-semi c)))
	 (x3 (- x pwc)) (y3 (+ y pws)) (x4 (+ x pwc)) (y4 (- y pws)))
    (if erase
	(progn
	  (draw-lines *w* *plane-erase* (list (floor x1) (floor y1)
					     (floor x2) (floor y2)))
	  (draw-lines *w* *plane-erase* (list (floor x3) (floor y3)
					     (floor xc) (floor yc)
					     (floor x4) (floor y4)))
	  (when show-name
		(draw-glyphs *w* *plane-erase* (floor (- x 7))
				  (floor (+ y (* 3 plane-length)))
				  (symbol-name name))))
      (progn
	(draw-lines *w* *plane* (list (floor x1) (floor y1)
					   (floor x2) (floor y2)))
	(draw-lines *w* *plane* (list (floor x3) (floor y3)
					   (floor xc) (floor yc)
					   (floor x4) (floor y4)))
	(when show-name
	      (draw-glyphs *w* *plane* (floor (- x 7))
				(floor (+ y (* 3 plane-length)))
				(symbol-name name)))))))	     


;;; DRAW-HELICOPTER : same as draw-plane
(defun draw-helicopter (copter &key (show-name nil) (erase nil))
  (let* ((name (first copter)) (x (second copter)) (y (third copter))
	 (theta (fourth copter)) (blades (fifth copter))
	 (s (sin theta)) (c (cos theta)) (sb (sin blades)) (cb (cos blades))
	 (cls (* copter-length s)) (clc (* copter-length c))
	 (blsb (* blade-length sb)) (blcb (* blade-length cb))
	 (xc (- x (* copter-semi s))) (yc (- y (* copter-semi c))))
    (if erase
	(progn
	  (draw-lines
	   *w* *plane-erase* (list (floor (- x cls)) (floor (- y clc))
				   (floor (+ x cls)) (floor (+ y clc))))
	  (draw-lines
	   *w* *blade-erase* (list (floor (- xc blsb)) (floor (- yc blcb))
				   (floor (+ xc blsb)) (floor (+ yc blcb))))
	  (draw-lines
	   *w* *blade-erase* (list (floor (- xc blcb)) (floor (+ yc blsb))
				   (floor (+ xc blcb)) (floor (- yc blsb))))
	  (when show-name
		(draw-glyphs *w* *plane-erase* (floor (- x 7))
				  (floor (+ y (* 3 plane-length)))
				  (symbol-name name))))
      (progn
	(draw-lines
	 *w* *plane* (list (floor (- x cls)) (floor (- y clc))
			   (floor (+ x cls)) (floor (+ y clc))))
	(draw-lines
	 *w* *blade* (list (floor (- xc blsb)) (floor (- yc blcb))
			   (floor (+ xc blsb)) (floor (+ yc blcb))))
	(draw-lines
	 *w* *blade* (list (floor (- xc blcb)) (floor (+ yc blsb))
			   (floor (+ xc blcb)) (floor (- yc blsb))))
	(when show-name
	      (draw-glyphs *w* *plane* (floor (- x 7))
				(floor (+ y (* 3 plane-length)))
				(symbol-name name)))))))
	

;;; REDRAW : erase planes from their old locations and draw them in
;;; their new locations.  erase all copters from their old locations
;;; and draw them in their new locations
(defun redraw (&key (show-names nil))
  (mapc #'(lambda (x y)
	      (draw-plane x :show-name show-names :erase t)
	      (draw-plane y :show-name show-names))
	  *old-planes* *planes*)
  (mapc #'(lambda (x y)
	      (draw-helicopter x :show-name show-names :erase t)
	      (draw-helicopter y :show-name show-names))
	  *old-copters* *copters*))


;;; RECOMPUTE : goes through all of the planes and helicopters and
;;; computes the new position of each given their velocity and heading.	  
(defun recompute ()
  (setq *old-planes* *planes* *old-copters* *copters*)
  (setq *planes* nil *copters* nil)
  (mapc #'(lambda (x) (push (update-plane x) *planes*)) *old-planes*)
  (mapc #'(lambda (x) (push (update-copter x) *copters*)) *old-copters*)
  (setq *planes* (reverse *planes*))
  (setq *copters* (reverse *copters*))
)


;;; UPDATE-PLANE : given a plane, compute its new position based on
;;; its heading and velocity
(defun update-plane (plane)
  (list (first plane)
	(- (second plane) (* *plane-rate* (sin (fourth plane))))
	(- (third plane) (* *plane-rate* (cos (fourth plane))))
	(fourth plane)))


;;; UPDATE-COPTER : given a copter, compute its new position based on
;;; its heading and velocity
(defun update-copter (copter)
  (list (first copter)
	(- (second copter) (* *copter-rate* (sin (fourth copter))))
	(- (third copter) (* *copter-rate* (cos (fourth copter))))
	(fourth copter)
	(mod (+ (fifth copter) *rotor-rate*) (* 2 3.14159))))


;;; DOLOOP : function used to debug the private actions.
(defun doloop ()
  (do ()
      ()
      (recompute)
      (redraw :show-names t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;               FUNCTIONS EVAL'd AS WORLD IS LOADED                   ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Data Structures
(setq *towers* '((T1 340 120)))
(setq *airfields* '((A1 40 70) (A2 290 60) (A3 500 110)))
(setq *planes* '((P1 200 100 .2) (P2 300 30 -2)))
(setq *copters* '((H1 200 115 1.4 2.0) (H2 360 50 4.5 2.1)))
(setq *radar-radius* 20)

;;; Constants
(setq plane-length 8 plane-width 6 plane-semi 3)
(setq *plane-rate* 2.0)
(setq *copter-rate* 2.30)
(setq *rotor-rate* 0.2)
(setq copter-length 8 blade-length 7 copter-semi 3)
(setq airfield-size 2)
(setq tower-size 2)

;;; Initialize
(defun begin () 
  (init)
  (open-window 1 40 400 180)
  (mapc #'(lambda (x) (draw-tower x)) *towers*)
  (mapc #'(lambda (x) (draw-airfield x)) *airfields*)
  (mapc #'(lambda (x) (draw-plane x :show-name t)) *planes*)
  (mapc #'(lambda (x) (draw-helicopter x :show-name t)) *copters*)
  nil)

(begin)

