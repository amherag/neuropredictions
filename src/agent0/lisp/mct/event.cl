

;;;  EVENT.CL
;;;
;;;  Main event loop handler for MCT, Mark's CLX Toolkit


(require :clx)

 
(defvar *display* nil)			; display
(defvar *screen*)			; screen
(defvar *root*)				; root window of screen
(defvar *colormap*)
(defvar *white*)
(defvar *black*)
(defvar *font*)
(defparameter *fontname* "*new century schoolbook-*-r-*-14*") ; was 9x15
(defparameter *panefontname* "*lucidatypewriter-medium-*-11-*")
(defparameter *italicfontname* "*times-*-i-*-20-*")
(defparameter *smallfontname* "*lucidatypewriter-medium-*-11-*")
(defvar *font*)
(defvar *pane-font*)
(defvar *italic-font*)
(defvar *small-font*)
(defvar *font-baseline*)
(defvar *pane-font-baseline*)
(defvar *font-descent*)
(defvar *pane-font-descent*)
(defvar *font-height*)
(defvar *pane-font-height*)
(defvar *char-width*)
(defvar *char-height*)
(defvar *dummy-window*)
(defvar *gcontext*)
(defvar *black-gc*)
(defvar *white-gc*)
(defvar *grey25-gc*)
(defvar *grey50-gc*)
(defvar *grey75-gc*)
(defvar *dialog-list*)
(defvar *busy*)
(defvar *handlers*)
(defvar *display-on*)


(defun mct-open-display ()
  (let ((a (system:getenv "DISPLAY")) (b (system:getenv "HOST")))
    (if a (xlib:open-display (subseq a 0 (position #\: a)))
          (xlib:open-display (subseq b 0 (position #\. b))))))

(defun mct-init-xwindows ()
  (setq *display* (mct-open-display))
  (setf (xlib:display-after-function *display*) #'xlib:display-finish-output)
  (setq *screen* (xlib:display-default-screen *display*))
  (check-color)
  (setq *root* (xlib:screen-root *screen*))
  (setq *colormap* (xlib:screen-default-colormap *screen*))
  (setq *black* (xlib:screen-black-pixel *screen*))
  (setq *white* (xlib:screen-white-pixel *screen*))
  (setq *font* (xlib:open-font *display* *fontname*))
  (setq *italic-font* (xlib:open-font *display* *italicfontname*))
  (setq *small-font* (xlib:open-font *display* *smallfontname*))
  (setq *pane-font* (xlib:open-font *display* *panefontname*))
  (setq *font-baseline* (xlib:max-char-ascent *font*))
  (setq *pane-font-baseline* (xlib:max-char-ascent *pane-font*))
  (setq *font-descent* (xlib:max-char-descent *font*))
  (setq *pane-font-descent* (xlib:max-char-descent *pane-font*))
  (setq *font-height* (+ *font-baseline* *font-descent*))
  (setq *pane-font-height* (+ *pane-font-baseline* *pane-font-descent*))
  (setq *char-width* (xlib:text-width *font* "O"))
  (setq *char-height* *font-height*)
  (setq *dummy-window* (mct-default-window 10 10))
  (setq *gcontext* (mct-gcontext :invid-p t))
  (setq *black-gc* (mct-black-gc))
  (setq *white-gc* (mct-white-gc))
  (mct-init-gcontexts)
  (setq *dialog-list* nil)
  (setq *busy* nil)
  (setq *handlers* nil)
  (setq *display-on* t)
  )
  
(defun mct-init-gcontexts ()
  (if *color*
      (progn
	(setq *grey25* (mct-alloc-color "grey35"))
	(setq *grey50* (mct-alloc-color "grey55"))
	(setq *grey60* (mct-alloc-color "grey65"))
	(setq *grey75* (mct-alloc-color "grey80"))
	(setq *grey25-gc* (mct-gcontext :foreground *grey25*))
	(setq *grey50-gc* (mct-gcontext :foreground *grey50*))
	(setq *grey60-gc* (mct-gcontext :foreground *grey60*))
	(setq *grey75-gc* (mct-gcontext :foreground *grey75*)))
    (progn
      (setq *text-gc* *white-gc*)
      (setq *grey25-gc* (mct-grey25-gc))
      (setq *grey50-gc* *black-gc*)	; so we can read the labels
      (setq *grey60-gc* *black-gc*)	; so we can read the buttons
      (setq *grey75-gc* (mct-grey75-gc)))))

(defvar *mct-colors* nil)

(defun mct-alloc-color (color)
  (let ((color (xlib:alloc-color *colormap* color)))
    (push color *mct-colors*)
    color))

(defun mct-free-all-colors ()
  (xlib:free-colors *colormap* *mct-colors*)
  (setf *mct-colors* nil))

(defun mct-gcontext (&key (invid-p nil)
			  (font *font*)
			  (foreground *black*)
			  (background *white*))
  (xlib:create-gcontext :drawable *dummy-window*
			:background (if invid-p foreground background)
			:foreground (if invid-p background foreground)
			:font font))

(defun mct-default-window (width height &optional
				  (x (screen-center width t))
				  (y (screen-center height nil)))
  (xlib:create-window
   :parent *root*
   :x x
   :y y
   :width width :height height
   :background *black*
   :border *white*
   :border-width 2
   :colormap *colormap*
   :event-mask '(:exposure :button-press)))

(defun mct-set-wm-props (window long-name short-name)
  (xlib:set-wm-properties window
			  :name long-name
			  :icon-name short-name
			  :resource-name short-name
			  :resource-class "AGENT0"
			  :command '(aop)
			  :x (xlib:drawable-x window)
			  :y (xlib:drawable-y window)
			  :width (xlib:drawable-width window)
			  :height (xlib:drawable-height window)
			  :min-width (xlib:drawable-width window)
			  :min-height (xlib:drawable-height window)
			  :max-width (xlib:drawable-width window)
			  :max-height (xlib:drawable-height window)
			  :input :off
			  :initial-state :normal))


;;;
;;; mct-event-loop
;;;
;;; Borrowed shamelessly from Tileworld by Michael P. Frank
;;;
;;; A generalized event-loop that allows work (e.g. the simulation)
;;; to be done in the background, or not, determined dynamically.  
;;; Handlers for events may be added or removed dynamically.
;;;
;;; Arguments: 
;;;
;;;      work-form, an expression to be evaluated each cycle
;;;                 whenever the *busy* dynamic variable is true.
;;;                 It should be quick enough to allow good user-
;;;                 interface response time.
;;;
;;; Dynamic variables:
;;;
;;;      *busy* - If set (non-nil) by a handler-function, means there is
;;;               work to do, and the event loop will alternate between 
;;;               executing work-form and polling for events.  If cleared 
;;;               (nil) by the work-form or handler-functions, it's 
;;;               assumed there is no work to do, and the event loop will 
;;;               block until there is an event, and work-form will not 
;;;               be executed at all.
;;;
;;;      *quit* - When set, the event loop will quit, and the current
;;;               *handler-functions* binding will be lost.  Bound to
;;;               nil upon entering event loop.
;;;
;;;      *handlers* - a list of functions, each with the arg list:
;;;               (&rest event-slots &key <vars>* &allow-other-keys)
;;;               where the vars are the event slots they're interested in.
;;;               Note that every function will be called on every event.
;;;               The functions' return values are ignored.
;;;               *handlers* may be changed dynamically by the handler
;;;               functions or the work-form.  Of course, the change will
;;;               not take effect until the next event needs to be handled.
;;;               For safety, don't change this list destructively.
;;;
;;;      *event-level* - counts the number of embedded levels of
;;;               xtw-event-loop that are currently active, so that some
;;;               handlers can restrict their response to top-level
;;;               events by checking to see if *event-level* is 1.
;;;               other handlers may work more often, at lower levels.
;;;

(defun mct-event-loop (work-form &optional (new-event-level *event-level*))
  (let ((old-event-level *event-level*))
    (setq *event-level* new-event-level)
    (do ((uic 0 (1+ uic)))
	((= *quit* *event-level*)
	 ;;(format t "There were ~s user-interface cycles.~&" uic)
	 (setq *event-level* old-event-level)
	 ;;(format t "Exiting mct-event-loop~%")
	 (if (= *event-level* 1) (mct-close-all))
	 )
	(xlib:process-event 
	 *display* 
	 :timeout (if *busy* 0 0)	; (if *busy* 0 nil)
	 :discard-p t
	 :force-output-p t
	 :handler #'(lambda (&rest event-slots)
		      ;;(format t "Event detected!~&")
		      (mapc #'(lambda (func)
				;;(format t "Handler.~&")
				(apply func event-slots))
			    *handlers*)
		      t))
	(when *busy*
	      ;;(format t "Busily doing ~s.~&" work-form)
	      (eval work-form)))))


(defun mct-add-handler (handler)
  (setf *handlers* (adjoin handler *handlers*))) ;Ensures uniqueness

(defun mct-remove-handler (handler)
  (setf *handlers* (remove handler *handlers*))) ;Non-destructive

(defun screen-center (size x-p)
  (truncate (- (if x-p (xlib:screen-width *screen*)
	 (xlib:screen-height *screen*))
	       size) 2))


;;;
;;; handle-exposure-events
;;;
;;; Calls all the current handlers, if there's an exposure event
;;; waiting.  If there's not, just return.
;;;

(defun handle-exposure-events ()
  (do ()
      ((not (xlib:process-event 
	     *display* :timeout 0 :discard-p nil
	     :force-output-p nil
	     :handler #'(lambda (&rest event-slots &key event-key
				       &allow-other-keys)
			  (when (eq event-key :exposure)
				(mapc #'(lambda (func)
					  (apply func 
						 event-slots))
				      *handlers*)
				t)))))))


;;;
;;; Functions to make some graphics contexts for drawing shaded areas
;;;

(defun mct-black-gc ()
  (xlib:create-gcontext
   :drawable *dummy-window*
   :background *white*
   :foreground *black*
   ))

(defun mct-white-gc ()
  (xlib:create-gcontext
   :drawable *dummy-window*
   :background *black*
   :foreground *white*
   ))

(defun mct-make-grey-gc (func)
  (let ((pm (xlib:create-pixmap :width 2 :height 2 :depth (xlib:drawable-depth
							   *dummy-window*)
				:drawable *dummy-window*)))
    (funcall func pm)
    (xlib:create-gcontext
     :drawable *dummy-window*
     :background *black*
     :foreground *white*
     :fill-style :tiled
     :tile pm)))
  
(defun mct-grey25-gc ()
  (mct-make-grey-gc
   #'(lambda (pm)
       (xlib:draw-rectangle pm *black-gc* 0 0 2 2 t)
       (xlib:draw-point pm *white-gc* 0 0))))

(defun mct-grey50-gc ()
  (mct-make-grey-gc
   #'(lambda (pm)
       (xlib:draw-rectangle pm *black-gc* 0 0 2 2 t)
       (xlib:draw-point pm *white-gc* 0 0)
       (xlib:draw-point pm *white-gc* 1 1))))

(defun mct-grey75-gc ()
  (mct-make-grey-gc
   #'(lambda (pm)
       (xlib:draw-rectangle pm *white-gc* 0 0 2 2 t)
       (xlib:draw-point pm *black-gc* 0 0))))


;;;  MCT-CLOSE-ALL
;;;
;;;  Closes all open windows

(defun mct-close-all ()
  (if *mct-colors*
      (mct-free-all-colors))
  (if *display*
      (xlib:close-display *display* :abort t))
  (setf *wi-list* nil))


(defun le () (load "event"))

(defun li () (load "widgets"))

(defun lw () (load "window"))

(defun lt () (load "test"))

(defvar *color* 'unknown)

(defun check-color ()
  (when (eq *color* 'unknown)
	(let ((info (xlib:screen-root-visual-info *screen*)))
	  (setq *color* (= (xlib:visual-info-bits-per-rgb info) 8)))))


(defun mct-events ()
  (setq *event-level* 1)
  (setq *quit* 0)
  (mct-event-loop nil))

(defun go ()
  (mct-close-all)
  (mct-init-xwindows)
  (test)
  (setq *event-level* 1)
  (setq *quit* 0)
  (mct-event-loop nil))

(defun go2 ()
  (close-all)
  (mct-init-xwindows)
  (test2)
  (setq *event-level* 1)
  (setq *quit* 0)
  (mct-event-loop nil))


;;;  LA -- LOAD ALL
  
(defun la ()
  (le)
  (li)
  (lw)
  (lt))
