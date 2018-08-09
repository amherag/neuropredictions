;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; ui.lisp
;;;;;
;;;;; General user-interface functions.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(in-package 'xtile)

;;
;; xtw-with-open-display
;;
;; Evaluates the given form, but with *display* open, 
;; and unwind-protected for closure upon exiting.
;;

(defun xtw-with-open-display (form)
  (let ((abort t)
	(*display* nil))
    (if (or (not (boundp '*hostname*))
	    (null *hostname*))
	(setf *hostname* (get-display-host)))
    (if (null *hostname*)
	(setf *hostname* "")
      )
    (unwind-protect
	(progn
	  (setf *display* (open-display *hostname*))
	  (format t ";;; Opened display on X host ~s.~&" *hostname*)
	  (eval form)
	  (setf abort nil))
      (when *display* 
	    (close-display *display* :abort abort)
	    (format t ";;; Closed display on ~s.~&" *hostname*)))))

(defun get-display-host ()
  (let* ((env_display (environment-variable "DISPLAY"))
	 (colon_pos (position #\: env_display)))
    (if colon_pos
	(subseq env_display 0 colon_pos)
      env_display
      )
    )
  )

;;
;; xtw-with-ui-defaults
;;
;; Evaluates the given form, but with various useful default values set
;; up for user-interface dynamic variables.  Must be called with 
;; *display* open.
;;

(defun xtw-with-ui-defaults (form)
  (let* ((*screen* (display-default-screen *display*))
	 (*root* (screen-root *screen*))
	 (*colormap* (screen-default-colormap *screen*))
	 (*black* (screen-black-pixel *screen*))
	 (*white* (screen-white-pixel *screen*))
	 (*font* (open-font *display* *fontname*))
	 (*font-baseline* (max-char-ascent *font*))
	 (*font-descent* (max-char-descent *font*))
	 (*font-height* (+ *font-baseline* *font-descent*))
	 (*char-width* (text-width *font* "O"))
	 (*char-height* *font-height*)
	 (*dummy-window* (xtw-default-window 10 10))
	 (*gcontext* (xtw-default-gcontext t))
	 (*black-gc* (xtw-black-gc))
	 (*white-gc* (xtw-white-gc))
	 (*grey25-gc* (xtw-grey25-gc))
	 (*grey50-gc* (xtw-grey50-gc))
	 (*grey75-gc* (xtw-grey75-gc))
	 (*dialog-list* nil)
	 (*busy* nil)
	 (*handlers* nil)
	 (*display-on* t)
	 )
    (eval form)))

(defun xtw-default-gcontext (&optional invid-p font)
  (create-gcontext :drawable *dummy-window*
		   :background (if invid-p *black* *white*)
		   :foreground (if invid-p *white* *black*)
		   :font (or font *font*)))


(defun xtw-default-window (width height)
  (create-window
   :parent *root*
   :x (screen-center width t)
   :y (screen-center height nil)
   :width width :height height
   :background *black*
   :border *white*
   :border-width 2
   :colormap *colormap*
   :event-mask '(:exposure :button-press)))

(defun xtw-set-wm-props (window long-name short-name)
  (set-wm-properties window
		     :name long-name
		     :icon-name short-name
		     :resource-name short-name
		     :resource-class "Tileworld"
		     :command '(tileworld)
		     :x (drawable-x window)
		     :y (drawable-y window)
		     :width (drawable-width window)
		     :height (drawable-height window)
		     :min-width (drawable-width window)
		     :min-height (drawable-height window)
		     :max-width (drawable-width window)
		     :max-height (drawable-height window)
		     :input :off
		     :initial-state :normal))

;;;
;;; xtw-event-loop       N E W   V E R S I O N
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

(defun xtw-event-loop (work-form)
  (let ((*quit* nil)
	(*event-level* (1+ *event-level*)))
    (do ((uic 0 (1+ uic)))
	(*quit*
	 ;(format t "There were ~s user-interface cycles.~&" uic)
	 )
	(process-event 
	 *display* 
	 :timeout (if *busy* 0 nil)
	 :discard-p t
	 :force-output-p t
	 :handler #'(lambda (&rest event-slots)
		      ;(format t "Event detected!~&")
		      (mapc #'(lambda (func)
				;(format t "Handler.~&")
				(apply func event-slots))
			    *handlers*)
		      t))
	(when *busy*
	      ;(format t "Busily doing ~s.~&" work-form)
	      (eval work-form)))))

(defun xtw-add-handler (handler)
  (setf *handlers* (adjoin handler *handlers*))) ;Ensures uniqueness

(defun xtw-remove-handler (handler)
  (setf *handlers* (remove handler *handlers*))) ;Non-destructive

(defun screen-center (size x-p)
  (truncate (- (if x-p (screen-width *screen*) (screen-height *screen*))
	       size) 2))

;;;
;;; handle-exposure-events
;;;
;;; Calls all the current handlers, if there's an exposure event
;;; waiting.  If there's not, just return.
;;;

(defun handle-exposure-events ()
  (do ()
      ((not (process-event *display* :timeout 0 :discard-p nil
			   :force-output-p nil
			   :handler #'(lambda (&rest event-slots &key event-key
						     &allow-other-keys)
					(when (eq event-key :exposure)
					      (mapc #'(lambda (func)
							(apply func 
							       event-slots))
						    *handlers*)
					      t)))))
      )
)

;;;
;;; xtw-ui-reset is to be called when conditions have changed
;;; (i.e., new world or agent parameters) and the user interface
;;; needs to react to the new conditions.
;;;

(defun xtw-ui-reset ()
  (xtw-exper-ui-new-param)
  (xtw-world-ui-new-param)
  (xtw-agent-ui-new-param)
)

;;;
;;; Functions to make some graphics contexts for drawing shaded areas
;;;

(defun xtw-black-gc ()
  (create-gcontext
   :drawable *dummy-window*
   :background *black*
   :foreground *black*
   ))

(defun xtw-white-gc ()
  (create-gcontext
   :drawable *dummy-window*
   :background *white*
   :foreground *white*
   ))

(defun xtw-make-grey-gc (func)
  (let ((pm (create-pixmap :width 2 :height 2 :depth (drawable-depth
						      *dummy-window*)
			   :drawable *dummy-window*)))
    (funcall func pm)
    (create-gcontext
     :drawable *dummy-window*
     :background *black*
     :foreground *white*
     :fill-style :tiled
     :tile pm)))
  
(defun xtw-grey25-gc ()
  (xtw-make-grey-gc
   #'(lambda (pm)
       (draw-rectangle pm *black-gc* 0 0 2 2 t)
       (draw-point pm *white-gc* 0 0))))

(defun xtw-grey50-gc ()
  (xtw-make-grey-gc
   #'(lambda (pm)
       (draw-rectangle pm *black-gc* 0 0 2 2 t)
       (draw-point pm *white-gc* 0 0)
       (draw-point pm *white-gc* 1 1))))

(defun xtw-grey75-gc ()
  (xtw-make-grey-gc
   #'(lambda (pm)
       (draw-rectangle pm *white-gc* 0 0 2 2 t)
       (draw-point pm *black-gc* 0 0))))

;;; The following was just too inefficient.  And prone to X errors.

;;(defun window-parent (w)
;;  (multiple-value-bind (kids p)
;;		       (query-tree w)
;;		       kids
;;		       p))

;;(defun window-grandparent (w)
;;  (window-parent (window-parent w)))

