
;;;  X-GRAPHICS.LISP
;;;
;;;

;;;  Graphics for AOP in Xwindows
;;;
;;;  Uses MCT, Mark's CLX Toolkit

(in-package :user)

;;  Where to look for MCT

(defvar *mct-load-path* "~aop/lisp/mct/")


(defvar *graphics-loaded* nil)
(defvar *command-window* nil)
(defvar *agent-windows* nil)


(defun aop-start-graphics ()
  (format t "Starting Graphic Mode~%")
  (when (not *graphics-loaded*)
	(format t "Loading Graphics Package; please wait~%")
	(load-graphics))
  (unwind-protect
      (progn
	(mct-init-xwindows)
	(print-prompt)
	(setq *busy* t)
	(recalc-command-window)
	(setq *event-level* 1)
	(setq *quit* 0)
	(mct-event-loop '(aop-work)))
    (aop-finish-graphics)))

(defun aop-finish-graphics ()
  (mct-close-all)
  (setq *agent-windows* nil)
  (setq *command-window* nil)
  (setq *agent-window-coords* '((0 0) (0 600) (500 0) (500 600)
				(0 0) (0 0) (0 0) (0 0) (0 0)))
  (setq *agent-window-used-coords* nil)
  't)

(defun load-graphics ()
  (load (strcat *mct-load-path* "load.cl"))
  (mct-load-all)
  (setq *graphics-loaded* t))

;;  UG
;;
;;  "Unload Graphics" resets the flag, for when the MCT toolkit has
;;  changed.

(defun ug ()
  (setf *graphics-loaded* nil))

(defvar *running* nil)
(defvar *next-tick-time* (now))
(defvar *aw-to-kill* nil)


(defun aop-work ()
  (let (char)
    (when (> (get-current-time) *next-tick-time*)
	  (setf *next-tick-time* (+ (get-current-time) 5))
	  (when *running* (aop-step *agents* (get-current-time)))
	  (dolist (agent *agents*) (recalc-agent-window agent))
	  (recalc-command-window)
	  (dolist (aw *aw-to-kill*) (kill-aw aw))
	  )
    (when (setf char (read-char-no-hang))
	  (setf command
		(aop-exec
		 (strcat (if (eq char #\Newline) "" (string char))
			 (read-line *standard-input*)))) ; nil nil
	  (print-prompt)
	  (cond ((eq command 'exit)
		 (format t "~%Stopped running AOP. Type <RETURN> to run a tick. Type 'q' to exit AOP.~%")
		 (setq *running* nil))
		((eq command 'nographics)
		 (setq *quit* *event-level*))
		))))

(defun kill-aw (aw)
  (aop-kill-agent (aw-agent aw))
  (mct-close-window (mct-wi-kid (aw-wi aw)))
  (setf *agent-windows* (remove (assoc (agent-name (aw-agent aw))
				       *agent-windows*)
				*agent-windows*))
  (push (pop *agent-window-used-coords*) *agent-window-coords*)
  (setf *aw-to-kill* (remove aw *aw-to-kill*)))

(defun aop-kill-agent (agent)
  (setf *agents* (remove agent *agents*))
  (if (eq *current-agent* agent)
      (setf *current-agent* (name-agent 'agent))))


;;;  STRUCTURE CW
;;;
;;;  cw is the Command window

(defstruct cw
  wi
  )

(defun recalc-command-window ()
  (let ((cw *command-window*))
    (unless cw
	    (setf cw (create-command-window))
	    (setf *command-window* cw))
    ;;  No action
    ))


(defun create-command-window ()
  (let* ((tick (button " Tick " :function #'tick-button
		       :expand-x t :expand-y t))
	 (run (button " Run " :function #'run-button
		      :expand-x t :expand-y t))
	 (agent (button " New " :function #'new-agent-button
			:expand-x t :expand-y t))
	 (quit (button " Quit " :upf #'quit-button
		       :expand-x t :expand-y t))
	 (title (hlabel "AGENT0" :font *italic-font*)))
    (setf
     *command-window*
     (mct-install-window
      :name "Agent0"
      :x 1020 :y 460
      :kid (vframe (space 5)
		   (hframe (space 3)
			   (vframe
			    title
			    (space 3)
			    tick
			    (space 3)
			    run
			    (space 3)
			    agent
			    (space 3)
			    quit
			    (space 3))
			   (space 3)))))))
		       
(defun tick-button (w)
  (aop-step *agents* (get-current-time)))

(defun run-button (w)
  (setq *running* (not *running*)))

(defun new-agent-button (w)
  (format t "Cannot yet define new agents from Graphics.~%")
  )

(defun quit-button (w)
  (setq *quit* *event-level*))

;;;  STRUCTURE AW
;;;
;;;  aw is an Agent Window

(defstruct aw
  wi					; The corresponding MCT-window
  belief-text				; Text for beliefs
  cmt-text				; Text for commitments
  beliefs

  cmtrule-text				; Text for cmtrules  
  cmtrules
  agent					; Agent
  )

;;;  RECALC-AGENT-WINDOW
;;;
;;;  recalc-agent-window will create the agent window if it doesn't
;;;  yet have one.

(defun recalc-agent-window (agent)
  (let ((aw (rest (assoc (agent-name agent) *agent-windows*)))
	changed)
    (when (null aw)
	  (setf aw (create-agent-window agent))
	  (push (cons (agent-name agent) aw) *agent-windows*))
    (recalc-beliefs agent aw)
    (recalc-cmtrules agent aw)))

(defun recalc-beliefs (agent aw)
;  (unless
;   (and (eq (agent-beliefs (aw-agent aw)) (aw-beliefs aw))
;	(aw-belief-text aw) (aw-cmt-text aw)))
  (setf (aw-beliefs aw) (agent-beliefs agent))
  (setf (aw-belief-text aw)
	(with-output-to-string
	 (stream) (show-beliefs agent stream)))
  (setf (aw-cmt-text aw)
	(with-output-to-string
	 (stream) (show-commitments agent stream))))

(defun recalc-cmtrules (agent aw)
;  (unless
;   (and (eq (agent-commit-rules agent) (aw-cmtrules aw))
;	(aw-cmtrule-text aw)))
  (setf (aw-cmtrules aw) (agent-commit-rules agent))
  (setf (aw-cmtrule-text aw)
	(with-output-to-string
	 (stream) (print-commit-rules
		   (agent-commit-rules agent) stream))))

(defun get-beliefs (beliefs)
  (let (result)
    (dolist (belief beliefs)
	    (unless (eq (first (fact-pred belief))
			'cmt) (push belief result)))
    (reverse result)))

(defun get-cmts (beliefs)
  (let (result)
    (dolist (belief beliefs)
	    (when (eq (first (fact-pred belief))
		      'cmt) (push belief result)))
    (reverse result)))

(setq *agent-window-coords* '((0 0) (0 600) (500 0) (500 600)
				(0 0) (0 0) (0 0) (0 0) (0 0)))
(setq *agent-window-used-coords* nil)


;;;  CREATE-AGENT-WINDOW
;;;

(defun create-agent-window (agent)
  (let* ((coord (pop *agent-window-coords*))
	 (aw (make-aw :agent agent))
	 (foo (recalc-beliefs agent aw))
	 (bar (recalc-cmtrules agent aw))
	 (beliefs (stext
		   :getf #'(lambda (w) (aw-belief-text (widget-data w)))
		   :data aw
		   :font *small-font* :lines 5))
	 (cmts (stext :getf #'(lambda (w) (aw-cmt-text (widget-data w)))
		      :data aw
		      :font *small-font* :lines 2))
	 (cmtrules (stext
		    :getf #'(lambda (w) (aw-cmtrule-text (widget-data w)))
		    :data aw
		    :font *small-font* :lines 5))
	 (load (button " Load "
		       :upf #'(lambda (w)
				(load-agent (string-downcase
					     (string (widget-data w)))))
		       :data (agent-name agent)))
	 (kill (button " Kill " :upf #'(lambda (w)
					 (push (widget-data w) *aw-to-kill*))
		       :data aw))
							  
	 (editbutton (button "  Edit  "
			     :downf #'(lambda (w)
					(format t "Edit doesn't work yet.~%"))
			     ))
	 (loadbutton (button " Load "
			     :downf #'(lambda (w)
					(format t "Load doesn't work yet.~%"))
			     ))
	 (cmtctrls (vframe editbutton (space 3) loadbutton))
	 (cmdframe (hframe (espace) load (space 20) kill (espace)))
	 (main (vframe
		(space 5) cmdframe (space 5)
		(hlabel "Beliefs")
		beliefs (space 5)
		(hlabel "Commitment Rules")
		(hframe cmtrules cmtctrls (space 5)) (space 5)
		(hlabel "Commitments")
		cmts (space 5)
		))
	 (wi (mct-install-window
	      :name (string (agent-name agent))
	      :x (first coord) :y (second coord)
	      :kid main)))
    (push coord *agent-window-used-coords*)
    (setf (aw-wi aw) wi)
    aw
    ))