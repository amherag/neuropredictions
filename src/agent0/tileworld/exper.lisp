;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; experiment.lisp
;;;
;;; Experiment-control functions for the tileworld system.
;;; Requires Sun Common Lisp, X windows, and CLX.
;;;
;;; Michael Frank -- 1990 July 20 Friday -- 3:21 pm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'xtile)

(export '(run small medium large))

(defun small () (setf *label-font-name* "*-6-*"))
(defun medium () (setf *label-font-name* "*-16-*"))
(defun large () (setf *label-font-name* "*-r-*-34-*"))

(defvar *experiment-dir-name*)
(setf *experiment-dir-name* "~tile/xtile/experiments")
(defvar *exper-keephistory*)
(defvar *exper-historyfile*)

;;;
;;; Load an experiment file from the experiment-directory.
;;;

(defun load-experiment-file (efilename)
  (setf *experiment-file-name* 
	(defsystem::append-directories 
	  *experiment-dir-name*
	  efilename))
  (load *experiment-file-name*)
)


;;;
;;; The function that should always be called after an experiment
;;; file is loaded.
;;;

(defun post-load ()
  (setf *current-experiment-list* *experiment-list*)
  (format t ";;; Starting a match.~%")
  (start-set)
  )


;;;
;;; Set things up for a new set of experiments.
;;;

(defun start-set ()
  (setf *current-experiment* (car *current-experiment-list*))
  (eval *current-experiment*)
  (log-init)  ;; reset logging and averaging upon starting a set.
  (format t ";;;  Starting a set.~%")
  )


;;; The default experiment-file name.
(setf *loaded-as* "default.exper.lisp")


;;;
;;; run - The function that the user should invoke in order to start
;;;       tileworld.  Takes an optional experiment-file name.
;;;

(defun run (&optional experiment-name)
  (cond ((stringp experiment-name)
	 (format t "Loading ~s as the experiment-definition file.~&"
		 experiment-name)
	 (load-experiment-file experiment-name)
	 )	;Assume strings are filenames for loading experiment-lists
	((null experiment-name)
	 (if (and (boundp '*experiment-file-name*) *experiment-file-name*)
	     (progn (format t ";;; Loading previous experiment file.~&"
			    *experiment-file-name*)
		    (load *experiment-file-name*))
	   (progn (format t "Loading default experiment file.~&")
		  (load-experiment-file "default.exper.lisp"))))
	((format t "WARNING: Ignoring invalid experiment-name.~&")))
  
  (if (null *experiment-list*)
      (format t "Can't run - no list of experiments to do.~&")
    (xtw-with-open-display
     '(xtw-with-ui-defaults
       '(xtw-with-experiment-ui
	 '(xtw-with-world-ui
	   '(xtw-with-agent-ui
	     '(xtw-main-loop))))))))	;main sub


;;; xtw-main-loop
;;;                 Intended to do a single xtw-event-loop
;;; that is not supposed to terminate until we are really,
;;; truly done and we want the windows to close and the
;;; program to stop.


(defun xtw-main-loop ()
  (post-load)
  (start-game)
  (xtw-event-loop '(xtw-step)))

;;;
;;; There will be separate functions for ending the game,
;;; the set, and the match.  A set is a series of games
;;; whose only difference is different seeds.  A match
;;; is a series of sets, with possibly different
;;; parameters.
;;;

(defun xtw-end-game ()
  (format t ";;;   Ending game.~&")
  (close-history-if-open)
  (log-settings)	;log the settings upon ending game
  (log-score)
  (cond ((> *world-nseeds* 0)
	 (incf *world-rand-seed*)
	 (decf *world-nseeds*)
	 (setf *busy* (not *world-wait-between-seeds*))
	 (start-game))
	(t
	 (setf *busy* (not *world-wait-after-all-seeds*))
	 (xtw-end-set))))

(defun xtw-end-set ()
  (format t ";;;  Ending set.~&")
  (log-average)
  (setf *current-experiment-list* (cdr *current-experiment-list*))
  (cond (*current-experiment-list*
	 (start-set)
	 (start-game))
	(t
	 (xtw-end-match))))

(defun xtw-end-match ()
  (format t ";;; Ending match.~&")
  (setf *quit* t))
	 
	 

;;;;;;;;;;;;;;;;; xtw-run-experiments and xtw-run-experiment
;;;;;;;;;;;;;;;;; are now obsolete.  The comments are good though.

;;;
;;; run-experiments does not simply do a mapcar of run-experiment
;;; over the experiment-list, because we want to leave open the possibility
;;; of interactively skipping experiments on the list, or displaying the
;;; remainder of the list, etc.
;;;
;;; run-experiments should actually be run after opening the display
;;; and bringing up the experiment window.
;;;

;;(defun xtw-run-experiments ()
;;  (setf *current-experiment-list* *experiment-list*)
;;  (do ()
;;      ((null *current-experiment-list*)
;;       (format t "No more experiments in list.~&"))
;;      (setf *current-experiment* (car *current-experiment-list*))
;;      (xtw-run-experiment)
;;      (setf *current-experiment-list* (cdr *current-experiment-list*))
;;      ))		;Main sub

;;; This function is the place where the user-interfaces are brought
;;; up and the main-loop is run.

;;(defun xtw-run-experiment ()
;;  (eval *current-experiment*)		;Set up parameters
;;  (loop
;;   (xtw-ui-reset)			;reset user interface
;;   (xtw-start-experiment)		;start things going
;;   (xtw-event-loop '(xtw-step))	;Main sub
;;   (cond ((> *world-nseeds* 0)
;;	  (incf *world-rand-seed*)
;;	  (decf *world-nseeds*)
;;	  (setf *busy* (not *world-wait-between-seeds*))
;;	  )
;;	 ((return))))
;;  (setf *busy* (not *world-wait-after-all-seeds*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; The meaning of xtw-start-experiment is "initialize everything,
;;; based on current parameters, to the beginning of the simulation."
;;; This is in contrast with xtw-ui-reset which just means to set
;;; up the user-interface to reflect the new parameters.
;;;   xtw-ui-reset must be done first so that xtw-start-experiment
;;; can display things in the user interface in its correct form.
;;; xtw-start-experiment should also serve to reset the game to the
;;; beginning.
;;;;  IMPORTANT NOTE:  Whenever there is a possibility of parameters
;;;; having been changed, xtw-ui-reset MUST be called BEFORE calling
;;;; xtw-start-experiment.
;;;;  Hm. Maybe I can think of a better name for this function.
;;;; It's used in resetting the current game, and also in starting
;;;; new games. Oh well.

(defun xtw-start-experiment ()
  (open-history)		;open history file if *keephistory* set
  (xtw-world-start)
  (xtw-agent-start)
)

;;; A common pair.  xtw-ui-reset and xtw-start-experiment.
;;; This sets up a game, as opposed to setting up a whole set.

(defun start-game ()
  (xtw-ui-reset)
  (xtw-start-experiment)
  (format t ";;;   Starting a game.~%")
  )

;;;
;;; xtw-step is used as the work-function in
;;; the event-loop.  xtw-step performs a single step of the
;;; simulation.  It should never take more than a fraction of a second,
;;; so that in between steps, the user-interface can respond to input
;;; events without a great delay.
;;;

(defun xtw-step ()
  (cond ((and *game-length* (>= *elapsed* *game-length*))
	 (xtw-end-game)
	 t)
	((let (mind1 mind2 think-time cycles-to-act)
	   (setf mind1 (agent-copy *agent*))
	   (setf mind2 (agent-copy *agent*))
	   (setf think-time (think *world* mind1))
	   (keephist "THINK TIME USED:"  'write-plain think-time)
	   (setf cycles-to-act (- (floor (/ (if *game-length*
						(min (+ *elapsed* think-time)
						     *game-length*)
					      (+ *elapsed* think-time))
					    (agent-act-time *agent*)))
				  (floor (/ *elapsed*
					    (agent-act-time *agent*)))))
	   (multiple-value-setq (*world* mind2)
				(act *world* mind2 cycles-to-act))
	   (merge-minds *agent* mind1 mind2)
	   (setf *elapsed* (+ *elapsed* think-time))
	   (setf *time-remaining* (- *game-length* *elapsed*))
	   (xtw-world-timedisp)
	   (xtw-world-scoredisp)
	   ))))

(defun exper-current-time ()
  (subseq (reverse (multiple-value-list (get-decoded-time)))
	  3))

