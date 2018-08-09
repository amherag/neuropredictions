
;;;  USER-INTERFACE
;;;
;;;  User interface to AOP by Mark Torrance (torrance@cs.stanford.edu)
;;;
;;;  Help functions, interactive prompt, loading and debugging of
;;;  agent programs.

;;;  Includes new code for displaying separate windows for each agent,
;;;  and for running a world-simulation in a separate window, if
;;;  desired.


;;;  This variable determines whether to use the multiple windows and
;;;  world-simulation display version of the AGENT0 user interface.
;;;  Defaults to no.  (not yet implemented)

(in-package :user)

(defvar *aop-graphics* nil)


;;;  *show-msgs* toggles whether or not to display messages between
;;;  agents as they are sent.

(defvar *show-msgs* nil)



;;; get-current-time currently returns the current universal time.

(defun get-current-time ()
  (get-universal-time))

(defun now ()
  (get-universal-time))

;;;  Time constants in seconds for minute, hour, day, and year:

(defconstant m 60)
(defconstant h 3600)
(defconstant day (* 24 h))
(defconstant yr (* 365 day))


;;;  Create a new agent named "agent" if it doesn't already exist

(defvar *current-agent* nil)
(unless *current-agent*
  (setq *current-agent* (make-agent :name 'agent))
  (new-agent *current-agent*))


;;;  STRCAT
;;;
;;;  Concatenate two strings resulting in a string

(defun strcat (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(defun xnor (x y)
  (or (and x y) (not (or x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  AOP
;;;
;;;  Runs an interactive command line which understands certain
;;;  commands, and evals the command line if it cannot otherwise
;;;  parse it.  The function (aop-help) tells more about what
;;;  these commands do.


(defun aop ()
  "AOP is the main loop for the Agent0 interpreter, an interpreter for
programs in the paradigm of Agent Oriented Programming (Yoav Shoham,
Stanford University CS Department).  Type (aop) to begin.  Type help
to the <AGENT> prompt for further instructions."
  (do () ((not (read-char-no-hang))) (read-char))
  (print-prompt)
  (do ((input (read-line) (read-line)))
      ((eq (aop-exec input) 'exit))
    (print-prompt)))

(defun print-prompt (&optional (running nil))
  (and running (format t "*"))
  (format t "<~A> "
	  (upcase-name *current-agent*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  DEFINE AOP COMMANDS
;;;

(defvar *command-hash* (make-hash-table))

(defun cmd (string-or-list function)
  (if (listp string-or-list)
      (mapcar #'(lambda (x) (cmd x function)) string-or-list)
    (setf (gethash (read-from-string string-or-list) *command-hash*)
	  function))
  nil)
	
(cmd "ls" '(progn
	     (format t "~%This is not UNIX, but I'll show beliefs anyway.~%")
	     (show-beliefs *current-agent*)))
(cmd "run" '(progn
	      (format t "~%Running AOP.  Type 'walk' or 'stop' to stop~%")
	      (run-aop)))
(cmd "agents" '(mapcar #'(lambda (agent) (format t "~A " (agent-name agent)))
		       *agents*))
(cmd '("bels" "beliefs") '(show-beliefs *current-agent*))
(cmd '("cmts" "cmt" "commitments")
     '(show-commitments *current-agent*))
(cmd '("all-beliefs" "all-bels" "allbels")
     '(progn (show-beliefs *current-agent*)
	     (show-commitments *current-agent*)))
(cmd "clrbels" '(clear-agent-beliefs *current-agent*))
(cmd "clrcmts" `(clear-agent-commitments *current-agent*))
(cmd '("clearall" "clrall" "clear-all")
     '(progn (clear-agent-beliefs *current-agent*)
	     (clear-agent-commitments *current-agent*)))
(cmd "cmtrules"
     '(print-commit-rules (agent-commit-rules *current-agent*)))

(cmd "showmsgs" '(setq *show-msgs* t))
(cmd "noshowmsgs" '(setq *show-msgs* nil))
(cmd "new" '(aop-new))
(cmd "about" '(aop-about))
(cmd '("time" "now") '(print-time (now)))
(cmd '("q" "quit" "stop" "exit" "walk") ''exit)
(cmd '("h" "help") '(aop-help))	    
(cmd "pop" '(setq *current-agent* (name-agent 'agent)))
(cmd '("g" "graphics") '(aop-start-graphics))
(cmd "nographics" ''nographics)


(defun run-function (function)
  (prog1
      (eval function)
    (fresh-line)))

;;;  AOP-EXEC
;;;
;;;

(defun aop-exec (input)
  (let ((function (when (not (string-equal input ""))
			(gethash (read-from-string input) *command-hash*))))
    (cond (function
	   (prog1
	       (run-function function)
	     (terpri)))
	    
	  ((string-equal input "")
	   (aop-step *agents* (get-current-time))
	   (terpri))

	  ((and (> (length input) 4)
		(string-equal (subseq input 0 4) "bel?"))
	   (let* ((fact-pattern
		   (lisp-parse-string "fact" (subseq input 4)))
		  (fact-pred (fact-pattern-pred fact-pattern)))
	     (princ (fact-tval-at-time
		     (find-fact fact-pred (agent-beliefs *current-agent*))
		   (fact-pattern-time fact-pattern)
		   (fact-pred-posp fact-pred)))
	   (terpri)))
	  
	  ((and (> (length input) 6)
		(string-equal (subseq input 0 6) "inform"))
	   (multiple-value-bind
	    (to-agent string-pos)
	    (read-from-string (subseq input 6))
	    (send-message
	     'inform to-agent (agent-name *current-agent*)
	     (lisp-parse-string "fact" (subseq input (+ string-pos 6))))
	    (format t "~A will be informed next tick~%" to-agent)))
	  
	  ((and (> (length input) 7)
		(string-equal (subseq input 0 7) "request"))
	   (multiple-value-bind
	    (to-agent string-pos)
	    (read-from-string (subseq input 7))
	    (send-message
	     'request to-agent (agent-name *current-agent*)
	     (lisp-parse-string "act" (subseq input (+ string-pos 7))))
	    (format t "~A will be requested next tick~%" to-agent)))
	  
	  ((and (= (length input) 8)
		(string-equal (subseq input 0 8) "incoming"))
	   (print (agent-incoming-messages *current-agent*))
	   (terpri))
	  
	  ((and (> (length input) 4)
		(string-equal (subseq input 0 4) "load"))
	   (princ (load-agent (subseq input 5)))
	   (terpri))
	  
	  ((and (> (length input) 7)
		(string-equal (subseq input 0 7) "believe"))
	   (beliefs-add-fact
	    (parse-string "fact" (subseq input 8))
	    *current-agent*)
	   (format t "~%Belief added~%"))
	  
	  ((and (> (length input) 5)
		(string-equal (subseq input 0 5) "state"))
	   (beliefs-add-fact
	    (lisp-parse-string "fact" (subseq input 6))
	    *current-agent*)
	   (format t "~%Belief added~%"))
	  
	  ((and (> (length input) 7)
		(string-equal (subseq input 0 7) "cmtrule"))
	   (push (lisp-parse-commit-rule (read-from-string (subseq input 8)))
		 (agent-commit-rules *current-agent*))
	   (format t "~%Commit rule added~%"))
	  
	((string-equal input "make")
	 (load-aop-parse))

	((and (> (length input) 2)
	      (string-equal (subseq input 0 2) "go")
	      (name-agent (read-from-string (subseq input 2))))
	 (setq *current-agent*
	       (name-agent (read-from-string (subseq input 2))))
	 (terpri))

	((name-agent (read-from-string input))
	 (setq *current-agent* (name-agent (read-from-string input)))
	 (terpri))

	(t (princ (eval (read-from-string input)))
	   (terpri))

	)))


;  (let ((com (open "/tmp/aopipe")))

;;;  RUN-AOP
;;;
;;;  more work to be done here.  This will allow command-line input
;;;  to interrupt interactively.  We should still print the prompt.
;;;  Should allow user to turn agents on and off.
;;;  Should use the timegrain of agents to decide how often to update them.

(defun run-aop ()
  (print-prompt 'running)
  (do ((command nil) (char nil))
      (nil nil)
    (when (and (setf char (read-char-no-hang))
	       (not (eq char #\Newline)))
      (setf command (aop-exec
		     (strcat (string char)
			     (read-line *standard-input* nil nil))))
      (when (eq command 'exit)
	(format t "~%Stopped running AOP.  Type <RETURN> to run a tick.  Type q to exit AOP.~%")
	(return nil))
      (print-prompt 'running))
    (sleep 3)
    (aop-step *agents* (get-current-time))))


;;;  AOP-HELP
;;;
;;;  Describes commands for use within the (aop) function

(defun aop-help (&optional topic)
  "Help for Agent0 main loop commands"
  (format t "
                       HELP FOR AGENT0\
                       ---------------\
                ~A is your current agent \
   load joe-triv         loads file joe-triv.lisp\
   agents                see all the defined agents \
   request <agent> <act> ~A requests <agent> to perform <act> \
   inform <agent> <fact> ~A informs <agent> of <fact> \
   beliefs               see the beliefs of ~A \
   cmtrules              see the commitrules of ~A \
   incoming              see the incoming messages \
   state (time prop)     assert the proposition as a belief \
   clrbels               remove all beliefs of ~A \
   cmtrule (msg mntl agent action)   add the new commit-rule to ~A\
   showmsgs              display messages when sent \
   noshowmsgs            don't display messages when sent (default) \
   \
   go <agent>            make the named agent current \
   q or quit or exit     leave the AOP function \
   run                   enter asynchronous mode (ticks run continuously) \
   walk or stop          return to synchronous mode (the initial default) \
   <return>              on a line by itself, runs a tick when in synch mode \
   h or help             display this list \
   (+ 3 5)               eval any lisp form\
   new                   display a list of what's new in this version \
   about                 author and copyright information \
\
"
	  (upcase-name *current-agent*)
	  (upcase-name *current-agent*)
	  (upcase-name *current-agent*)
	  (upcase-name *current-agent*)
	  (upcase-name *current-agent*)
	  (upcase-name *current-agent*)
	  (upcase-name *current-agent*)
	  ))
  
(defun upcase-name (agent) (string-upcase (symbol-name (agent-name agent))))


;;;  PARSE-STRING
;;;
;;;  This will ask paren to parse a string as a structure of type type.

;;;(defun parse-string (type string)
;;;  (paren:initialize-parser (strcat *aop-load-path* "aop-" type "-tables.lisp"))
;;;  (paren:parse-string string)
;;;  )


(defun prolog-form (proposition)
  proposition)

;;;  AOP-NEW
;;;
;;;  This will display the file new.txt from the aop-load-path, which
;;;  should contain a listing of new features in the current version
;;;  of AOP.

(defun aop-new ()
  (with-open-file (infile (strcat *aop-load-path* "new.txt"))
    (do ((line (read-line infile nil)
	       (read-line infile nil)))
	((null line))
      (format t "~A~%" line))))


;;;  AOP-ABOUT
;;;
;;;  Displays the file about.txt from the aop-load-path, which
;;;  contains information about the authors and the program's
;;;  copyright

(defun aop-about ()
  (with-open-file (infile (strcat *aop-load-path* "about.txt"))
    (do ((line (read-line infile nil)
	       (read-line infile nil)))
	((null line))
      (format t "~A~%" line))))


;;;  LOAD-AGENT
;;;
;;;  This function simply loads a lisp file with the base name given
;;;  from the directory *aop-agent-path*.  Said file should contain a
;;;  form (defagent ..), and 0 or more auxiliary functions which
;;;  implement private actions.  See the file bnf.txt for the syntax

(defun load-agent (name)
  (let ((filename (strcat *aop-agent-path* name ".lisp")))
    (if (probe-file filename)
	(progn
	  (format t "Loading agent file ~A~%" (string-upcase name))
	  (load filename))
	(format t "***No file named ~A found.  Check the variable *aop-agent-path*.~%" name))))


;;;  LOAD-AGENT
;;;
;;;  This function takes an agent name "agent" to load.
;;;  It asks paren to parse "agent.aop" and defines an agent
;;;  named "agent" with the parse as its program.  It also
;;;  loads the auxiliary file "agent.lsp" of lisp functions
;;;  to implement the agent's private actions.

;;;(defun load-agent (agent-name-string)
;;;  "Call LOAD-AGENT with a string which is the name of an agent to load.
;;;The files \"name.aop\" and \"name.lsp\" will be loaded from the directory
;;;stored in *aop-agent-path* (a string ending in \"/\")."
;;;  (let ((aop-filename (strcat *aop-agent-path* agent-name-string ".aop"))
;;;	(aux-filename (strcat *aop-agent-path* agent-name-string ".lsp"))
;;;	(agent-name (read-from-string agent-name-string)))
;;;    (when (probe-file aux-filename)
;;;      ;;  An aux file is present
;;;      (load aux-filename))
;;;    (if (probe-file aop-filename)
;;;	(progn
;;;	  (setq *defining-agent*
;;;		(make-agent :name agent-name))
;;;	  (format t "Defining agent ~A~%" (string-upcase agent-name-string))
;;;	  (parse-file aop-filename)
;;;	  (setf (agent-name *defining-agent*)
;;;		(read-from-string agent-name-string))
;;;	  (new-agent *defining-agent*)
;;;	  (setq *current-agent* *defining-agent*)
;;;	  (setq *defining-agent* nil)
;;;	  'loaded)
;;;	(format t "No file ~A found for that agent; try again~%"
;;;		aop-filename))))


;;;  PARSE-FILE
;;;
;;;  Calls the Paren function paren:parse-file to read an agent file into
;;;  *defining-agent*

;;;(defun parse-file (filename)
;;;  (format t "Parsing file ~A now~%" filename)
;;;  (paren:initialize-parser (strcat *aop-load-path* "aop-tables.lisp"))
;;;  (paren:parse-file filename)
;;;  )



;;;(defun make (type)
;;;  (paren:make-tables-from-source (strcat *aop-load-path* "aop-" type ".src"))
;;;  (paren:initialize-parser (strcat *aop-load-path* "aop-" type "-tables.lisp"))
;;;  t)



;(system "xterm -title \"AOP COMMAND INTERFACE\"
;               -e sh -c 'cat > /tmp/aopipe' &")


;;  inform can be called from within an agent's action function, to
;;  hack another agent's beliefs (or your own) to add a new fact

(defun inform (agent-name predicate time)
  (beliefs-add-fact (lisp-parse-fact (list time predicate))
		    (name-agent agent-name))
  t)


(defun inform-fact (agent-name fact)
  (beliefs-add-fact (lisp-parse-fact fact) (name-agent agent-name))
  t)

(defun send-inform (from-agent-name to-agent-name fact)
  (send-message 'inform
		to-agent-name
		from-agent-name
		(lisp-parse-fact fact)))

(defun send-request (from-agent-name to-agent-name act)
  (send-message 'request
		to-agent-name
		from-agent-name
		(lisp-parse-act act)))

(defun bel? (agent-name fact)
  (let* ((fact-pattern
	  (lisp-parse-fact fact))
	 (fact-pred (fact-pattern-pred fact-pattern)))
    (fact-tval-at-time
     (find-fact fact-pred (agent-beliefs (name-agent agent-name)))
     (fact-pattern-time fact-pattern)
     (fact-pred-posp fact-pred))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  GRAPHICS
;;;  
;;;  Routines to draw a separate window for each agent


(defun run ()
  (xtw-with-open-display
   '(xtw-with-world-ui
     '(aop-main-event-loop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aop-main-event-loop ()
  (aop-init-world)
  (xtw-event-loop '(aop-world-step)))

(defun aop-world-step ()
  (if *exit*
      (setf *quit* t)
    (user-world-step)))

(defun user-world-step ()
  )

;;;  User program should specify user-world-initialize and
;;;  user-world-step functions if it wants to run a graphic
;;;  world-simulation



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  Uses functions from ui.lisp and world-ui.lisp

