
;;;  STRUCTURES
;;;
;;;  Data types and structures used in AOP


;;;  *aop-path* is a string which is the path for aop agent programs.
;;;  Programs written in agent0 should have filenames ending with
;;;  .aop, and may have Lisp support files ending with .lsp.  Both
;;;  types of files should be stored in the directory given here:

(setq *aop-path* "~/aop/")


;;;       NOT NEEDED ANYMORE -- MARK
;;; Quick setup hack for loop in akcl (Paul)
;;;#+akcl (defmacro loop (&rest args)
;;;	 `(sloop:sloop ,@args))


;;; Data Types: agent, time, fact, beliefs, commitments, capabilities.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  *AGENTS*

(defvar *agents* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  AGENT

(defstruct agent
  name					; A symbol
  (timegrain 10)			; Timegrain in seconds
  beliefs
  commit-rules
  (commitments nil)			; The current set of commitments
  capabilities
  incoming-messages
  messages
  )


;;;  This variable stores the new agent as it is built up by the
;;;  Paren parser.  It is used below in load-agent.

(defvar *defining-agent* (make-agent))


;;;  NEW-AGENT
;;;
;;;  call new-agent to define a new agent in the world.  It expects an
;;;  agent structure which is already filled out.

(defun new-agent (agent)
  (let ((agent-name (agent-name agent)))
    (when (name-agent agent-name)
      (format t "Replacing existing agent ~S in the current context~%"
	      agent-name)
      (setq *agents* (remove (name-agent agent-name) *agents* :test #'equal)))
    (push agent *agents*)))

;;  returns the agent with name as its name.
(defun name-agent (name)
  (car (member name *agents* :key #'agent-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  MESSAGE

(defstruct (message (:type list) :named)
  sender
  receiver
  type
  content				; either a fact or an action.
)

(defun message-inform-p (message)
  (eq (message-type message) 'inform))

(defun message-request-p (message)
  (eq (message-type message) 'request))

(defstruct (message-condition (:type list) :named)
  sender
  type
  content)

(defun print-commit-rules (cmtrules &optional (stream t) (depth 0))
  (mapcar
   #'(lambda (cmtrule)
       (format stream
	       "~&(MSGCOND:  ~A~& MNTLCOND: ~A\
 AGENT:    ~A~& ACTION:   ~A)~&"
	       (show-msgcond (commit-rule-msgcond cmtrule))
	       (show-mntlcond (commit-rule-mntlcond cmtrule))
	       (commit-rule-agent-name cmtrule)
	       (show-action (commit-rule-action cmtrule))))
   cmtrules))

(defun show-msgcond (msgcond)
  (show-m msgcond #'show-single-msgcond))

(defun show-mntlcond (mntlcond)
  (show-m mntlcond #'show-single-mntlcond))

(defun show-m (mcond function)
  (if (and (listp mcond) (> (length mcond) 1))
      (cons 'or (mapcar #'(lambda (x) (show-m-conj x function)) mcond))
      (show-m-conj (first mcond) function)))

(defun show-m-conj (mcond function) 
  (if (and (listp mcond) (> (length mcond) 1))
      (cons 'and (mapcar #'(lambda (x) (show-1-m x function)) mcond))
      (show-1-m (first mcond) function)))

(defun show-1-m (mcond function)
  (if (eq (first mcond) 'not)
      (list 'not (funcall function (second mcond)))
      (funcall function mcond)))

(defun show-single-msgcond (msgcond)
  (if msgcond
      (list (message-condition-sender msgcond)
	    (message-condition-type msgcond)
	    (if (eq (message-condition-type msgcond) 'request)
		(show-action (message-condition-content msgcond))
	      (show-fact (message-condition-content msgcond))))
    nil))

(defun show-single-mntlcond (mntlcond)
  (if mntlcond
      (show-fact (mental-condition-pattern mntlcond))
      nil))


;;;  CLEAR-AGENT-BELIEFS and CLEAR-AGENT-COMMITMENTS
;;;
;;;

(defun clear-agent-beliefs (agent)
  (let (beliefs)
    (dolist (belief (agent-beliefs agent))
	    (when (eq (car (fact-pred belief)) 'cmt)
		    (push belief beliefs)))
    (setf (agent-beliefs agent) beliefs))
  (format t "Cleared beliefs~%"))

(defun clear-agent-commitments (agent)
  (let (beliefs)
    (dolist (belief (agent-beliefs agent))
	    (unless (eq (car (fact-pred belief)) 'cmt)
		    (push belief beliefs)))
    (setf (agent-beliefs agent) beliefs))
  (format t "Cleared commitments~%"))


;;;  SHOW-BELIEFS

(defun show-beliefs (agent &optional (stream t))
  (mapcar #'(lambda (fact)
	      (unless
	       (eq (car (fact-pred fact)) 'cmt)
	       (format stream "~A   [.. U]" (show-pred (fact-pred fact)))
	       (mapcar #'(lambda (status)
			   (format stream " [")
			   (print-time (fact-status-time status) stream)
			   (format stream " ~A]"
				   (case (fact-status-truth-value status)
					 ('t t)
					 ((nil 'f) 'f)
					 ('unknown 'unknown))))
		       (reverse (fact-status fact)))
	       (format stream "~%")))
	  (agent-beliefs agent))
  't)

(defun show-commitments (agent &optional (stream t))
  (mapcar #'(lambda (fact)
	      (when (eq (car (fact-pred fact)) 'cmt)
		    (format stream "~A" (show-pred (fact-pred fact)))
		    (when (> (length (fact-status fact)) 2)
			  (format stream "~%   "))
		    (format stream "    [.. U]")
		    (mapcar #'(lambda (status)
				(format stream " [")
				(print-time (fact-status-time status)
					    stream)
				(format stream " ~A]"
					(case (fact-status-truth-value status)
					      ('t t)
					      ((nil 'f) 'f)
					      ('unknown 'unknown))))
			    (reverse (fact-status fact)))
		    (format stream "~%")))
	  (agent-beliefs agent))
  't)
		

(defun show-pred (proposition)
  (if (cmt-p proposition)
      (show-cmt proposition)
      proposition))

(defun show-cmt (cmt)
  (list 'cmt
	(cmt-from-agent cmt)
	(cmt-to-agent cmt)
	(show-action (cmt-action cmt))))

(defun print-action (action &optional (stream t) (depth 0))
  (format stream "~A" (show-action action)))

(defun show-action (action)
  (if (action-p action)
      (let ((stream (make-string-output-stream)))
	(cond ((action-inform-p action)
	       (format stream "(~A ~A ~A ~A)"
		       (upcase-symbol (action-type action))
		       (time-string (action-time action))
		       (upcase-symbol (action-agent action))
		       (show-fact (action-fact action))))
	      ((or (action-request-p action)
		   (action-unrequest-p action))
	       (format stream "(~A ~A ~A ~S)"
		       (upcase-symbol (action-type action))
		       (time-string (action-time action))
		       (upcase-symbol (action-agent action))
		       (show-action (action-action action))))
	      ((eq (action-type action) 'if)
	       (format stream "(IF ~S ~S)"
		       (action-mntlcond action)
		       (action-action action)))
	      ((eq (action-type action) 'do)
	       (format stream "(DO ~A ~S)"
		       (time-string (action-time action))
		       (private-action-fn (action-private-action action))))
	      (t (error "print-action: unknown action type ~A"
			(action-type action)))
	      )
	(get-output-stream-string stream))
    action))

(defun show-fact (fact)
  (cond ((fact-pattern-p fact)
	 (strcat
	  "(" (time-string (fact-pattern-time fact)) " "
	  (write-to-string (fact-pattern-pred fact)) ")"))
	((integerp (first fact))
	 (strcat "(" (time-string (first fact))
		 (subseq (write-to-string (rest fact)) 1)))
	(t fact)))

(defun upcase-symbol (sym)
 (if (symbolp sym)
     (string-upcase (symbol-name sym))
     sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  ACTION

(defstruct (action (:type list) :named)
  type
  time
  agent
  mntlcond
  action
  private-action
  fact)

(defmacro action-if-p (action)
  `(eq 'if (action-type action)))
  
(defmacro action-do-p (action)
  `(eq 'do (action-type action)))
  
(defmacro action-request-p (action)
  `(eq 'request (action-type action)))

(defmacro action-unrequest-p (action)
  `(eq 'unrequest (action-type action)))
  
(defmacro action-inform-p (action)
  `(eq 'inform (action-type action)))
  
(defmacro action-refrain-p (action)
  `(eq 'refrain (action-type action)))



;;; --- private-action ---
(defstruct (private-action (:type list) :named)
  fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- Time ---
;;; Time needs to support ordering and template matching.  It is
;;; assumed that times will be kept in sorted order.  This allows for
;;; efficient retrieval of ranges and times.


(defconstant days '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defconstant months '("foo" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
		      "Sep" "Oct" "Nov" "Dec"))


(defun print-time (time &optional (stream t) (depth 0))
  (format stream "~A" (time-string time)))

(defun time-string (time)
  (let ((stream (make-string-output-stream)))
    (if (not (numberp time))
	(format stream "~s" time)
	(if (< time 10000)
	    (format stream "~d" time)
	    (multiple-value-bind
		  (second minute hour date month year day-of-week
			  daylightp time-zone)
		(decode-universal-time time 7) ; correct only
					; in pacific time zone
	      (multiple-value-bind (csecond cminute chour cdate cmonth cyear
					    cday-of-week cdaylightp ctime-zone)
		  (decode-universal-time (get-current-time) 7)
		(if (= year cyear)
		    (cond ((and (= date cdate) (= month cmonth))
			   (format stream "~d:~2,'0d:~2,'0d"
				   hour minute second))
			  ((and (= month cmonth) (< (- date cdate) 7)
				(> (- date cdate) 0))
			   (format stream "~a ~d:~2,'0d:~2,'0d"
				   (nth day-of-week days) hour minute second))
			  (t (format stream "~a ~a ~d ~d:~2,'0d:~2,'0d"
				     (nth day-of-week days)
				     (nth month months)
				     date hour minute second)))
		    (format stream "~a ~a ~d ~d:~2,'0d:~2,'0d ~d"
			    (nth day-of-week days)
			    (nth month months)
			    date hour minute second year))))))
    (get-output-stream-string stream)))

(defun time-compare (time1 time2 func)
  (let ((timea (if (eq time1 'now) (now) time1))
	(timeb (if (eq time2 'now) (now) time2)))
    (apply func (list timea timeb))))


;;;  replace all occurrences of the atom 'now with time in expression

(defun replace-now (expression time)
  (when expression
    (if (atom expression)
	(if (eq expression 'now) time expression)
	(cons (replace-now (car expression) time)
	      (replace-now (cdr expression) time)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- Facts ---
;;; The current version of AOP, agent0, does not allow for the
;;; disjuction of facts.  

;;; Currently there is only support for retreival of fact from a
;;; particular time OR from any time.  It might be nice to support the
;;; retrieval of facts that are in a certain time range.  It is a
;;; trivial hack to retrieve the "latest" time that matches (in fact
;;; this is sort of the way that it happens because facts are true
;;; until they are contradicted).

;;; A note on NOT.  (not (on a b)) is true in two conditions:
;;;   - the latest fact that mentions (on a b) is (not (on a b))
;;;   - there is no fact that mentions (on a b)

;;;  Each FACT is a structure consisting of pred, which is the content
;;;  of the fact, and status, which is a list of FACT-STATUS records
;;;  sorted with the most recent or furthest future fact first, and
;;;  the oldest fact last.  Agent is used to specify with whom you
;;;  believe the fact.

(defstruct (fact (:type list) :named)
  pred					; Content
  status
  agent					; With whom do you believe it
					; This is for common belief
  )

;;; The FACT-PRED contains one of the following fact types: p, b, cmt, can.
;;; Note: they are all of type 'LIST.  This allows them to be passed
;;; straight through to UNIFY.


;;;  A FACT-STATUS record contains the time about which the record refers, and the truth-value that the
;;;  associated belief took on or will take on at that time.

(defstruct (fact-status (:type list) :named)
  time
  truth-value
  )


(defun fact-time-tval (fact time)
  (if fact
      (let ((member (member time (fact-status fact) :key #'fact-status-time
			    :test #'(lambda (x y) (time-compare x y #'>=)))))
	(if member
	    (fact-status-truth-value (car member))
	    'unknown))
      'unknown))

(defun fact-tval-at-time (fact time &optional (tval 't))
  (eq tval (fact-time-tval fact time)))

(defstruct (belief (:type list) :named)	; Belief
  agent
  fact)

(defstruct (cmt (:type list) :named)	; Commitment
  from-agent
  to-agent
  action)

(defstruct (can (:type list) :named)	; Capabilites of agents.
  agent
  stuff)

(defun strip-nots (fact-pred)
  "Return an un-negated fact-pred from a possibly negated one."
  (cond ((null fact-pred)
	 fact-pred)
	((eq (car fact-pred) 'not)
	 (strip-nots (cadr fact-pred)))
	(t
	 fact-pred)))

(defun fact-pred-posp (fact-pred)
  "If the fact is positive (has an even number of nots wrapped
around it) return t, otherwise nil"
  (equal fact-pred (strip-nots fact-pred)))


;;;  find-fact finds the fact in beliefs which has fact-pred as its
;;;  predicate.

(defun find-fact (fact-pred beliefs)
  (let ((fact-rest (member (strip-nots fact-pred) beliefs
			   :key #'fact-pred :test #'equal)))
    (when (and fact-rest (first fact-rest))
      (first fact-rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- commit-rule ---

(defstruct (commit-rule (:type list) :named)
  msgcond
  mntlcond
  agent-name
  action)



;;; --- mmtlcond and mntlpatterns---
;;; Mental conditions match against beliefs.  In AGENT0 mental
;;; conditions can be conjunctions, each element of CONDITIONS is one
;;; of those conjunctions.
;;; Mental conditions are trees of mental patterns connected together
;;; by 'not and 'and.
;;; Mental conditions can be of two types patterns and expressions.
;;;   - A PATTERN is matched in the typical unification style with the
;;;     database.
;;;   - An RELOP is is an numerical constraint on the
;;;     values of the variables whose values are bound in patterns.
;;;     If ?A is bound to 5 and ?B 6, then ?A < ?B would succeed.  For
;;;     simplicity of implementation non of the variables mentioned in
;;;     an RELOP can be free.  They must be bound by a previous
;;;     unification of a pattern.

(defstruct (mental-condition (:type list) :named)
  type
  pattern
  operation
  expression1
  expression2
  )

(defstruct (fact-pattern (:type list) :named)
  time
  pred)

(defmacro mental-condition-pattern-p (mental-condition)
  `(eq 'pattern (mental-condition-type ,mental-condition)))
(defmacro mental-condition-relop-p (mental-condition)
  `(eq 'relop (mental-condition-type ,mental-condition)))

