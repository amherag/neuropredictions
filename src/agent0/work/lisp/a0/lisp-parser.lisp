
;;;  LISP-PARSER
;;;
;;;  A parser for AOP facts, actions, programs, etc. which expects the
;;;  input string to be in Lisp format.


(setq *defining-an-agent* nil)

(defmacro defagent (name &rest keys)
  `(defagent-internal ',name ,@keys))

(defun defagent-internal (name &key (timegrain 10)
			       (beliefs) (commit-rules))
  (format t "Defining agent ~A~%" (string-upcase name))
  (setq *defining-agent-name* name)
  (setq *defining-an-agent* t)
  (setq *defining-agent* (make-agent :name name))
  (lisp-parse-beliefs beliefs)
  (setf (agent-commit-rules *defining-agent*)
	(lisp-parse-commit-rules commit-rules))
  (new-agent *defining-agent*)
  (setq *defining-an-agent* nil)
  (setq *current-agent* (name-agent name))
  (setq *defining-agent* nil))


;;;  LISP-PARSE-BELIEFS
;;;
;;;  Parse a set of initial beliefs, which are just fact-patterns.

(defun lisp-parse-beliefs (beliefs)
  (mapcar #'(lambda (x) (beliefs-add-fact
			 (lisp-parse-fact x)
			 *defining-agent*))
	  beliefs))


;;;  LISP-PARSE-COMMIT-RULES
;;;
;;;  Parse a set of commit rules in my lisp-like AOP syntax.  The
;;;  syntax looks like this:
;;;
;;;    '((((?agent1 REQUEST (DO ?time (becool))))) ; Message condition
;;;       ((B (?time (i_am_cool))))		; Mental condition
;;;       ?agent1				; Commit to agent who requested
;;;       (do ?time (becool)))		        ; to perform my private action

(defun lisp-parse-commit-rules (commit-rules)
  (mapcar #'lisp-parse-commit-rule commit-rules))

(defun lisp-parse-commit-rule (commit-rule)
  (make-commit-rule
   :msgcond (lisp-parse-msgcond (first commit-rule))
   :mntlcond (lisp-parse-mntlcond (second commit-rule))
   :agent-name (lisp-parse-agent (third commit-rule))
   :action (lisp-parse-act (fourth commit-rule))))


;;;  LISP-PARSE-MSGCOND
;;;
;;;  Returns a DNF representation of the msgcond passed into it

(defun lisp-parse-msgcond (msgcond)
  (cond ((null msgcond)
	 '((())))
	((eq (first msgcond) 'or)
	 (mapcar #'lisp-parse-msg-dnf (rest msgcond)))
	((eq (first msgcond) 'and)
	 (list (mapcar #'lisp-parse-1-msgcond (rest msgcond))))
	((eq (first msgcond) 'not)
	 (list (list (list 'not (lisp-parse-1-msgcond msgcond)))))
	(t (list (list (lisp-parse-1-msgcond msgcond))))))

(defun lisp-parse-msg-dnf (msgcond)
  (cond ((eq (first msgcond) 'and)
	 (list (mapcar #'lisp-parse-1-msgcond (rest msgcond))))
	((eq (first msgcond) 'not)
	 (list (list 'not (lisp-parse-1-msgcond (second msgcond)))))
	(t (list (lisp-parse-1-msgcond msgcond)))))

(defun lisp-parse-1-msgcond (msgcond)
  (cond ((eq (first msgcond) 'not)
	 (list 'not (lisp-parse-1-msgcond (second msgcond))))
	((eq (second msgcond) 'inform)
	 (make-message-condition
	  :sender (lisp-parse-agent (first msgcond))
	  :type 'inform
	  :content (lisp-parse-fact (third msgcond))))
	(t
	 (make-message-condition
	  :sender (lisp-parse-agent (first msgcond))
	  :type (second msgcond)
	  :content (lisp-parse-act (third msgcond))))))
	 

;;;  LISP-PARSE-MNTLCOND
;;;
;;;  Returns a DNF representation of the mntlcond passed into it

(defun lisp-parse-mntlcond (mntlcond)
  (cond ((null mntlcond) '((())))
	((eq (first mntlcond) 'or)
	 (mapcar #'lisp-parse-mntl-dnf (rest mntlcond)))
	((eq (first mntlcond) 'and)
	 (list (mapcar #'lisp-parse-1-mntlcond (rest mntlcond))))
	((eq (first mntlcond) 'not)
	 (list (list (list 'not (lisp-parse-1-mntlcond (second mntlcond))))))
	(t (list (list (lisp-parse-1-mntlcond mntlcond))))))

(defun lisp-parse-mntl-dnf (mntlcond)
  (cond ((eq (first mntlcond) 'and)
	 (list (mapcar #'lisp-parse-1-mntlcond (rest mntlcond))))
	((eq (first mntlcond) 'not)
	 (list (list 'not (lisp-parse-1-mntlcond (second mntlcond)))))
	(t (list (lisp-parse-1-mntlcond mntlcond)))))

(defun lisp-parse-1-mntlcond (mntlcond)
  (cond ((eq (first mntlcond) 'not)
	 (list 'not (lisp-parse-1-mntlcond (second mntlcond))))
	((eq (first mntlcond) 'B)
	 (make-mental-condition
	  :type 'pattern
	  :pattern (lisp-parse-fact-pattern (second mntlcond))))
	((eq (first mntlcond) 'CMT)
	 (make-mental-condition
	  :type 'pattern
	  :pattern (make-cmt :to-agent (lisp-parse-agent (second
							  mntlcond))
			     :from-agent *defining-agent-name*
			     :action (lisp-parse-act (third
						      mntlcond)))))
	(t (make-mental-condition
	    :type 'relop
	    :operation (first mntlcond)
	    :expression1 (second mntlcond)
	    :expression2 (third mntlcond)))))


(defun lisp-parse-var (var)
  (if (and (symbolp var) (eq (aref (symbol-name var) 0) #\?))
      (make-var :name (intern (subseq (symbol-name var) 1)))
      var))


;;;  LISP-PARSE-STRING
;;;
;;;  Parses a string, a fact or an act, in my Lisp-like AOP syntax.

(defun lisp-parse-string (type string)
  (cond ((string-equal type "fact")
	 (lisp-parse-fact (read-from-string string)))
	((string-equal type "act")
	 (lisp-parse-act (read-from-string string)))
	(t nil)))
	   

(defun lisp-parse-fact (fact)
  (let ((pred (second fact)))
    (make-fact-pattern
     :time (lisp-parse-time (first fact))
     :pred (or (case (first pred)
		 (B (make-belief :agent (lisp-parse-agent (second pred))
				  :fact (lisp-parse-fact (third pred))))
		 (CMT (make-cmt :to-agent (lisp-parse-agent (second pred))
				 :from-agent (lisp-parse-agent (third pred))
				 :action (lisp-parse-act (fourth pred))))
		 (CAN (make-can :agent (lisp-parse-agent (second pred))
				 :stuff (lisp-parse-act (third pred)))))
	       (lisp-parse-pred pred)))))

(defun lisp-parse-time (time)
  (if *defining-an-agent*
      (lisp-parse-pred time)
      (eval (lisp-parse-pred time))))

(defun lisp-parse-pred (pred)
  (if *defining-an-agent*
      (lisp-replace-vars pred)
    (replace-now pred (now))))		;  replace-now is defined in
					;  structures.lisp


(defun lisp-replace-vars (pred)
  (when pred
    (if (atom pred)
	(lisp-parse-var pred)
	(cons (lisp-replace-vars (car pred))
	      (lisp-replace-vars (cdr pred))))))

(defun lisp-parse-agent (agent)
  (lisp-parse-var agent))

(defun lisp-parse-act (act)
  (or
   (case (first act)
     (do (make-action :type 'do
		       :time (lisp-parse-time (second act))
		       :private-action (lisp-parse-privateact (third act))))
     (inform (make-action :type 'inform
			   :time (lisp-parse-time (second act))
			   :agent (lisp-parse-agent (third act))
			   :fact (fourth act)))	; Don't parse it yet
     ((request unrequest)
      (make-action :type (first act)
		   :time (lisp-parse-time (second act))
		   :agent (lisp-parse-agent (third act))
		   :action (fourth act)))
     (refrain (let ((subact (lisp-parse-act (second act))))
		 (make-action :type 'refrain
			      :time (action-time subact)
			      :action subact)))
     (if (make-action :type 'if
		       :mntlcond (lisp-parse-mntlcond (second act))
		       :action (lisp-parse-act (third act)))))
   (error "Parse-act: Unknown action type ~A" (first act))))


(defun lisp-parse-fact-pattern (fact-pattern)
  (lisp-parse-fact fact-pattern))


(defun lisp-parse-privateact (privateact)
  (make-private-action
   :fn (lisp-replace-vars privateact)))
