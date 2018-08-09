;;;  AGENT0
;;;  An AOP interpreter.
;;;
;;;  Authors: Paul A. Viola and Mark C. Torrance
;;;  Date: Jan 10 1991


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  AOP-STEP
;;;
;;;  Loops through the agents and for each agent processes
;;;  one step

(defun aop-step (agents current-time)
  "For each agent recieve messages and process one step."
  (dolist (agent agents)
	  (setf (agent-messages agent)
		(agent-incoming-messages agent))
	  (setf (agent-incoming-messages agent) nil))
  (dolist (agent agents)
	  (agent-simulate-step agent current-time))
  (do-user-update agents))
  
(defun do-user-update (agents)
  )

(defun agent-simulate-step (agent current-time)
  "Process incoming messages, revoke and/or perform commitments."
  (message-process agent current-time)

  ;; (revoke-commitments agent capabilities)
  
  ;; Perform all the commitments of which you are capable that were
  ;;   to have been performed on this time step.
  (perform-commitments agent current-time)
  )
  

(defun message-process (agent current-time)
  "Updates beliefs, makes commitments and performs commitments."
  (dolist (message (agent-messages agent))
	  (when *show-msgs*
		(format t "~A ~AS ~A ~A"
			(message-sender message)
			(message-type message)
			(agent-name agent)
			(show-fact
			 (message-content message))))
	  (when (message-inform-p message)
		(beliefs-add-fact (message-content message)
				  ;;  message content is a fact-pattern
				  agent)))
  ;; Make commitments.
  (dolist (commit-rule (agent-commit-rules agent))
	  (commit-if-applicable commit-rule
				agent
				current-time)))

(defun perform-commitments (agent current-time)
  (dolist (fact (agent-beliefs agent))
	  (when (and (cmt-p (fact-pred fact))
		     (eq (second (fact-pred fact)) (agent-name agent))
		     (fact-tval-at-time fact current-time 't))
	    (perform-action (fact-pred fact) agent current-time))))

  
(defun perform-action (pred agent current-time)
  "Have AGENT perform ACTION."  
  (let ((action (cmt-action pred)))
    (cond ((action-if-p action)
	   (let ((bindings
		  (bind-mntl-dnf (action-mntlcond action)
				 agent
				 (agent-beliefs agent)
				 current-time)))
	     (when (not (equal 'fail bindings))
	       (perform-action (bond-action (action-action action)
					    bindings)
			       agent
			       current-time)
	       (uncommit agent (1+ current-time) pred))))
	  ;; These are #'> because we want to perform the action on the
	  ;; first tick after it matures. The efficiency of this could
	  ;; be improved.
	  ;; Ming and Robert suggest that we want >= so actions will
	  ;; be performed during the tick when the agent commits to
	  ;; them, if the actions are desired at that same time.  This
	  ;; means commitments will never remain true for immediate
	  ;; actions, so we hack the time of the uncommit to 1+
	  ;; current-time.

	  ((action-do-p action)
	   (when (time-compare current-time (action-time action) #'>=)
	     (perform-private-action (action-private-action action) agent)
	     (uncommit agent (1+ current-time) pred)))
	  ((action-inform-p action)
	   (when (time-compare current-time (action-time action) #'>=)
	     (send-message 'inform
			   (action-agent action)
			   (agent-name agent)
			   ;;  This used to parse the fact here, but
			   ;;  doesn't anymore.
			   (action-fact action))
	     (uncommit agent (1+ current-time) pred)))
	  ((action-request-p action)
	   (when (time-compare current-time (action-time action) #'>=)
	     (send-message 'request
			   (action-agent action)
			   (agent-name agent)
			   (action-action action))
	     (uncommit agent (1+ current-time) pred)))
	  ((action-unrequest-p action)
	   (when (time-compare current-time (action-time action) #'>=)
	     (send-message 'unrequest
			   (action-agent action)
			   (agent-name agent)
			   (action-action action))
	     (uncommit agent (1+ current-time) pred)))
	  ((action-refrain-p action)
	   ;; Don't "do" anything
	   ))))

(defun perform-private-action (private-action agent)
  (eval (private-action-fn private-action)))

(defun send-message (type to-agent-name from-agent-name content)
  (push (make-message :sender from-agent-name
		      :receiver to-agent-name
		      :type type
		      :content content)
	(agent-incoming-messages (name-agent to-agent-name))))

(defun uncommit (agent time pred)
  (belief-insert pred nil		; nil is the truth value
		 time agent))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- beliefs ---

;;;  BELIEFS is a list of FACTS


(defun beliefs-add-fact (fact-pattern	; includes time about which
					; is believed
			 agent
			 &key (truth-value (fact-pred-posp
					    (fact-pattern-pred fact-pattern))))
  (belief-insert (strip-nots (fact-pattern-pred fact-pattern))
		 truth-value
		 (fact-pattern-time fact-pattern)
		 agent))


;;; This function is used to insert new beliefs and commitment into
;;; the belief database.
;;;
;;; new-fact-pred has had the nots stripped from around it.
;;; truth-value may be true, nil, or unknown
;;; time is the time from which fact is believed with truth-value
;;; beliefs is the agent's beliefs.
;;;     It will be destructively modified.

(defun parser-add-belief (agent fact-pattern)
  "Helper function for the AOP parser."
  (beliefs-add-fact fact-pattern
		    agent))

(defun fact-truth-value (fact)
  "Tell whether the fact is negated or not."
  (fact-pred-posp (fact-pred fact)))

(defun belief-insert (new-fact-pred truth-value time agent)
  "Insert new beliefs into the beliefs database"
  (let* ((fact (car (member new-fact-pred (agent-beliefs agent)
			    :key #'fact-pred :test #'equal)))
	 (fact-status-list (and fact (fact-status fact)))
	 (new-fact-status (make-fact-status :time time
					    :truth-value truth-value)))

    (cond ((null fact)			; If no entry yet for this fact...
	   (push (make-fact :pred new-fact-pred
			    :status (list new-fact-status))
		 (agent-beliefs agent)))

	  ((time-compare		; if the new fact is newer than the 1st
	    time (fact-status-time (car fact-status-list)) #'>)
	   (push new-fact-status (fact-status fact)))

	  ;; We now know that the new fact is older than the first fact
	  ((= (length fact-status-list) 1) ; If there is only one belief
	   (if (time-compare
		time (fact-status-time (car fact-status-list)) #'=)
	       (setf (car fact-status-list) new-fact-status)
	       (setf (cdr fact-status-list) (list new-fact-status))))

	  (t				; We are inserting the new fact
	   (do* ((stati fact-status-list (cdr stati))
		 (rest-stati (cdr stati) (cdr stati))
		 (curr-status (cadr stati) (cadr stati)))
		((null rest-stati)
		 (setf (cdr stati) (list new-fact-status)))
	     (cond ((time-compare time (fact-status-time curr-status) #'=)
		    (setf (car rest-stati) new-fact-status)
		    (return nil))
		   ((time-compare time (fact-status-time curr-status) #'>)
		    (setf (cdr stati) (cons new-fact-status rest-stati))
		    (return nil))))))))

(defun bind-commit (commit-rule agent current-time)
  "Figures out if the messages and beliefs satisfy the commit-rule conditions
and returns the bindings for variables if it does."
  (if (and (commit-rule-msgcond commit-rule)
	   (not (equal (commit-rule-msgcond commit-rule) '((())))))
      (bind-msg-dnf (commit-rule-msgcond commit-rule)
		    (commit-rule-mntlcond commit-rule)
		    agent
		    nil
		    current-time)
    (bind-mntl-dnf (commit-rule-mntlcond commit-rule)
		   agent nil current-time)))

(defun bind-msg-dnf (msg-dnf mntl-dnf agent bindings current-time)
  (if msg-dnf
      (let ((new-bindings (bind-msg-conj (first msg-dnf) mntl-dnf
					 agent bindings current-time)))
        (if (eq new-bindings 'fail)
            (bind-msg-dnf (rest msg-dnf) mntl-dnf agent bindings
			  current-time)
            new-bindings))
      'fail))

(defun bind-msg-conj (conj mntl-dnf agent bindings current-time)
  (if (and conj (not (equal conj '(NIL))))
      (bind-msgcond (first conj) mntl-dnf (agent-messages agent)
		    agent conj bindings current-time)
      ;;  if it bound, return the binding, otherwise fail
      (bind-mntl-dnf mntl-dnf agent bindings current-time)))

(defun bind-msgcond (msgcond mntl-dnf messages agent conj bindings
			     current-time)
  (if (null msgcond)
      bindings
    (if messages
	(if (eq (first msgcond) 'not)
	    (let ((new-bindings
		   (condition-match-helper (second msgcond)
					   (first messages)
					   bindings)))
	      (if (eq new-bindings 'fail)
		  ;; Negative, but it didn't bind
		  (bind-msgcond msgcond mntl-dnf (rest messages)
				agent conj bindings current-time)
		;; Negative, and it bound
		'fail))
	  (let ((new-bindings
		 (condition-match-helper msgcond (first messages)
					 bindings)))
	    (if (eq new-bindings 'fail)
		;; Affirmative, but it didn't bind
		(bind-msgcond msgcond mntl-dnf (rest messages)
			      agent conj bindings current-time)
	      ;; Affirmative and it bound
	      (bind-msg-conj (rest conj) mntl-dnf agent new-bindings
			     current-time))))
      ;; Otherwise, we bound to no message
      (if (eq (first msgcond) 'not)
	  ;; If it's negated, succeed (forward-track)
	  (bind-msg-conj (rest conj) mntl-dnf agent bindings current-time)
	;; Otherwise, fail (back-track)
	'fail))))

;;;  BIND-MNTL-DNF
;;;
;;;  Recursively tries to bind the mental dnf to beliefs of the agent.

(defun bind-mntl-dnf (mntl-dnf agent bindings current-time)
  (if mntl-dnf
      (let ((new-bindings (bind-mntl-conj (first mntl-dnf) agent bindings
					  current-time)))
	(if (eq new-bindings 'fail)
	    (bind-mntl-dnf (rest mntl-dnf) agent bindings current-time)
	    new-bindings))
      'fail))

(defun bind-mntl-conj (conj agent bindings current-time)
  (if conj
      (bind-mntlcond (first conj) (agent-beliefs agent) agent conj bindings
		     current-time)
      ;;  If it bound, return the binding, otherwise fail
      bindings))			; Finally, succeed !!!

(defun bind-mntlcond (mntlcond beliefs agent conj bindings current-time)
  (if (null mntlcond) bindings
      (if beliefs
	  (if (eq (first mntlcond) 'not)
	      (let ((new-bindings (fact-unify (first beliefs)
					      (second mntlcond)
					      current-time
					      bindings)))
		(if (eq new-bindings 'fail)
		    ;; If we failed, then try to match against other beliefs
		    (bind-mntlcond mntlcond (rest beliefs) agent conj bindings
				   current-time)
		    ;; If we succeed, backtrack
		    'fail))
	      (let ((new-bindings (fact-unify (first beliefs)
					      mntlcond
					      current-time
					      bindings)))
		(if (eq new-bindings 'fail)
		    ;; Affirmative, but it didn't bind
		    (bind-mntlcond mntlcond (rest beliefs) agent conj bindings
				   current-time)
		    ;; Affirmative and it bound
		    (bind-mntl-conj (rest conj) agent new-bindings
				    current-time))))
	  ;; Otherwise, we bound to no belief
	  (if (eq (first mntlcond) 'not)
	      ;; If it's negated, succeed (forward-track)
	      (bind-mntl-conj (rest conj) agent bindings current-time)
	      ;; Otherwise, fail (back-track)
	      'fail))))


;;  GET-BELIEF
;;
;;  Aux function for George John to return a binding list for the
;;  match of a pattern against the beliefs of an agent.  Returns one
;;  binding list, for the first successful match, or 'fail if none
;;  found.

(defun get-belief (agent-name pattern &optional bindings)
  (let ((agent (name-agent agent-name)))
    (get-belief-aux (agent-beliefs agent) pattern bindings)))

(defun get-belief-aux (beliefs pattern &optional bindings)
  (if beliefs
      (let ((result (fact-unify (first beliefs)
				(make-mental-condition
				 :pattern (lisp-parse-pred pattern))
				(now)
				bindings)))
	(if (eq result 'fail)
	    (get-belief-aux (rest beliefs) pattern bindings)
	  result))
    'fail))

;;;  GET-ALL-BELIEFS
;;;
;;;  Aux function for George John to return a set of binding lists for
;;;  the matches of a pattern against the beliefs of an agent.
;;;  Returns a list of binding lists, each of which worked.

(defun get-all-beliefs (agent-name pattern &optional bindings)
  (let ((agent (name-agent agent-name))
	new-bindings
	result)
    (dolist (belief (agent-beliefs agent))
	    (setf new-bindings (fact-unify belief
					   (make-mental-condition
					    :pattern (lisp-parse-pred pattern))
					   (now)
					   bindings))
	    (if (not (eq new-bindings 'fail))
		(push new-bindings result)))
    result))
    


;;  Let's have this happen at the bottom of the recursion instead of
;;  at the top...  NEW IN PROGRESS

(defun execute-commit-rule (commit-rule agent current-time)
  (beliefs-add-fact
   (make-fact-pattern
    :pred (make-cmt :from-agent (agent-name agent)
		    :to-agent (unify-bond-expression
			       (commit-rule-agent-name commit-rule)
			       bindings)
		    :action (unify-bond-expression
			     (replace-now
			      (commit-rule-action commit-rule)
			      (now))
			     bindings))
    :time current-time)
   agent
   :truth-value t))
  

(defun commit-if-applicable (commit-rule agent current-time)
  (let ((bindings (bind-commit commit-rule agent current-time)))
    (when (not (eq bindings 'fail))
	  (beliefs-add-fact
	   (make-fact-pattern
	    :pred (make-cmt :from-agent (agent-name agent)
			    :to-agent (unify-bond-expression
				       (commit-rule-agent-name commit-rule)
				       bindings)
			    :action (unify-bond-expression
				     (replace-now
				      (commit-rule-action commit-rule)
				      (now))
				     bindings))
	    :time current-time)
	   agent
	   :truth-value t))))
	 
(defun condition-match-helper (message-condition message binding)
  (let ((augmented-binding nil))
    (setf augmented-binding
	  (unify (message-condition-sender message-condition)
		 (message-sender message) binding))
    (when (not (eq augmented-binding 'fail))
      (setf augmented-binding
	    (unify (message-condition-type message-condition)
		   (message-type message) augmented-binding)))
    (when (not (eq augmented-binding 'fail))
      (setf augmented-binding
	    (unify (message-condition-content message-condition)
		   (message-content message)
		   augmented-binding)))
    augmented-binding))

;;; Two commitments are in contradiction if they are identical or if one is
;;;   the REFRAIN of the other.
(defun cmt-contradict (fact1 fact2)
  ;; If the two cmt are in contradiction return t.
  (labels ((strip-refrains (fact-pred)
	     (cond ((null fact-pred)
		    fact-pred)
		   ((eq (car fact-pred) 'refrain)
		    (strip-nots (cadr fact-pred)))
		   (t
		    fact-pred))))
    (let ((fact1-pred (print (strip-refrains (fact-pred fact1))))
	  (fact2-pred (print (strip-refrains (fact-pred fact2)))))
      (equal fact1-pred fact2-pred))))

(defun make-commit-fact (from-agent-name to-agent-name action time)
  (make-fact :time time
	     :pred (make-cmt :from-agent from-agent-name
			     :to-agent to-agent-name
			     :stuff action)
	     :agent nil))




(defun fact-unify (fact mental-condition current-time bindings)
  "Takes a FACT and  pattern MENTAL-CONDITION and tries to
unify given the current bindings."
  (let ((pattern (mental-condition-pattern mental-condition)))

	   ;; If the the mental condition is about beliefs its pattern
	   ;; will be a fact pattern
    (cond ((fact-pattern-p pattern)
	   (let* ((pattern-pred (fact-pattern-pred pattern))
		  (pattern-truth-value (fact-pred-posp pattern-pred))
		  (pattern-time (unify-bond-expression
				 (eval
				  (replace-now
				   (fact-pattern-time pattern)
				   current-time)) bindings))
		  (augmented-bindings
		   (unify (fact-pred fact)
			  (strip-nots pattern-pred)
			  bindings)))
	     (when (var-p pattern-time)
	       (error "fact-unify: time in fact pattern not bound~%")) 
	     (if (and (not (eq augmented-bindings 'fail))
		      (fact-tval-at-time fact pattern-time
					 pattern-truth-value))
		 augmented-bindings
		 'fail)))
	  ;; If the the mental condition is about commitments its
	  ;; pattern will be a list
	  (t
	   (let* ((pattern-pred pattern)
		  (pattern-time current-time)
		  (augmented-bindings
		   (unify (fact-pred fact)
			  pattern-pred
			  bindings)))
	     (if (and (not (eq augmented-bindings 'fail))
		      (fact-tval-at-time fact pattern-time 't))
		 augmented-bindings
		 'fail))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; For the parser.
(defun <> (a b) (not (= a b)))

;;;(defun load-aop-parse ()
;;;  (print "Making tables .... ")
;;;  (paren:make-tables-from-source (strcat *aop-load-path* "aop.src"))
;;;  (print "initializing ... ")
;;;  (paren:initialize-parser (strcat *aop-load-path* "aop-tables.lisp")))

;;;(defun initialize-parsing ()
;;;  (load (strcat *aop-load-path* "aop-readt.lisp"))
;;;  (paren:set-paren-readtable paren:*aop-readtable*))
