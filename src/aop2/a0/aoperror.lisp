;;;Macintosh Allegro Coral Common Lisp specific error-catching
;;;routine.  Calls eval on the function.  If no error, then 
;;;prints the return of eval.  If an error, then prints the 
;;;error message.  Subtle bug: this won't work with multiple-
;;;valued functions like truncate, but that's not a problem in
;;;our current implementation
(defun aopeval (function)  
  (multiple-value-bind (ans err) (catch-error-quietly (eval function))
    (setq ans (if err (apply 'format nil ans) ans))
    ans))
  
(defun run-function (function)
  (prog1
    (aopeval function)
    (fresh-line)))

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

	(t (princ (run-function (read-from-string input)))
	   (terpri))

	)))

             