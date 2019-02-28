(cl21:in-package :cl21-user)
(defpackage neuropredictions.agents
  (:use :cl21
        :lparallel
        :neuropredictions.config
        :neuropredictions.predict
	;; :neuropredictions.ga
	:random-state)
  (:export :foo
           ))
(in-package :neuropredictions.agents)

(defun shuffle (sequence)
  (loop for i from (length sequence) downto 2
     do (rotatef (elt sequence (random-int *rand-gen* 0 (1- i)))
                 (elt sequence (1- i))))
  sequence)

(defun round-to (number precision &optional (what #'round))
  (float (let ((div (expt 10 precision)))
           (/ (funcall what (* number div)) div))))

(defun gen-beliefs (n)
  (let ((options '(0.236 0.382 0.5 0.618 1 1.618)))
    (map (lm (_)
	   (let ((nums (map (lm (fibo)
			      ;; (if (= (random-int *rand-gen* 0 1) 0)
			      ;;     (+ fibo (/ (random-int *rand-gen* 0 5) 100))
			      ;;     (- fibo (/ (random-int *rand-gen* 0 5) 100)))
			      
			      ;; (let ((r (random-int *rand-gen* 0 1)))
			      ;; 	(cond ((= r 0) (round-to (+ fibo (/ (random-int *rand-gen* 0 5) 100)) 3))
			      ;; 	      ((= r 1) (round-to (- fibo (/ (random-int *rand-gen* 0 5) 100)) 3))
			      ;; 	      ))
			      (round-to (float (/ (random-int *rand-gen* 0 200) 100)) 3)
			      )
			    (take (1+ (random-int *rand-gen* 0 (1- (length options))))
				  (shuffle options)))))
	     (shuffle (concatenate 'list (make-list (- (length options) (length nums))) nums))
	     ))
	 (iota n))
    ))

(defun gen-rules (num-agents num-rules)
  (let (;; (opts (shuffle
	;;        (reduce #'append (map (lm (pi)
	;; 			       (map (lm (mean)
	;; 				      (list mean pi))
        ;;                                     '(0.0 50.0 100.0)))
        ;;                              '(0.0 0.15 0.3)))))
	)
    ;; (map (lm (_)
    ;; 	   (map (lm (_)
    ;; 		  (map (lm (_) (nth opts (random-int *rand-gen* 0 (1- (length opts))))) (iota 4)))
    ;; 		(iota num-rules)))
    ;; 	 (iota num-agents))
    (map (lm (_)
	   (map (lm (_)
		  (map (lm (_) `(,(random-int *rand-gen* 0 100)
				  ,(random-int *rand-gen* 0 100)
				  ,(float (/ (random-int *rand-gen* 0 100) 100))))
		       (iota 4)))
		(iota num-rules)))
	 (iota num-agents))
    ))

;; (defclass agent ()
;;   ((beliefs :initarg :beliefs :initform (gen-beliefs) :accessor beliefs)
;;    (rules :initarg :rules :initform (gen-rules 10) :accessor rules)))

(defclass agents ()
  ((beliefs :initarg :beliefs :initform (gen-beliefs *num-agents*) :accessor beliefs)
   (rules :initarg :rules :initform (gen-rules *num-agents* *num-rules*) :accessor rules)))

(defmethod ms:class-persistent-slots ((self agents))
  '(beliefs rules))

;; (slot-value (make-instance 'agents) 'beliefs)

;; (gen-beliefs 10)
;; (print (gen-rules 5 10))

;; (quote
;;  (let ((agent (make-instance 'agents)))
;;    (print (cl-json:encode-json-to-string (slot-value (make-instance 'agents) 'beliefs)))
;;    (print (cl-json:encode-json-to-string (slot-value (make-instance 'agents) 'rules)))
;;    nil))

;; (get-data instrument data)

;; (dex:get "http://localhost:5000/ifis-agents")

;; (dex:post "http://localhost:5000/ifis-agents"
;;           :content '(("data". "hello")))

(defparameter *rates* (get-rates :EUR_USD 1 :H1))

;; (pprint (slot-value (make-instance 'agents) 'beliefs))
;; (pprint (slot-value (make-instance 'agents) 'rules))

;; (get-data :EUR_USD *rates* :levels (remove nil '(0.246 0.372 0.608 1.03 1.628)))
;; (gen-beliefs 10)

(defun group (n list)
  "Split LIST into a list of lists of length N."
  (declare (integer n))
  (when (zerop n)
    (error "Group length N shouldn't be zero."))
  (labels ((rec (src acc)
             (let ((rest (nthcdr src n)))
               (if (consp rest)
                   (rec rest (cons (subseq src 0 n) acc))
                   (nreverse (cons src acc))))))
    (when list
      (rec list nil))))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun agents-best (distribution)
  (cdar (sort (copy-seq distribution) #'< :key (lambda (elt) (first elt)))))

(defun agents-without-worst (distribution)
  (map #'cdr (cdr (sort (copy-seq distribution) #'> :key (lambda (elt) (first elt))))))

(defun agents-reproduce (population)
  (let* ((offspring nil)
	 (d (agents-distribution population t))
	 (best (agents-best d))
	 (best-beliefs (slot-value best 'beliefs))
	 (best-rules (slot-value best 'rules))
	 (best-removed? nil)
	 )
    (dotimes (i (/ (length population) 2))
      (let* ((x (agents-selectone d))
	     (y (agents-selectone d))
	     (crossover? (agents-crossover x y)))
	(when (and (equal x best) crossover?) (setq best-removed? t))
	(when (and (equal y best) crossover?) (setq best-removed? t))
	;; (agents-crossover x y)
	;; (setq offspring (nconc (list x y) offspring))
	;; (push x offspring)
	;; (push y offspring)
	))
    ;; (let ((best (agents-best d)))
    ;;   (if (find best offspring)
    ;; 	  offspring
    ;; 	  (append (list best) (agents-without-worst (agents-distribution offspring))))
    ;;   )
    ;; (pushnew (agents-best d) offspring)
    ;; offspring
    (if best-removed?
	;; (= (agents-fitness (agents-distribution population))
	;; 	(agents-fitness ))
	(append (list (make-instance 'agents
				     :beliefs best-beliefs
				     :rules best-rules))
		(agents-without-worst (agents-distribution population)))
	population)
    ;; (append (list (make-instance 'agents))
    ;; 	    (agents-without-worst (agents-distribution population)))
    )
  )

(defparameter *continue?* t)

;; 10, 4, 50
(defparameter *num-agents* 10)
(defparameter *num-rules* 2)
(defparameter *cached-agents* (make-hash-table :test #'equal))

(defun random-search (generations)
  (let ((best-fitness 100)
	(best-agents nil))
    (doeach (x (iota generations))
      (when *continue?*
       (let* ((agents (make-instance 'agents))
	     (fitness (agents-fitness agents nil)))
	(if (< fitness best-fitness)
	    (progn
	      (setq best-fitness fitness)
	      (setq best-agents agents)
	      (setf *random-best* agents)
	      (print best-fitness))
	    (print best-fitness))))
      )
    best-agents))

(defparameter *random-best* nil)
;; (agents-fitness *random-best* t)
;; (time (random-search 10))
;; (ms:marshal *random-best*)

(defun agents-extract (agents n)
  (make-instance 'agents
		 :beliefs (list (nth (slot-value agents 'beliefs) n))
		 :rules (list (nth (slot-value agents 'rules) n))))

(defun avg (lst)
  (/ (reduce #'+ lst) (length lst)))

(defun agent-uncertainty (agents)
  "Used for only one agent."
  (avg (map (lambda (rule)
	      ;; (avg
	      ;;  (map (lambda (r)
	      ;; 	      (second r))
	      ;; 	    rule))
	      (second (last rule))
	      )
	    (first (slot-value agents 'rules)))))

(defparameter *num-agents* 4)
(defparameter *num-rules* 4)
(defparameter *cached-agents* (make-hash-table :test #'equal))

;; get all the agents' uncertainty
;; (map (lambda (idx)
;;        (print (agent-uncertainty (agents-extract *random-best* idx))))
;;      (iota (length (slot-value *random-best* 'beliefs))))

;; (agents-fitness (agents-extract *random-best* 9) nil t)

;; (agents-fitness *random-best* nil #'dir)
;; (agents-fitness *random-best* nil #'mse)

;; (agents-fitness (first (slot-value (make-instance 'agents) 'rules)))

;; Uptrend:
;; 35% of the agents believe uptrend has a high resistance.
;; Stagnation:
;; 15% of the agents believe we're in a stagnation zone.
;; Downtrend:
;; 25% of the agents believe

;; here
;; (defparameter *population* (map (lm (_) (make-instance 'agents)) (iota 10)))
;; (time (setf *population* (agents-reproduce *population*)))
;; (time (doeach (x (iota 1000)) (when *continue?* (setf *population* (agents-reproduce *population*)))))
;; (time (agents-distribution *population* t))
;; (time (agents-fitness (agents-best (agents-distribution *population*)) t))
;; (format t "~{~a~%~}" (get-real-data))
;; (ms:marshal *population*)
;; (ms:marshal (get-real-data))

;; (setf *rates* (ms:unmarshal (read-from-string (file-get-contents "src/2-21-2019.rates"))))

;; (setf *random-best* (ms:unmarshal (read-from-string (file-get-contents "src/20agents-2rules-rand.pop"))))
;; (setf *random-best* (ms:unmarshal (read-from-string (file-get-contents "src/10agents-2rules-rand.pop"))))
;; (setf *random-best* (ms:unmarshal (read-from-string (file-get-contents "src/4agents-4rules-rand.pop"))))

;; (setf *population* (ms:unmarshal (read-from-string (file-get-contents "src/20agents-2rules-100ind-100gen.pop"))))
;; (setf *population* (ms:unmarshal (read-from-string (file-get-contents "src/10agents-2rules-100ind-100gen.pop"))))
;; (setf *population* (ms:unmarshal (read-from-string (file-get-contents "src/4agents-4rules-100ind-150gen.pop"))))


;; #Agents, #Rules, #Individuals, #Generations, #Markets
;;; Capacity of modelling (lower # of rules better results?)
;; Training charts
;;; Interpretation
;;; Profit plots
;;;; How many agents were profitable
;; Forecasting
;;; Interpretation
;;; Profit plots

(defun agents-distribution (population &optional (print-best? nil))
  "Done."
  (let* ((fitnesses (pmapcar #'agents-fitness population))
	 (sum (apply #'+ fitnesses)))
    (if print-best?
	(print (first (sort (copy-seq fitnesses) #'<))))
    (map (lambda (fitness x)
	   (cons (/ fitness sum) x))
	 fitnesses
	 population)))

;; (agents-fitness (make-instance 'agents))
;; (map #'agents-fitness (list (make-instance 'agents) (make-instance 'agents) (make-instance 'agents) (make-instance 'agents)))
;; (agents-distribution (list (make-instance 'agents) (make-instance 'agents) (make-instance 'agents) (make-instance 'agents)))

(defun agents-selectone (distribution)
     "Done"
     (let ((random (random-float *rand-gen* 0 1.0)) 
           (prob 0) 
           genotype) 
       (some (lambda (pair) 
	       (incf prob (car pair))
	       (if (> random prob)
		   nil
		   (setf genotype (cdr pair)))) 
	     distribution)
       (agents-mutate genotype)))

;; (agents-selectone (agents-distribution (list (make-instance 'agents) (make-instance 'agents) (make-instance 'agents) (make-instance 'agents))))

;; (append (subseq '(1 2 3 4) 0 2)
;; 	(nthcdr '(nil nil nil nil) 2))
;; (append 
;;  (nthcdr '(1 2 3 4) 3)
;;  (subseq '(nil nil nil nil) 0 3))

(defun agents-crossover (x y &optional (chance 0.6))
  "Done."
  (let ((n-agents (length (slot-value x 'rules)))
	(n-rules (length (first (slot-value x 'rules))))
	(n-beliefs (length (first (slot-value x 'beliefs))))
	(x-beliefs (apply #'append (slot-value x 'beliefs)))
	(x-rules (apply #'append (apply #'append (slot-value x 'rules))))
	(y-beliefs (apply #'append (slot-value y 'beliefs)))
	(y-rules (apply #'append (apply #'append (slot-value y 'rules)))))
    (if (<= (random-float *rand-gen* 0 1.0) chance)
	(progn
	  (let* ((site (random-int *rand-gen* 0 (length x-beliefs)))
		 ;; (swap (rest (nthcdr x-beliefs site)))
		 )
	    
	    ;;(setf (rest (nthcdr x-beliefs site)) (rest (nthcdr y-beliefs site)))
	    
	    ;;(setf (rest (nthcdr y-beliefs site)) swap)

	    (setf (slot-value x 'beliefs)
		  (group n-beliefs (append (subseq x-beliefs 0 site) (nthcdr y-beliefs site))))
	    (setf (slot-value y 'beliefs)
		  (group n-beliefs (append (nthcdr y-beliefs site) (subseq x-beliefs 0 site))))
	    )
	  (let* ((site (random-int *rand-gen* 0 (length x-rules)))
		 ;; (swap (rest (nthcdr x-rules site)))
		 )
	    ;; (setf (rest (nthcdr x-rules site)) (rest (nthcdr y-rules site)))
	    ;; (setf (rest (nthcdr y-rules site)) swap)

	    (setf (slot-value x 'rules)
		  (group n-rules (group n-agents (append (subseq x-rules 0 site) (nthcdr y-rules site)))))
	    (setf (slot-value y 'rules)
		  (group n-rules (group n-agents (append (nthcdr y-rules site) (subseq x-rules 0 site)))))
	    )))

    ;; (setf (slot-value x 'beliefs) (group n-beliefs x-beliefs))
    ;; (setf (slot-value x 'rules) (group n-agents (group n-rules x-rules)))
    ;; (setf (slot-value y 'beliefs) (group n-beliefs y-beliefs))
    ;; (setf (slot-value y 'rules) (group n-agents (group n-rules y-rules)))
    )
  )

;; (pprint (let ((x (make-instance 'agents))
;; 	      (y (make-instance 'agents)))
;; 	  ;; (print (slot-value x 'rules))
;; 	  ;; (print (length (alexandria:random-elt (slot-value x 'beliefs))))
;; 	  (print (reduce #'+ (map (lambda (elt) (length elt)) (slot-value x 'beliefs))))
;; 	  (agents-crossover x y)
;; 	  (agents-crossover x y)
;; 	  ;; (print (slot-value x 'rules))
;; 	  (reduce #'+ (map (lambda (elt) (length elt)) (slot-value x 'beliefs)))))

;; (pprint (slot-value (make-instance 'agents) 'beliefs))

;; (apply #'nconc (slot-value (make-instance 'agents) 'beliefs))
;; (pprint (apply #'nconc (apply #'nconc (slot-value (make-instance 'agents) 'rules))))
;; (agents-crossover (make-instance 'agents) (make-instance 'agents))

(defun agents-mutate (agents &optional (chance 0.005))
  "Done."
  (let (changed?)
    (setf (slot-value agents 'beliefs)
	  (map (lambda (agent-beliefs)
		 (if (> (random-float *rand-gen* 0 1.0) chance)
		     agent-beliefs
		     (progn
		       (setq changed? t)
		       (first (gen-beliefs 1))
		       ))
		 ) 
	       (slot-value agents 'beliefs)))
    (setf (slot-value agents 'rules)
	  (map (lambda (agent-rules)
		 (map (lambda (rule)
			(map (lambda (params)
			       (if (> (random-float *rand-gen* 0 1.0) chance)
				   params
				   (progn
				     (setq changed? t)
				     (caaar (gen-rules 1 1))
				     )))
			     rule)
			)
		      agent-rules))
	       (slot-value agents 'rules)))
    (values agents changed?))
  )

;; (let ((agents (make-instance 'agents)))
;; 	 ;; (print (slot-value agents 'beliefs))
;; 	 (agents-mutate agents 0.005)
;; 	 ;; (print (slot-value agents 'beliefs))
;; 	 ;; nil
;; 	 )

(defun agents-fitness (agents &optional (print-sim? nil) (print-dir? nil))
  "Done."
  (let ((sig (reduce #'+
		     (append (alexandria:flatten (slot-value agents 'beliefs))
			     (alexandria:flatten (slot-value agents 'rules))))))
    (if (and (not print-sim?) (gethash *cached-agents* sig))
	(gethash *cached-agents* sig)
	(handler-case (let* ((all-levels (map (lambda (belief) (remove nil belief)) (slot-value agents 'beliefs)))
			     (closes)
			     (data (map (lambda (levels)
					  (map (lambda (heat)
						 (let ((heat (gethash heat :heat))
						       (close (gethash heat :close)))
						   (let ((z (gethash heat :z))
							 (index (alexandria:if-let
								    ((res (search `(,close) (gethash heat :y)
										  :key (lm (elt)
											 (when (> close elt) t)))))
								  res
								  (1- (length (gethash heat :y)))
								  )))
						     (push close closes)
						     (list (nth z (if (< (- index 2) 0) 0 (- index 2)))
							   (nth z (if (< (- index 1) 0) 1 (- index 1)))
							   (nth z index)
							   close
							   )))
						 )
					       (get-data :EUR_USD *rates* :levels levels)
					       )
					  )
					all-levels)))
			(let ((sim (cl-json:decode-json-from-string
				    (dex:post "http://localhost:5000/ifis-agents"
					      ;; :headers '(("Content-Type" . "application/json"))
					      :content `(("inputs". ,(cl-json:encode-json-to-string
								      data))
							 ("rules" . ,(cl-json:encode-json-to-string (slot-value agents 'rules))))
					      ))))
			  (if print-sim?
			      (format t "~{~a~%~}" (append (list (first sim)) sim)))
			  (if print-dir?
			      (format t "~{~a~%~}" (dir sim (get-real-data))))
			  (let ((err (mse sim (get-real-data))))
			    (if (not print-sim?)
				(setf (gethash *cached-agents* sig) err))
			    err)
			  )
			)
	  (error (c)
	    ;; (format t "Condition: ~a.~%Error in agents: ~a ~%~% ~a.~&" c (slot-value agents 'beliefs) (slot-value agents 'rules))
	    ;; (format t "Condition: ~a~%" c)
	    100)
	  ))
    )
  )

(defun agents-describe (agents)
  (let* ((all-levels (map (lambda (belief) (remove nil belief)) (slot-value agents 'beliefs)))
	 (closes)
	 (data (map (lambda (levels)
		      (map (lambda (heat)
			     (let ((heat (gethash heat :heat))
				   (close (gethash heat :close)))
			       (let ((z (gethash heat :z))
				     (index (alexandria:if-let
						((res (search `(,close) (gethash heat :y)
							      :key (lm (elt)
								     (when (> close elt) t)))))
					      res
					      (1- (length (gethash heat :y)))
					      )))
				 (push close closes)
				 (list (nth z (if (< (- index 2) 0) 0 (- index 2)))
				       (nth z (if (< (- index 1) 0) 1 (- index 1)))
				       (nth z index)
				       close
				       )))
			     )
			   (get-data :EUR_USD *rates* :levels levels)
			   )
		      )
		    all-levels)))
    (let ((sim (cl-json:decode-json-from-string
		(dex:post "http://localhost:5000/ifis-agents"
			  ;; :headers '(("Content-Type" . "application/json"))
			  :content `(("inputs". ,(cl-json:encode-json-to-string
						  data))
				     ("rules" . ,(cl-json:encode-json-to-string (slot-value agents 'rules))))
			  ))))
      (list (length
	     (dir sim (get-real-data)))
	    (length sim)
	    (length (get-real-data))
	    (length (first data)))
      )
    )
  )

;; here
;; (agents-describe (agents-extract (first *population*) 1))

;; (caar (slot-value (agents-extract (first *population*) 1) 'rules))

(defun beta (pi mu1 mu2)
  (/ (* pi 100) (1+ (- mu1 mu2))))

;; (beta 0.1 99 60)

(defun interpret-indeterminacy (pi mu1 mu2)
  (let ((beta (beta pi mu1 mu2)))
    (cond ((= beta 0) "without hesitancy")
	((< beta 0.25) "little hesitancy")
	((< beta 0.5) "some hesitancy")
	((< beta 1.5) "moderate hesitancy")
	((< beta 3.0) "considerable hesitancy")
	((< beta 5.0) "high hesitancy")
	((<= beta 100.0) "very high hesitancy")
	(t "unkown hesitancy"))))

;; (interpret-indeterminacy 0.3 99 30)

(defun interpret-proximity (diff)
  )

(defun interpret-transaction (mean)
  (cond ((= mean 0) "totally sure of a downtrend")
	((< mean 20) "sell ")
	((< mean 0.1) "some hesitancy")
	((< mean 0.15) "moderate hesitancy")
	((< mean 0.20) "considerable hesitancy")
	((< mean 0.25) "high hesitancy")
	((<= mean 0.3) "very high hesitancy")
	(t "unkown hesitancy")))

(defun interpret-consequent (ifs)
  (let ((diff (abs (- (nth ifs 0) (nth ifs 1)))))
    diff))

;; (agents-fitness (nth *population* 4))

;; (defparameter *agents* (make-instance 'agents))
;; (agents-fitness *agents*)

(defun get-real-data ()
  (reverse (subseq (reverse (map (lambda (rate) (cdr (assoc :close-bid rate))) *rates*)) 0 100)))

(defun mse (series1 series2)
  (/ (reduce #'+ (map (lambda (elt) (expt elt 2)) (map #'- series1 (rest series2))))
     (length series1)))

(defun dir (series1 series2)
  "Currently it returns the agent profit at each trade."
  (let ((sim series1)
	(real (rest series2))
	;; we only need the real previous, as the simulated is based on the last real price at every moment
	;; (prev (- (second series2) (first series2)))
	(prev (first series2))
	)
    (map (lambda (s r)
	   (let ((real-dir (- r prev))
		 (sim-dir (- s prev)))
	     (setq prev r)
	     (if (equal (plusp real-dir)
			(plusp sim-dir))
		 (abs sim-dir)
		 (* -1 (abs sim-dir))))
	   )
	 sim
	 real)
    ))
