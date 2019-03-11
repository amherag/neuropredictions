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

(defun combinations (&rest lists)
  (if (endp lists)
      (list nil)
      (mapcan (lambda (inner-val)
		(map (lambda (outer-val)
		       (cons outer-val
			     inner-val))
		     (car lists)))
	      (apply #'combinations (cdr lists)))))

(defun access (item alist)
  (second (assoc item alist)))

;; (access :profit '((:meow 5) (:profit (1 31 12))))

(defun shuffle (sequence)
  (loop for i from (length sequence) downto 2
     do (rotatef (elt sequence (random-int *rand-gen* 0 (1- i)))
                 (elt sequence (1- i))))
  sequence)

(defun round-to (number precision &optional (what #'round))
  (float (let ((div (expt 10 precision)))
           (/ (funcall what (* number div)) div))))



(map (lambda (price rprice)
       (map (lm (fib)
	      (+ rprice (* fib (- rprice price))))
	    '(0.236 0.382 0.618)))
     '(1.35 1.36 1.34 1.35)
     (rest '(1.35 1.36 1.34 1.35)))

(map (lambda (price)
       (map (lm (fib)
	      (* fib price))
	    '(0.236 0.382 0.618)))
     '(1.35 1.36 1.34 1.35))



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

;; (defparameter *rates* (get-rates :EUR_USD 1 :H1))

;; (defparameter *AUD_USD* (get-rates :AUD_USD 2 :H1))
;; (defparameter *EUR_GBP* (get-rates :EUR_GBP 2 :H1))
;; (defparameter *EUR_JPY* (get-rates :EUR_JPY 2 :H1))
;; (defparameter *EUR_USD* (get-rates :EUR_USD 2 :H1))
;; (defparameter *GBP_USD* (get-rates :GBP_USD 2 :H1))
;; (defparameter *USD_CAD* (get-rates :USD_CAD 2 :H1))
;; (defparameter *USD_JPY* (get-rates :USD_JPY 2 :H1))

;; market data
;; beginning training: Friday, January 4, 2019 1:00:00 AM GMT-08:00
;; end training: Sunday, February 3, 2019 8:00:00 PM GMT-08:00
;; beginning testing: Sunday, February 3, 2019 9:00:00 PM GMT-08:00
;; end testing: Monday, March 4, 2019 4:00:00 PM GMT-08:00

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
	 (d (agents-distribution population))
	 (fitness (multiple-value-bind (_ f) (agents-distribution population)
		    f))
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
    (values
     (if best-removed?
	 ;; (= (agents-fitness (agents-distribution population))
	 ;; 	(agents-fitness ))
	 (append (list (make-instance 'agents
				      :beliefs best-beliefs
				      :rules best-rules))
		 (agents-without-worst (agents-distribution population)))
	 population)
     fitness)
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

(defun write-to-file (filename content)
  (with-open-file (str filename
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format str content)))

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

(defun agents-do-all ()
  (doeach (config (combinations '(4 10 20) '(4 10 20) '(4 10 20)
				'("aud_usd" "eur_gbp" ;; "eur_jpy" "usd_jpy"
				  "eur_usd" "gbp_usd" "usd_cad")))

    (defparameter *file-name* (format nil "results/~a_h1-~aagents-~arules-~aind-100gen"
				      (nth config 3) (nth config 0) (nth config 1) (nth config 2)))
  
    (when (not (probe-file
		(format nil "~a_population.dat"
			*file-name*
			(nth config 3) (nth config 0) (nth config 1) (nth config 2))))
    
      (format t "Running ~s~%" *file-name*)
    
      (defparameter *cached-agents* (make-hash-table :test #'equal))
      (defparameter *num-agents* (nth config 0))
      (defparameter *num-rules* (nth config 1))
      (defparameter *population* (map (lm (_) (make-instance 'agents)) (iota (nth config 2))))
  
      (defparameter *fitnesses* nil)
  
      (defparameter *error* nil)
      (defparameter *sim* nil)
      (defparameter *profits* nil)

      ;; setting training data-set
      (defparameter *rates* (subseq (ms:unmarshal (read-from-string (file-get-contents (concatenate 'string "data/" (nth config 3) ".dat")))) 0 500))

      (doeach (x (iota 100))
	(multiple-value-bind (agents fitness)
	    (agents-reproduce *population*)
	  (setf *population* agents)
	  (push fitness *fitnesses*)))

      (defparameter *best-agent* (agents-best (agents-distribution *population*)))

      ;; extracting report data for training stage
      (multiple-value-bind (err sim profits)
	  (agents-fitness (agents-best (agents-distribution *population*)) t)
	(setf *error* err)
	(setf *sim* sim)
	(setf *profits* profits))

      ;; writing population
      (write-to-file (format nil "~a_population.dat" *file-name*)
		     (format nil "~s" (ms:marshal *population*)))

      ;; writing report for training stage
      (write-to-file (format nil "~a_training_report.csv" *file-name*)
		     (concatenate 'string (format nil "real,sim,profits,acc-profits,error~%")
				  (format nil "~{~a~%~}" (map (lambda (&rest rest)
								(format nil "~{~a~^,~}"rest))
							      (get-real-data)
							      *sim*
							      *profits*
							      (get-accumulation 0 *profits*)
							      (reverse *fitnesses*))
					  )))
      ;; writing training error
      (write-to-file (format nil "~a_training_error.txt" *file-name*)
		     (format nil "~s" *error*))

      ;; writing training interpretation
      (write-to-file (format nil "~a_training_interpretation.txt" *file-name*)
		     (agents-describe *best-agent*))

      ;; =========================
      ;; setting testing dataset
      (defparameter *rates* (subseq (ms:unmarshal (read-from-string (file-get-contents (concatenate 'string "data/" (nth config 3) ".dat")))) 100 600))

      ;; extracting report data for testing stage
      (multiple-value-bind (err sim profits)
	  (agents-fitness (agents-best (agents-distribution *population*)) t)
	(setf *error* err)
	(setf *sim* sim)
	(setf *profits* profits))
    
      ;; writing report for testing stage
      (write-to-file (format nil "~a_testing_report.csv" *file-name*)
		     (concatenate 'string (format nil "real,sim,profits,acc-profits~%")
				  (format nil "~{~a~%~}" (map (lambda (&rest rest)
								(format nil "~{~a~^,~}"rest))
							      (get-real-data)
							      *sim*
							      *profits*
							      (get-accumulation 0 *profits*))
					  )))
      ;; writing testing error
      (write-to-file (format nil "~a_testing_error.txt" *file-name*)
		     (format nil "~s" *error*))

      ;; writing testing interpretation
      (write-to-file (format nil "~a_testing_interpretation.txt" *file-name*)
		     (agents-describe *best-agent*))
      )
    ))

;; (agents-do-all)

;; here
;; (defparameter *population* (map (lm (_) (make-instance 'agents)) (iota 4)))
;; (time (setf *population* (agents-reproduce *population*)))
;; (time (doeach (x (iota 100)) (when *continue?* (setf *population* (agents-reproduce *population*)))))
;; (time (agents-distribution *population*))
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
;;;; #Agents seeing what
;;; Profit plots
;;;; Real - Simulated, per agent, as a whole
;;;; How many agents were profitable
;; Forecasting
;;; Interpretation
;;; Profit plots
;;;; Real - Simulated, per agent, as a whole

;; What to save (serialize)
;; Population
;; Market rates (half for training, half for testing)
;=== to automate ====
;; Real rates (training 500) ××
;; simulated rates (training)
;; Profit at each trade at training (defun dir)
;; Real rates (testing 500)
;; Simulated rates (testing)
;; Profit at each trade at testing (defun dir)
;; 
;; Error minimization (they are 100)
;; Error at training
;; Error at testing (defun mse)

;; What to test
;; Agents: 4, 10, 20
;; Rules: 2, 4, 10
;; Individuals: 4, 10, 20
;; Markets: AUD_USD, EUR_GBP, EUR_JPY, EUR_USD, GBP_USD, USD_CAD, USD_JPY

;; Generations: 100 (the point is to reach certain curve-fit and that's it)
;; Timeframe: 1H (the point is to demonstrate, not to find where it's better)

(defun agents-distribution (population)
  "Done."
  (let* ((fitnesses (pmapcar #'agents-fitness population))
	 (sum (apply #'+ fitnesses)))
    (values
     (map (lambda (fitness x)
	    (cons (/ fitness sum) x))
	  fitnesses
	  population)
     (first (sort (copy-seq fitnesses) #'<)))))

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

(defun agents-fitness (agents &optional (sim-and-dir? nil))
  "Done."
  (let ((sig (reduce #'+
		     (append (alexandria:flatten (slot-value agents 'beliefs))
			     (alexandria:flatten (slot-value agents 'rules))))))
    (if (and (not sim-and-dir?) (gethash *cached-agents* sig))
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
			  (let ((err (mse sim (get-real-data))))
			    (if (not sim-and-dir?)
				(setf (gethash *cached-agents* sig) err))
			    (if sim-and-dir?
				(values err
					;; simulated rates
					(let ((tmp (append (list (first sim)) sim)))
					  (reverse (rest (reverse tmp))))
					;; profit at each trade
					(append (list 0) (dir sim (get-real-data))))
				err))
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
		    all-levels))
	 (agents-profits (map (lambda (agent-data agent-rules)
				(let ((sim (cl-json:decode-json-from-string
					    (dex:post "http://localhost:5000/ifis-agents"
						      ;; :headers '(("Content-Type" . "application/json"))
						      :content `(("inputs". ,(cl-json:encode-json-to-string
									      (list agent-data)))
								 ("rules" . ,(cl-json:encode-json-to-string (list agent-rules))))
						      ))))
				  (reduce #'+ (dir sim (get-real-data)))
				  ))
			      data
			      (slot-value agents 'rules)
			      ))
	 (agents-perception (let ((simp-fibos (map (lambda (agent-data)
						     (let* ((data-no-pi (map (lambda (elt) (subseq elt 0 3)) agent-data))
							    (mx (apply #'max (alexandria:flatten data-no-pi)))
							    )
						       (map (lambda (datum)
							      (map (lambda (d)
								     (let ((nd (/ d mx)))
								       (cond ((< nd (/ 1 3)) 0)
									     ((< nd (/ 2 3)) 1)
									     (t 2))))
								   datum))
							    data-no-pi)
						       ))
						   data)))
			      (map (lambda (agent-fibos)
				     (let ((above-high 0) (level-high 0) (below-high 0)
					   (above-med 0) (level-med 0) (below-med 0)
					   (above-low 0) (level-low 0) (below-low 0)
					   (lfib (length agent-fibos)))
				       (map (lambda (fibos)
					      (cond ((= (first fibos) 0) (incf above-low))
						    ((= (first fibos) 1) (incf above-med))
						    ((= (first fibos) 2) (incf above-high)))
			    
					      (cond ((= (second fibos) 0) (incf level-low))
						    ((= (second fibos) 1) (incf level-med))
						    ((= (second fibos) 2) (incf level-high)))
			    
					      (cond ((= (third fibos) 0) (incf below-low))
						    ((= (third fibos) 1) (incf below-med))
						    ((= (third fibos) 2) (incf below-high)))
					      )
					    agent-fibos)
				       (list (list (/ above-low lfib) (/ above-med lfib) (/ above-high lfib))
					     (list (/ level-low lfib) (/ level-med lfib) (/ level-high lfib))
					     (list (/ below-low lfib) (/ below-med lfib) (/ below-high lfib)))))
				   simp-fibos)
			      )))

    (let ((interprets (make-hash-table :test #'equal)))
      (map (lambda (agent-number agent-rules)
    	     (let ((dups (make-hash-table :test #'equal)))
    	       (map (lambda (agent-rule)
    		      (let ((simp-rule (map (lambda (clause)
    					      (let ((b (apply #'beta clause))
    						    (action (cond ((< (first clause) 33) 0) ;; sell
    								  ((< (first clause) 66) 1) ;; buy or sell a little
    								  (t 2) ;; buy
    								  )))

    						(list action b)
    						))
    					    agent-rule)))
    			(map (lambda (fibo-type clause)
    			       ;; buy; above; low fib
    			       (let ((key (list `(:action ,(first (last simp-rule)))
    						`(:fibo-type ,fibo-type)
    						`(:fibo-strength ,(first clause))))
    				     (num-agents )
    				     (pi-fibo (second clause))
    				     (pi-action (second (last simp-rule)))
    				     (profit (nth agents-profits agent-number))
    				     (perception (nth (nth (nth agents-perception agent-number)
							   fibo-type)
						      (first clause))))

    				 (when (gethash interprets key) ;; already exists, I need to average
    				   (setq pi-fibo (/ (+ (access :pi-fibo (gethash interprets key))
    						       pi-fibo)
    						    2)
    					 pi-action (/ (+ (access :pi-action (gethash interprets key))
    							 pi-action)
    						      2)
    					 profit (/ (+ (access :profit (gethash interprets key))
    						      profit)
    						   2)
					 perception (/ (+ (access :perception (gethash interprets key))
							  perception)
						       2)))
				 
    				 (if (gethash dups key)
    				     (setf (gethash interprets key)
    					   (list `(:num-agents ,(access :num-agents (gethash interprets key)))
    						 `(:pi-fibo ,pi-fibo)
    						 `(:pi-action ,pi-action)
						 `(:perception ,(float perception))
						 `(:profit ,profit)))
    				     (progn
    				       (setf (gethash dups key) t)
    				       (setf (gethash interprets key)
    					     (list `(:num-agents ,(if (gethash interprets key)
    								      (1+ (access :num-agents (gethash interprets key)))
    								      1))
    						   `(:pi-fibo ,pi-fibo)
    						   `(:pi-action ,pi-action)
						   `(:perception ,(float perception))
						   `(:profit ,profit)))
    				       )
    				     )
    				 )
    			       )
    			     (iota 3)
    			     (reverse (rest (reverse simp-rule))))
    			))
    		    agent-rules)))
    	   (iota (length (slot-value agents 'rules)))
    	   (slot-value agents 'rules))

      (format nil "~{~a~^~%~}"
	      (map (lambda (elt)
		     (let* ((key (car elt))
			    (body (cdr elt))
			    (num-agents (access :num-agents body))
			    (profit (round (* (access :profit body) 10000)))
			    (perception (round (* (access :perception body) 100)))
			    (fibo-strength (access :fibo-strength key))
			    (fibo-type (access :fibo-type key))
			    (pi-fibo (round-to (access :pi-fibo body) 3))
			    (action (access :action key))
			    (pi-action (round-to (access :pi-action body) 3))
			    )
		       (format nil "* ~a agents — with an average profit of ~a units — perceived in ~a% of the market that a ~a resistance ~a — with a hesitancy of ~a — is a signal to ~a — with a hesitancy of ~a."
			       num-agents
			       profit
			       perception
			       (cond ((= fibo-strength 0) "weak")
				     ((= fibo-strength 1) "moderate")
				     ((= fibo-strength 2) "strong"))
			       (cond ((= fibo-type 0) "above current price")
				     ((= fibo-type 1) "nearby current price")
				     ((= fibo-type 2) "below current price"))
			       pi-fibo
			       (cond ((= action 0) "sell")
				     ((= action 1) "hold the current position")
				     ((= action 2) "buy"))
			       pi-action
			       ))
		     )
		   (sort (copy-seq (alexandria:hash-table-alist interprets))
			 (lambda (elt1 elt2)
			   (if (= (access :num-agents elt1)
				  (access :num-agents elt2))
			       (> (access :profit elt1)
				  (access :profit elt2))
			       (> (access :num-agents elt1)
				  (access :num-agents elt2))))
			 )))
      
      )
    )
  )

;; here
;; (agents-describe (agents-best (agents-distribution *population*)))
;; (agents-describe (agents-extract (first *population*) 1))

;; (caar (slot-value (agents-extract (first *population*) 1) 'rules))

(defun beta (mu1 mu2 pi)
  (/ pi (sqrt (1+ (abs (- mu1 mu2))))))

;; (beta 99 90 0.5)
;; (beta 100 0 1.0)

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
;; (beta 0.3 99 99)

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

(defun get-accumulation (start vals)
  (let ((results `(,start)))
    (map (lambda (val)
	   (push (+ (first results) val) results))
	 vals)
    (rest (reverse results))))

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
