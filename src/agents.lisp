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

(defparameter *pop* '((1 2 3) (4 4 1) (1 1 1) (3 1 2)))
;; (fitness '(1 2 3))
;; (selectone (distribution *pop*))
;; (reproduce *pop*)

;; (defclass broker ())

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
			      (let ((r (random-int *rand-gen* 0 1)))
				(cond ((= r 0) (round-to (+ fibo (/ (random-int *rand-gen* 0 5) 100)) 3))
				      ((= r 1) (round-to (- fibo (/ (random-int *rand-gen* 0 5) 100)) 3))
				      )))
			    (take (1+ (random-int *rand-gen* 0 (1- (length options))))
				  (shuffle options)))))
	     (shuffle (concatenate 'list (make-list (- (length options) (length nums))) nums))
	     ))
	 (iota n))
    ))

(defun gen-rules (num-agents num-rules)
  (let ((opts (shuffle
	       (reduce #'append (map (lm (pi)
				       (map (lm (mean)
					      (list mean pi))
                                            '(0.0 50.0 100.0)))
                                     '(0.0 0.15 0.3))))))
    (map (lm (_)
	   (map (lm (_)
		  (map (lm (_) (nth opts (random-int *rand-gen* 0 (1- (length opts))))) (iota 4)))
		(iota num-rules)))
	 (iota num-agents))
    ))

(defclass agent ()
  ((beliefs :initarg :beliefs :initform (gen-beliefs) :accessor beliefs)
   (rules :initarg :rules :initform (gen-rules 10) :accessor rules)))

(defparameter *num-agents* 4)
(defparameter *num-rules* 4)

(defclass agents ()
  ((beliefs :initarg :beliefs :initform (gen-beliefs *num-agents*) :accessor beliefs)
   (rules :initarg :rules :initform (gen-rules *num-agents* *num-rules*) :accessor rules)))

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

(defun agents-reproduce (population)
  (let ((offspring nil)
        (d (agents-distribution population)))
    (dotimes ;; (i (/ (length population) 2))
	(i 1)
      (let ((x (agents-selectone d))
            (y (agents-selectone d)))
        (agents-crossover x y)
        (setq offspring (nconc (list x y) offspring))
	))
    offspring))

;; here
;; (defparameter *agents* (list (make-instance 'agents) (make-instance 'agents) (make-instance 'agents) (make-instance 'agents)))
;; (agents-selectone (agents-distribution *agents*))
;; (agents-distribution *agents*)
;; (agents-reproduce *agents*)
;; (pprint (slot-value (nth *agents* 0) 'beliefs))

;; (nconc (list (make-instance 'agents) (make-instance 'agents)) (list (make-instance 'agents) (make-instance 'agents)))
;; (agents-reproduce (list (make-instance 'agents) (make-instance 'agents) (make-instance 'agents) (make-instance 'agents)))

(defun agents-distribution (population)
  "Done."
  (let* ((fitnesses (map #'agents-fitness population))
	 (sum (apply #'+ fitnesses)))
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

(defun agents-crossover (x y &optional (chance 0.6))
  "Done."
  (let ((n-agents (length (slot-value x 'rules)))
	(n-rules (length (first (slot-value x 'rules))))
	(n-beliefs (length (first (slot-value x 'beliefs))))
	(x-beliefs (apply #'nconc (slot-value x 'beliefs)))
	(x-rules (apply #'nconc (apply #'nconc (slot-value x 'rules))))
	(y-beliefs (apply #'nconc (slot-value y 'beliefs)))
	(y-rules (apply #'nconc (apply #'nconc (slot-value y 'rules)))))
    (if (> (random-float *rand-gen* 0 1.0) chance)
	nil
	(progn
	  (let* ((site (random-int *rand-gen* 0 (- (length x-beliefs) 1)))
		 (swap (rest (nthcdr x-beliefs site))))
	    (setf (rest (nthcdr x-beliefs site)) (rest (nthcdr y-beliefs site))) 
	    (setf (rest (nthcdr y-beliefs site)) swap))
	  (let* ((site (random-int *rand-gen* 0 (- (length x-rules) 1)))
		 (swap (rest (nthcdr x-rules site))))
	    (setf (rest (nthcdr x-rules site)) (rest (nthcdr y-rules site))) 
	    (setf (rest (nthcdr y-rules site)) swap))))

    (setf (slot-value x 'beliefs) (group n-beliefs x-beliefs))
    (setf (slot-value x 'rules) (group n-agents (group n-rules x-rules)))
    (setf (slot-value y 'beliefs) (group n-beliefs y-beliefs))
    (setf (slot-value y 'rules) (group n-agents (group n-rules y-rules)))
    )
  )

;; (pprint (let ((x (make-instance 'agents))
;; 	      (y (make-instance 'agents)))
;; 	  (print (slot-value x 'beliefs))
;; 	  (print (length (alexandria:random-elt (slot-value x 'beliefs))))
;; 	  (agents-crossover x y)
;; 	  (agents-crossover x y)
;; 	  (agents-crossover x y)
;; 	  (agents-crossover x y)
;; 	  (agents-crossover x y)
;; 	  (agents-crossover x y)
;; 	  (print (slot-value x 'beliefs))
;; 	  (reduce #'+ (map (lambda (elt) (length elt)) (slot-value x 'rules)))))

;; (pprint (slot-value (make-instance 'agents) 'beliefs))

;; (apply #'nconc (slot-value (make-instance 'agents) 'beliefs))
;; (pprint (apply #'nconc (apply #'nconc (slot-value (make-instance 'agents) 'rules))))
;; (agents-crossover (make-instance 'agents) (make-instance 'agents))

(defun agents-mutate (agents &optional (chance 0.03))
  "Done."
  (setf (slot-value agents 'beliefs)
	(map (lambda (agent-beliefs)
	       (if (> (random-float *rand-gen* 0 1.0) chance)
		   agent-beliefs
		   (first (gen-beliefs 1)))
	       ) 
	     (slot-value agents 'beliefs)))
  (setf (slot-value agents 'rules)
	(map (lambda (agent-rules)
	       (map (lambda (rule)
		      (map (lambda (params)
			     (if (> (random-float *rand-gen* 0 1.0) chance)
				 params
				 (list (random-float *rand-gen* 0 100.0) (random-float *rand-gen* 0 0.3))))
			   rule)
		      )
		    agent-rules)) 
	     (slot-value agents 'rules)))
  agents)

;; (print (let ((agents (make-instance 'agents)))
;; 	 (print (slot-value agents 'beliefs))
;; 	 (agents-mutate agents)
;; 	 (print (slot-value agents 'beliefs))
;; 	 nil))

(defun agents-fitness (agents)
  "Done."
  (handler-case (let* ((all-levels (map (lambda (belief) (remove nil belief)) (slot-value agents 'beliefs)))
		       (data (map (lambda (levels)
				    (map (lambda (heat)
  				    	   ;; (let ((heat (gethash heat :heat))
  				    	   ;; 	 (close (gethash heat :close)))
  				    	   ;;   (let ((z (gethash heat :z))
  				    	   ;; 	   (index (search `(,close) (gethash heat :y)
  				    	   ;; 			  :key (lm (elt)
  				    	   ;; 				 (when (> close elt) t)))))
  				    	   ;;     (list (nth z (if (< (- index 2) 0) 0 (- index 2)))
  				    	   ;; 	     (nth z (if (< (- index 1) 0) 1 (- index 1)))
  				    	   ;; 	     (nth z index))))
				    	   )
  				    	 ;; (get-data :EUR_USD *rates* :levels levels)
					 (get-data :EUR_USD *rates*)
					 )
				    )
				  all-levels)))
		  ;; (let ((sim (cl-json:decode-json-from-string
  		  ;; 	      (dex:post "http://localhost:5000/ifis-agents"
  		  ;; 			;; :headers '(("Content-Type" . "application/json"))
  		  ;; 			:content `(("inputs". ,(cl-json:encode-json-to-string
  		  ;; 						data))
  		  ;; 				   ("rules" . ,(cl-json:encode-json-to-string (slot-value agents 'rules))))
  		  ;; 			))))
		  ;;   (format t "~a" sim)
		  ;;   (mse sim (get-real-data))
		  ;;   )
		  )
    (error (c)
      (format t "Condition: ~a.~%Error in agents: ~a ~%~% ~a.~&" c (slot-value agents 'beliefs) (slot-value agents 'rules))
      100)
    )
  )

;; (doeach (x (iota 100))
;;   (get-data :EUR_USD *rates*))

;; (defparameter *agents* (make-instance 'agents))
;; (agents-fitness *agents*)

(defun get-real-data ()
  (reverse (subseq (reverse (map (lambda (rate) (cdr (assoc :close-bid rate))) *rates*)) 0 100)))

(defun mse (series1 series2)
  (/ (reduce #'+ (map (lambda (elt) (expt elt 2)) (map #'- series1 series2)))
     (length series1)))





;; good one
;; (let ((agents (make-instance 'agents)))
;;   (map (lambda (beliefs rules)
;;   	 (let* ((levels (remove nil beliefs))
;;   		(data (get-data :EUR_USD *rates* :levels levels)))
;;   	   (dex:post "http://localhost:5000/ifis-agents"
;;   		     ;; :headers '(("Content-Type" . "application/json"))
;;   		     :content `(("inputs". ,(cl-json:encode-json-to-string
;;   					     (map (lm (heat)
;;   						    (let ((heat (gethash heat :heat))
;;   							  (close (gethash heat :close)))
;;   						      (let ((z (gethash heat :z))
;;   							    (index (search `(,close) (gethash heat :y)
;;   									   :key (lm (elt)
;;   										  (when (> close elt) t)))))
;;   							(list (nth z (if (< (- index 2) 0) 0 (- index 2)))
;;   							      (nth z (if (< (- index 1) 0) 1 (- index 1)))
;;   							      (nth z index)))))
;;   						  data)))
;;   				("rules" . ,(cl-json:encode-json-to-string rules)))
;;   		     )
;;   	   )
;;   	 )
;;        (slot-value agents 'beliefs)
;;        (slot-value agents 'rules))
;;   )
