(cl21:in-package :cl21-user)
(defpackage neuropredictions.ga
  (:use :cl
	:random-state
	:neuropredictions.config)
  (:export :reproduce
	   :distribution
	   :selectone
	   :crossover
	   :mutate
	   :fitness
           ))
(in-package :neuropredictions.ga)

(defun reproduce (population) 
  (let ((offspring nil) 
        (d (distribution population))) 
    (dotimes (i (/ (length population) 2)) 
      (let ((x (selectone d)) 
            (y (selectone d))) 
        (crossover x y) 
        (setq offspring (nconc (list x y) offspring)))) 
    offspring))

(defun distribution (population) 
  (let* ((genotypes (remove-duplicates population :test #'equal)) 
  (sum (apply #'+ (mapcar #'fitness genotypes)))) 
    (mapcar #'(lambda (x) (cons (/ (fitness x) sum) x)) genotypes)))

(defun selectone (distribution) 
  (let ((random (random-float *rand-gen* 0 1.0)) 
         (prob 0) 
         genotype) 
    (some #'(lambda (pair) 
	      (incf prob (first pair)) 
	      (if (> random prob) nil 
		  ;;else 
		  (setq genotype (rest pair)))) 
	  distribution) 
    (mutate genotype)))

(defun crossover (x y) 
  (if (> (random-float *rand-gen* 0 1.0) 0.6) (list x y) 
      ;;else 
      (let* ((site (random-int *rand-gen* 0 (1- (length x)))) 
	     (swap (rest (nthcdr site x)))) 
	(setf (rest (nthcdr site x)) (rest (nthcdr site y))) 
	(setf (rest (nthcdr site y)) swap))))

(defun mutate (genotype) 
  (mapcar #'(lambda (x) 
	      (if (> (random-float *rand-gen* 0 1.0) 0.03) x 
		  ;; else 
		  (if (= x 1) 0 
		      ;; else 
		      1))) 
	  genotype))

(defun fitness (x) 
  (let ((xarg (/ (string2num x) 1073741823.0)) 
        (v '(0.5 0.25 1.0 0.25)) 
        (c '(0.125 0.375 0.625 0.875)) 
        (w 0.003)) 
    (reduce #'+ (mapcar #'(lambda (vi ci) 
       (let ((xc (- xarg ci))) 
         (* vi (exp (* -1 (/ (* 2 w)) xc xc))))) 
        v c))))

(defun string2num (s) 
  (loop for xi in (reverse s) for p = 1 then (* p 2) sum (* xi p)))
