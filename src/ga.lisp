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
        (setq offspring (nconc (list x y) offspring))
	))
    offspring))

;; (reproduce '((1 2 3 4) (41 313 12 131) (5435 1234 122 123) (1313 4151 123 4151)))

(defun distribution (population)
  (let* ((genotypes (remove-duplicates population :test #'equal)) 
	 (sum (apply #'+ (mapcar #'fitness genotypes)))) 
    (mapcar #'(lambda (x) (cons (/ (fitness x) sum) x)) genotypes)))

;; (distribution '((1 2 3) (41 313 12) (5435 1234 122) (1313 4151 123)))

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

;; (selectone (distribution '((1 2 3) (41 313 12) (5435 1234 122) (1313 4151 123))))

(defun crossover (x y) 
  (if (> (random-float *rand-gen* 0 1.0) 0.6) (list x y) 
      ;;else 
      (let* ((site (random-int *rand-gen* 0 (1- (length x)))) 
	     (swap (rest (nthcdr site x)))) 
	(setf (rest (nthcdr site x)) (rest (nthcdr site y))) 
	(setf (rest (nthcdr site y)) swap))))

;; (crossover '(1 2 3 4 5 6 7 8 9 10) '(11 12 13 14 15 16 17 18 19 20))
;; (crossover (selectone (distribution '((1 2 3) (41 313 12) (5435 1234 122) (1313 4151 123))))
;; 	   (selectone (distribution '((1 2 3) (41 313 12) (5435 1234 122) (1313 4151 123)))))

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

;; (fitness '(1 2 3))

(defun string2num (s) 
  (loop for xi in (reverse s) for p = 1 then (* p 2) sum (* xi p)))

