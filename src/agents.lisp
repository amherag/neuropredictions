(cl21:in-package :cl21-user)
(defpackage neuropredictions.agents
  (:use :cl21
        :lparallel
        :neuropredictions.config
        :neuropredictions.predict
	:neuropredictions.ga
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

(defun gen-beliefs (n)
  (let ((options '(0.236 0.382 0.5 0.618 1 1.618)))
    (map (lm (_)
	   (map (lm (fibo)
		  (if (= (random-int *rand-gen* 0 1) 0)
		      (+ fibo (/ (random-int *rand-gen* 0 5) 100))
		      (- fibo (/ (random-int *rand-gen* 0 5) 100))))
		(take (1+ (random-int *rand-gen* 0 (1- (length options)))) (shuffle options))))
	 (iota n))))

(defun gen-rules (num-agents num-rules)
  (let ((opts (shuffle
               (reduce #'append (map (lm (pi)
                                       (map (lm (mean)
                                              (list mean pi))
                                            '(0.0 50.0 100.0)))
                                     '(0.0 0.15 0.3))))))
    (apply #'nconc
	   (map (lm (_)
		  (map (lm (_)
			 (map (lm (_) (nth opts (random-int *rand-gen* 0 (1- (length opts))))) (iota 4)))
		       (iota num-rules)))
		(iota num-agents)))
    ))


(defclass agent ()
  ((beliefs :initarg :beliefs :initform (gen-beliefs) :accessor beliefs)
   (rules :initarg :rules :initform (gen-rules 10) :accessor rules)))

(defparameter *num-agents* 5)
(defparameter *num-rules* 10)

(defclass agents ()
  ((beliefs :initarg :beliefs :initform (gen-beliefs *num-agents*) :accessor beliefs)
   (rules :initarg :rules :initform (gen-rules *num-agents* *num-rules*) :accessor rules)))

;; (slot-value (make-instance 'agents) 'rules)

;; (gen-beliefs 10)
;; (print (gen-rules 5 10))

(defun gen-agents (n)
  (map (lm (_)
         (make-instance 'agent))
       (iota n)))

(quote
 (let ((agent (make-instance 'agent)))
   (print (cl-json:encode-json-to-string (slot-value (make-instance 'agent) 'beliefs)))
   (print (cl-json:encode-json-to-string (slot-value (make-instance 'agent) 'rules)))
   nil))

;; (defparameter *rates* (get-rates :EUR_USD 1 :H1))

(quote
 (time
  (map (lm (agent)
         (let ((data (get-data :EUR_USD *rates* :levels (slot-value agent 'beliefs)))
               (rules (slot-value agent 'rules)))


           (dex:post "http://localhost:5000/ifis-agents"
                     ;; :headers '(("Content-Type" . "application/json"))
                     :content `(("inputs". ,(cl-json:encode-json-to-string
                                             (map (lm (heat)
                                                    (let ((heat (gethash heat :heat))
                                                          (close (gethash heat :close)))
                                                      (let ((z (gethash heat :z))
                                                            (index (search `(,close) (gethash heat :y)
                                                                           :key (lm (elt)
                                                                                  (when (> close elt) t)))))
							;; (print (apply #'max z))
							(list (nth z (- index 2))
                                                              (nth z (- index 1))
                                                              (nth z index)))))
                                                  data)))
				("rules" . ,(cl-json:encode-json-to-string rules)))
                     )
           ))
       (gen-agents 1))))




;; (get-data instrument data)

;; (dex:get "http://localhost:5000/ifis-agents")

;; (dex:post "http://localhost:5000/ifis-agents"
;;           :content '(("data". "hello")))


