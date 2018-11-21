(cl21:in-package :cl21-user)
(defpackage neuropredictions.agents
  (:use :cl21
        :lparallel
        :neuropredictions.config
        :neuropredictions.predict)
  (:export :foo
           ))
(in-package :neuropredictions.agents)

;; (defclass broker ())

(defun gen-beliefs ()
  (let ((options '(0.236 0.382 0.5 0.618 1 1.618)))
    (map (lm (fibo)
           (if (= (random 2) 0)
               (+ fibo (/ (random 6) 100))
               (- fibo (/ (random 6) 100))))
         (take (1+ (random (length options))) (alexandria:shuffle options)))))

(defun gen-rules (n)
  (let ((opts (alexandria:shuffle
               (reduce #'append (map (lm (pi)
                                       (map (lm (mean)
                                              (list mean pi))
                                            '(0.0 50.0 100.0)))
                                     '(0.0 0.15 0.3))))))
    (map (lm (_)
           (map (lm (_) (alexandria:random-elt opts)) (iota 4)))
         (iota n))
    ))

(defclass agent ()
  ((beliefs :initarg :beliefs :initform (gen-beliefs) :accessor beliefs)
   (rules :initarg :rules :initform (gen-rules 10) :accessor rules)))

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


