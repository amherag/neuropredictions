(cl21:in-package :cl21-user)

(defpackage neuropredictions.predict
  (:use :cl21
        :lparallel
        :neuropredictions.config))
(in-package :neuropredictions.predict)

;; start module configuration

(setf lparallel:*kernel* (lparallel:make-kernel 8))
(setf clnuplot:*plot-default-directory* *plots-directory*)

;; end module configuration

;; start general purpose functions

(defun pips (n &optional (jpy? nil) (decimal? nil))
  (if decimal?
      (if jpy?
          (/ n 1000)
          (/ n 100000))
      (if jpy?
          (/ n 100)
          (/ n 10000))))

(defun print-rate (rate)
  (format t "~5$~%" rate))

(defun round-to (number precision &optional (what #'round))
  (float (let ((div (expt 10 precision)))
           (/ (funcall what (* number div)) div))))

(defun start-end (howmany)
  (let* ((pre-result (iota (round (/ (+ howmany 5000) 5000)) :start 0 :step 5000))
         (result (if (> (last pre-result) howmany)
                     (concatenate 'list (butlast pre-result) (list howmany))
                     (if (= (last pre-result) howmany)
                         pre-result
                         (concatenate 'list pre-result (list howmany))))))
    result))

(defun get-rates (instrument howmany granularity)
  "Gathers prices from Oanda."
  (let ((multiplier (cond ((eq granularity :S5) 5)
                          ((eq granularity :S10) 10)
                          ((eq granularity :S15) 15)
                          ((eq granularity :S30) 30)
                          ((eq granularity :M1) 60)
                          ((eq granularity :M2) 120)
                          ((eq granularity :M3) 180)
                          ((eq granularity :M4) 240)
                          ((eq granularity :M5) 300)
                          ((eq granularity :M10) 600)
                          ((eq granularity :M15) 900)
                          ((eq granularity :M30) 1800)
                          ((eq granularity :H1) 3600)
                          ((eq granularity :H2) 7200)
                          ((eq granularity :H3) 10800)
                          ((eq granularity :H4) 14400)
                          ((eq granularity :H6) 21600)
                          ((eq granularity :H8) 28800)
                          ((eq granularity :H12) 43200)
                          ((eq granularity :D) 86400)
                          ((eq granularity :W) 604800)
                          )))
    (let ((start-ends (map (lm (start end)
                             `((:start . ,(* (- (local-time:timestamp-to-unix (local-time:now))
                                                (* (1- start) multiplier))
                                             1000000))
                               (:end . ,(* (- (local-time:timestamp-to-unix (local-time:now))
                                              (* end multiplier))
                                           1000000))))
                           (rest (start-end howmany))
                           (start-end howmany))))
      (apply #'concatenate 'list
             (reverse
              (map (lm (start-end)
                     (sleep 0.1)
                     (let ((start (rest (assoc :start start-end)))
                           (end (rest (assoc :end start-end))))
                       ;; remove this map if more data is needed
                       (map ^(list (assoc :close-bid %)
                                   (assoc :time %))
                            (rest (assoc :candles (cl-json:decode-json-from-string
                                                   (dex:get #"https://api-fxtrade.oanda.com/v1/candles?\
instrument=${instrument}&\
granularity=${granularity}&\
start=${start}&\
end=${end}&\
dailyAlignment=0&\
candleFormat=bidask&\
alignmentTimezone=America%2FNew_York"
                                                            :insecure t
                                                            :headers '(("X-Accept-Datetime-Format" . "UNIX")))))))))
                   start-ends
                   ))))))

(defun rsum (f init xs)
  (loop for x in xs collect (setf init (funcall f init x))))

;; end general purpose functions

;; start predictus algorithm functions

(defun diffs (rates &optional (bid? t))
  "Calculates the difference between two close prices."
  (let* ((bid-or-ask (if bid? :close-bid :close-ask))
         (closes (map (lm (rate)
                        (rest (assoc bid-or-ask rate))
                        )
                      rates)))
    (map (lm (price delta)
           (cons price delta))
         (rest closes)
         (map (lm (c1 c2)
                (- c2 c1))
              closes
              (rest closes))
         )))

(defun fibos (diffs)
  "Returns fibos from bigger to smaller."
  (sort (flatten (map (lm (diff)
                        (let ((price (first diff))
                              (delta (rest diff)))
                          (list (+ price (* delta 0.382))
                                (+ price (* delta 0.618))
                                (+ price (* delta 1.000))
                                (+ price (* delta 1.618))
                                )))
                      diffs))
        #'>))

(defun get-subsets (pivot fibos)
  (let ((upper (take-while (lm (elt)
                             (> elt pivot))
                           fibos))
        (lower (drop-while (lm (elt)
                             (>= elt pivot))
                           fibos)))
    ;;upper
    `((:upper ,upper)
      (:lower ,lower))))

(defun ts-partition (n ts)
  (remove nil
          (pmaplist (lm (ts)
                     (if (>= (length ts) n)
                         (take n ts)))
                   ts)))

(defun clml-centroids (k clml-subset)
  (map (lm (centroid)
         (first (rest centroid)))
       (clml.hjs.k-means:get-cluster-centroids
        (clml.hjs.k-means:k-means k clml-subset))))

(defun clml-subset (subset)
  (let ((subset (map (lm (r)
                       #"${r}\n")
                     subset)))
    (clml.hjs.read-data:pick-and-specialize-data
     (clml.hjs.read-data:read-data-from-stream
      (make-string-input-stream #"@{subset}")
      :csv-header-p '("rates")
      :csv-type-spec '(double-float))
     :range :all
     :data-types '(:numeric)
     )))


;; end predictus algorithm functions

;; start optimization algorithm functions

(defun max-short (plot-results)
  (let* ((profits (map #'second plot-results))
         (highest (first profits))
         (max-short 0))
    (map (lm (profit)
           (if (> profit highest)
               (setq highest profit)
               (if (> (- highest profit) max-short)
                   (setq max-short (- highest profit)))))
         profits)
    max-short))

(defun score (points)
  (if (< (length points) 2)
      0
      (let* ((first (first points))
             (last (last points))
             (howmany (length points))
             (howmany-positive (length (remove-if-not ^(> % 0) points)))
             (ideal (iota howmany :start first :step (/ (- last first) (1- howmany)))))
        (if (or
             (< (- last first) 0)
             (<= howmany 1))
            0
            (/ (- last first)
               (expt (1+ (reduce #'+
                                 (map (lm (p i)
                                        (abs (- p i)))
                                      points
                                      ideal)))
                     (/ 1 howmany))))
        )))

(defun gen-percentiles ()
  (apply #'concatenate 'list
         (map (lm (n1)
                (remove nil
                        (map (lm (n2)
                               (list n1 n2)
                               )
                             ;;omega
                             (iota 10 :start 1 :step 1)))
                )
              ;;alpha
              (iota 10 :start 1 :step 1))))

;; end optimization algorithm functions
