(in-package :cl-user)
(defpackage neuropredictions.web
  (:use :cl
        :caveman2
        :neuropredictions.config
        :neuropredictions.view
        :neuropredictions.db
        :neuropredictions.predict
        :datafly
        :sxql
        )
  (:export :*web*))
(in-package :neuropredictions.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute ("/(.+?\\.html)" :regexp t) (&key captures)
  (render (pathname (apply #'concatenate 'string captures))))

;; (defroute ("/(.+?\\.js)" :regexp t) (&key captures)
;;   (render (pathname (apply #'concatenate 'string captures))))

;; (defroute ("/predict/load-data/:pair/:granularity" :method :get) (&key pair granularity)
;;   ;;(setf (getf (response-headers *response* :content-type) "application/json"))
;;   (let ((data (load-data (read-from-string (format nil ":~a" pair))
;;                          (read-from-string (format nil ":~a" granularity))))
;;         times
;;         rates)
;;     ;;(setf (getf (response-headers *response*) :content-type) "application/json")
;;     (mapcar (lambda ( ;;i
;;                      tuple)
;;               (setq times (append times (list (/ (second tuple) 1000))))
;;               (setq rates (append rates (list (first tuple))))
;;               )
;;             ;;(cl21:iota (length data))
;;             data)
;;     (render-json (list times rates))
;;     (render-json
;;      (cl21:hash-table #'equal
;;                       :times times
;;                       :rates rates))))

;; (defroute ("/predict/load-data/:pair/:granularity" :method :get) (&key pair granularity)
;;   ;;(setf (getf (response-headers *response* :content-type) "application/json"))
;;   (let ((data (load-data (read-from-string (format nil ":~a" pair))
;;                          (read-from-string (format nil ":~a" granularity))))
;;         times
;;         rates)
;;     ;;(setf (getf (response-headers *response*) :content-type) "application/json")
;;     (mapcar (lambda ( ;;i
;;                      tuple)
;;               (setq times (append times (list (/ (second tuple) 1000))))
;;               (setq rates (append rates (list (first tuple))))
;;               )
;;             ;;(cl21:iota (length data))
;;             data)
;;     (render-json (list times rates))
;;     (render-json
;;      (cl21:hash-table #'equal
;;                       :times times
;;                       :rates rates))))

;;(get-trades (load-data :AUD_USD :H1) '(10 11))


;; copia
;; (defroute ("/predict/load-trades/:pair/:granularity" :method :get) (&key pair granularity)
;;   (let* ((jpy? (if (cl-ppcre:scan "JPY" pair) t))
;;          (data (load-data (read-from-string (format nil ":~a" pair))
;;                           (read-from-string (format nil ":~a" granularity))))
;;          (trades (get-trades data
;;                              (cl21:getf (cl21:getf *bests* (read-from-string (format nil ":~a" pair)))
;;                                         (read-from-string (format nil ":~a" granularity)))
;;                              jpy?)))
;;     (render-json
;;      (remove nil
;;              (concatenate 'list
;;                           (mapcar (lambda (fst snd)
;;                                     (if (eq (first fst) :BUY)
;;                                         (cl21:hash-table #'equal
;;                                                          "backgroundColor" "rgba(0,0,255,0.0)"
;;                                                          "borderColor" "rgba(0,0,255,0.4)"
;;                                                          :data
;;                                                          (list (cl21:hash-table #'equal
;;                                                                                 :x (/ (second fst) 1000)
;;                                                                                 :y (third fst))
;;                                                                (cl21:hash-table #'equal
;;                                                                                 :x (/ (second snd) 1000)
;;                                                                                 :y (third snd))))
;;                                         (if (eq (first fst) :SELL)
;;                                             (cl21:hash-table #'equal
;;                                                              "backgroundColor" "rgba(255,0,0,0.0)"
;;                                                              "borderColor" "rgba(255,0,0,0.4)"
;;                                                              :data
;;                                                              (list (cl21:hash-table #'equal
;;                                                                                     :x (/ (second fst) 1000)
;;                                                                                     :y (third fst))
;;                                                                    (cl21:hash-table #'equal
;;                                                                                     :x (/ (second snd) 1000)
;;                                                                                     :y (third snd))))
;;                                             )))
;;                                   trades
;;                                   (rest trades))
;;                           (if (eq (first (cl21:last trades)) :BUY)
;;                               (list (cl21:hash-table #'equal
;;                                                      "backgroundColor" "rgba(0,0,255,0.0)"
;;                                                      "borderColor" "rgba(0,0,255,0.4)"
;;                                                      :data
;;                                                      (list (cl21:hash-table #'equal
;;                                                                             :x (/ (second (cl21:last trades)) 1000)
;;                                                                             :y (third (cl21:last trades)))
;;                                                            (cl21:hash-table #'equal
;;                                                                             :x (/ (second (cl21:last data)) 1000)
;;                                                                             :y (first (cl21:last data))))))
;;                               (if (eq (first (cl21:last trades)) :SELL)
;;                                   (list (cl21:hash-table #'equal
;;                                                          "backgroundColor" "rgba(255,0,0,0.0)"
;;                                                          "borderColor" "rgba(255,0,0,0.4)"
;;                                                          :data
;;                                                          (list (cl21:hash-table #'equal
;;                                                                                 :x (/ (second (cl21:last trades)) 1000)
;;                                                                                 :y (third (cl21:last trades)))
;;                                                                (cl21:hash-table #'equal
;;                                                                                 :x (/ (second (cl21:last data)) 1000)
;;                                                                                 :y (first (cl21:last data)))))))
;;                               )
;;                           )
                  
;;              ))))

;; (defparameter *data* (get-rates :AUD_USD 1 :M1))

(defparameter *full-queue* nil)

(defroute ("/heatmap/:key/:pair/:granularity" :method :get) (&key key pair granularity)
  ;; (defparameter *data* (get-rates (read-from-string (format nil ":~a" pair)) 1 (read-from-string (format nil ":~a" granularity))))
  (ignore-errors
    (if *full-queue*
        (loop do (sleep 0.0) while *full-queue*))
    (setf *full-queue* t)
    (if (find key *keys* :test #'string=)
        (let ((data (get-rates (read-from-string (format nil ":~a" pair)) 1 (read-from-string (format nil ":~a" granularity))))
              (instrument (read-from-string (format nil ":~a" pair))))
          (setf (getf (response-headers *response*) :content-type) "application/json")
          (setf (getf (response-headers *response*) :access-control-allow-origin) "*")
          (setf *full-queue* nil)
          (render-json (get-data instrument data))))
    )
  )

;; (defroute ("/predict/load-trades/:pair/:granularity" :method :get) (&key pair granularity)
;;   (let* ((jpy? (if (cl-ppcre:scan "JPY" pair) t))
;;          (data (load-data (read-from-string (format nil ":~a" pair))
;;                           (read-from-string (format nil ":~a" granularity))))
;;          (trades (get-trades data
;;                              (cl21:getf (cl21:getf *bests* (read-from-string (format nil ":~a" pair)))
;;                                         (read-from-string (format nil ":~a" granularity)))
;;                              jpy?)))
;;     (render-json
;;      (remove nil
;;              (concatenate 'list
;;                           (mapcar (lambda (fst snd)
;;                                     (if (eq (first fst) :BUY)
;;                                         (cl21:hash-table #'equal
;;                                                          "backgroundColor" "rgba(0,0,255,0.0)"
;;                                                          "borderColor" "rgba(0,0,255,0.4)"
;;                                                          :data
;;                                                          (list (cl21:hash-table #'equal
;;                                                                                 :x (/ (second fst) 1000)
;;                                                                                 :y (third fst))
;;                                                                (cl21:hash-table #'equal
;;                                                                                 :x (/ (second snd) 1000)
;;                                                                                 :y (third snd))))
;;                                         (if (eq (first fst) :SELL)
;;                                             (cl21:hash-table #'equal
;;                                                              "backgroundColor" "rgba(255,0,0,0.0)"
;;                                                              "borderColor" "rgba(255,0,0,0.4)"
;;                                                              :data
;;                                                              (list (cl21:hash-table #'equal
;;                                                                                     :x (/ (second fst) 1000)
;;                                                                                     :y (third fst))
;;                                                                    (cl21:hash-table #'equal
;;                                                                                     :x (/ (second snd) 1000)
;;                                                                                     :y (third snd))))
;;                                             )))
;;                                   trades
;;                                   (rest trades))
;;                           (if (eq (first (cl21:last trades)) :BUY)
;;                               (list (cl21:hash-table #'equal
;;                                                      "backgroundColor" "rgba(0,0,255,0.0)"
;;                                                      "borderColor" "rgba(0,0,255,0.4)"
;;                                                      :data
;;                                                      (list (cl21:hash-table #'equal
;;                                                                             :x (/ (second (cl21:last trades)) 1000)
;;                                                                             :y (third (cl21:last trades)))
;;                                                            (cl21:hash-table #'equal
;;                                                                             :x (/ (second (cl21:last data)) 1000)
;;                                                                             :y (first (cl21:last data))))))
;;                               (if (eq (first (cl21:last trades)) :SELL)
;;                                   (list (cl21:hash-table #'equal
;;                                                          "backgroundColor" "rgba(255,0,0,0.0)"
;;                                                          "borderColor" "rgba(255,0,0,0.4)"
;;                                                          :data
;;                                                          (list (cl21:hash-table #'equal
;;                                                                                 :x (/ (second (cl21:last trades)) 1000)
;;                                                                                 :y (third (cl21:last trades)))
;;                                                                (cl21:hash-table #'equal
;;                                                                                 :x (/ (second (cl21:last data)) 1000)
;;                                                                                 :y (first (cl21:last data)))))))
;;                               )
;;                           )
                  
;;              ))))

;;(cl-json:decode-json-from-string)
;;(cl-json:encode-json '(1 2 3))

;;(load-data :EUR_USD :H1)



;;(load-data :AUD_USD :H1)


(quote
 (defparameter *keys*
   (mapcar (lambda (_)
             (format nil "~a" (uuid:make-v4-uuid)))
           (make-list 5))))

(defparameter *keys* '("C5C87392-BAB9-476D-8583-A891F89983F2" "8C707F3E-1A82-419F-AD0D-5FC53B04EBF4"
                       "4BBCE98B-3B26-4768-9651-C9A3FEC175E6" "876D38D2-41DD-48EF-B624-B99A8BC63C54"
                       "2E7EB68E-19C6-4B6A-BCF8-F52A53422DB6"))

(defparameter *key-levels*
  (let ((keys (make-hash-table)))
    (mapcar (lambda (lvl key)
              (setf (gethash key  keys) lvl))
            '(:lvl1 :lvl2 :lvl3 :lvl4 :lvl5)
            *keys*)
    keys))

(defroute "/:key" (&key key)
  (ignore-errors
    (if (find key *keys* :test #'string=)
        (render #P"index.html" `(:key ,key)))
    ))


;; (gethash (first *keys*) *key-levels*)
;; (first *keys*)

;; (defroute "/index.js" ()
;;   (ignore-errors
;;     ;; (setf (getf (response-headers *response* :content-type) "text/javascript"))
;;     (render #P"index.js")))

;; (defroute "/jquery.min.js" ()
;;   (ignore-errors
;;     ;; (setf (getf (response-headers *response* :content-type) "application/javascript"))
;;     (render #P"jquery.min.js")))

;; (defroute "/plotly-latest.min.js" ()
;;   (ignore-errors
;;     ;; (setf (getf (response-headers *response* :content-type) "application/javascript"))
;;     (render #P"plotly-latest.min.js")))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  ;; (merge-pathnames #P"_errors/404.html"
  ;;                  *template-directory*)
  )

;; (ql-dist:install-dist "http://dists.cl21.org/cl21.txt")
;; (ql:update-all-dists)

