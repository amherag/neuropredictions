;; (cl21:in-package :cl21-user)
(in-package :cl-user)
(defpackage neuropredictions.web
  (:use :cl
        ;; :cl21
        :caveman2
        :neuropredictions.config
        :neuropredictions.view
        :neuropredictions.db
        :neuropredictions.predict
        ;; :datafly
        ;; :sxql
        )
  ;; (:require :cl21)
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

(defparameter *allowed-markets* (make-hash-table))
(setf (gethash :AUD_CAD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :AUD_CHF *allowed-markets*) '(:gold :platinum))
(setf (gethash :AUD_HKD *allowed-markets*) '(:gold :platinum))
(setf (gethash :AUD_JPY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :AUD_NZD *allowed-markets*) '(:gold :platinum))
(setf (gethash :AUD_SGD *allowed-markets*) '(:gold :platinum))
(setf (gethash :AUD_USD *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :CAD_CHF *allowed-markets*) '(:gold :platinum))
(setf (gethash :CAD_HKD *allowed-markets*) '(:gold :platinum))
(setf (gethash :CAD_JPY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :CAD_SGD *allowed-markets*) '(:gold :platinum))
(setf (gethash :CHF_HKD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :CHF_JPY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :CHF_ZAR *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_AUD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_CAD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_CHF *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_CZK *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_DKK *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_GBP *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :EUR_HKD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_HUF *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_JPY *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :EUR_NOK *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_NZD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_PLN *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_SEK *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_SGD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_TRY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_USD *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :EUR_ZAR *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :GBP_AUD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :GBP_CAD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :GBP_CHF *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :GBP_HKD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :GBP_JPY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :GBP_NZD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :GBP_PLN *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :GBP_SGD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :GBP_USD *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :GBP_ZAR *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :HKD_JPY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :NZD_CAD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :NZD_CHF *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :NZD_HKD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :NZD_JPY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :NZD_SGD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :NZD_USD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :SGD_CHF *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :SGD_HKD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :SGD_JPY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :TRY_JPY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_CAD *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :USD_CHF *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :USD_CNH *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_CZK *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_DKK *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_HKD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_HUF *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_INR *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_JPY *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :USD_MXN *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_NOK *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_PLN *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_SAR *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_SEK *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_SGD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_THB *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_TRY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_ZAR *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :ZAR_JPY *allowed-markets*) '(:silver :gold :platinum))

     ;; indices
(setf (gethash :BCO_USD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :SPX500_USD *allowed-markets*) '(:silver :gold :platinum))


(defparameter *allowed-timeframes* (make-hash-table))

(setf (gethash :S5 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :S10 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :S15 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :S30 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :M1 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :M2 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :M3 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :M4 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :M5 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :M10 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :M15 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :M30 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :H1 *allowed-timeframes*) '(:bronze :silver :gold :platinum))
(setf (gethash :H2 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :H3 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :H4 *allowed-timeframes*) '(:bronze :silver :gold :platinum))
(setf (gethash :H6 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :H8 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :H12 *allowed-timeframes*) '(:silver :gold :platinum))
(setf (gethash :D *allowed-timeframes*) '(:bronze :silver :gold :platinum))
(setf (gethash :W *allowed-timeframes*) '(:bronze :silver :gold :platinum))
(setf (gethash :M *allowed-timeframes*) '(:bronze :silver :gold :platinum))

;; (defparameter *allowed-timeframes*
;;   cl21:#H(
;;           :S5 '(:bronze :silver :gold :platinum)
;;      :S10 '(:bronze :silver :gold :platinum)
;;      :S15 '(:bronze :silver :gold :platinum)
;;      :S30 '(:bronze :silver :gold :platinum)
;;      :M1 '(:bronze :silver :gold :platinum)
;;      :M2 '(:bronze :silver :gold :platinum)
;;      :M3 '(:bronze :silver :gold :platinum)
;;      :M4 '(:bronze :silver :gold :platinum)
;;      :M5 '(:bronze :silver :gold :platinum)
;;      :M10 '(:bronze :silver :gold :platinum)
;;      :M15 '(:bronze :silver :gold :platinum)
;;      :M30 '(:bronze :silver :gold :platinum)
;;      :H1 '(:bronze :silver :gold :platinum)
;;      :H2 '(:bronze :silver :gold :platinum)
;;      :H3 '(:bronze :silver :gold :platinum)
;;      :H4 '(:bronze :silver :gold :platinum)
;;      :H6 '(:bronze :silver :gold :platinum)
;;      :H8 '(:bronze :silver :gold :platinum)
;;      :H12 '(:bronze :silver :gold :platinum)
;;      :D '(:bronze :silver :gold :platinum)
;;      :W '(:bronze :silver :gold :platinum)
;;      :M '(:bronze :silver :gold :platinum)
;;      ))

(defroute ("/transactions" :method :get) ()
    ;; (render-json (cl-json:encode-json-to-string (get-transactions)))
  (render-json (get-transactions)))

(defroute ("/heatmap/:key/:pair/:granularity" :method :get) (&key key pair granularity)  
  (ignore-errors
    (if *full-queue*
        (loop do (sleep 0.0) while *full-queue*))
    (setf *full-queue* t)
    (if (find key *keys* :test #'string=)
        (let ((pair (read-from-string (format nil ":~a" pair)))
              (granularity (read-from-string (format nil ":~a" granularity)))
              (level (gethash key *key-levels*)))
          (if (and (find level (gethash pair *allowed-markets*))
                   (find level (gethash granularity *allowed-timeframes*)))
              (let ((data (get-rates pair 1 granularity))
                    (instrument (read-from-string (format nil ":~a" pair))))
                (setf (getf (response-headers *response*) :content-type) "application/json")
                (setf (getf (response-headers *response*) :access-control-allow-origin) "*")
                (setf *full-queue* nil)
                (render-json (get-data instrument data))
                )
              (render-json "{}"))
          )
        (render-json "{}"))
    )
  )

(defroute ("/levelpairs/:key" :method :get) (&key key)
  (ignore-errors
    (if (find key *keys* :test #'string=)
        (let ((level (gethash key *key-levels*)))

          (render-json
           (let (results)
             (maphash (lambda (k v)
                        (if (find level v)
                            (pushnew k results)))
                      *allowed-markets*)
             (reverse results)))
          )
        (render-json "{}"))))

(defroute ("/levelgranularity/:key" :method :get) (&key key)
  (ignore-errors
    (if (find key *keys* :test #'string=)
        (let ((level (gethash key *key-levels*)))

          (render-json 
           (let (results)
             (maphash (lambda (k v)
                        (if (find level v)
                            (pushnew k results)))
                      *allowed-timeframes*)
             (reverse results))
           )
          )
        (render-json "{}"))))

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
                       "4BBCE98B-3B26-4768-9651-C9A3FEC175E6" "876D38D2-41DD-48EF-B624-B99A8BC63C54"))

(defparameter *key-levels*
  (let ((keys (make-hash-table :test #'equalp)))
    (mapcar (lambda (lvl key)
              (setf (gethash key keys) lvl))
            '(:bronze :silver :gold :platinum)
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
