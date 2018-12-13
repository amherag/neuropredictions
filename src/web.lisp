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
(setf (gethash :CHF_HKD *allowed-markets*) '(:gold :platinum))
(setf (gethash :CHF_JPY *allowed-markets*) '(:gold :platinum))
(setf (gethash :CHF_ZAR *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_AUD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_CAD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_CHF *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :EUR_CZK *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_DKK *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_GBP *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :EUR_HKD *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_HUF *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_JPY *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :EUR_NOK *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_NZD *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_PLN *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_SEK *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_SGD *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_TRY *allowed-markets*) '(:gold :platinum))
(setf (gethash :EUR_USD *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :EUR_ZAR *allowed-markets*) '(:gold :platinum))
(setf (gethash :GBP_AUD *allowed-markets*) '(:gold :platinum))
(setf (gethash :GBP_CAD *allowed-markets*) '(:gold :platinum))
(setf (gethash :GBP_CHF *allowed-markets*) '(:gold :platinum))
(setf (gethash :GBP_HKD *allowed-markets*) '(:gold :platinum))
(setf (gethash :GBP_JPY *allowed-markets*) '(:gold :platinum))
(setf (gethash :GBP_NZD *allowed-markets*) '(:gold :platinum))
(setf (gethash :GBP_PLN *allowed-markets*) '(:gold :platinum))
(setf (gethash :GBP_SGD *allowed-markets*) '(:gold :platinum))
(setf (gethash :GBP_USD *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :GBP_ZAR *allowed-markets*) '(:gold :platinum))
(setf (gethash :HKD_JPY *allowed-markets*) '(:gold :platinum))
(setf (gethash :NZD_CAD *allowed-markets*) '(:gold :platinum))
(setf (gethash :NZD_CHF *allowed-markets*) '(:gold :platinum))
(setf (gethash :NZD_HKD *allowed-markets*) '(:gold :platinum))
(setf (gethash :NZD_JPY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :NZD_SGD *allowed-markets*) '(:gold :platinum))
(setf (gethash :NZD_USD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :SGD_CHF *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :SGD_HKD *allowed-markets*) '(:gold :platinum))
(setf (gethash :SGD_JPY *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :TRY_JPY *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_CAD *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :USD_CHF *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :USD_CNH *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_CZK *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_DKK *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_HKD *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_HUF *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_INR *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_JPY *allowed-markets*) '(:bronze :silver :gold :platinum))
(setf (gethash :USD_MXN *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_NOK *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_PLN *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_SAR *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_SEK *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_SGD *allowed-markets*) '(:silver :gold :platinum))
(setf (gethash :USD_THB *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_TRY *allowed-markets*) '(:gold :platinum))
(setf (gethash :USD_ZAR *allowed-markets*) '(:gold :platinum))
(setf (gethash :ZAR_JPY *allowed-markets*) '(:gold :platinum))

;; indices
(setf (gethash :AU200_AUD *allowed-markets*) '(:platinum))
(setf (gethash :CN50_USD *allowed-markets*) '(:platinum))
(setf (gethash :EU50_EUR *allowed-markets*) '(:platinum))
(setf (gethash :FR40_EUR *allowed-markets*) '(:platinum))
(setf (gethash :DE30_EUR *allowed-markets*) '(:platinum))
(setf (gethash :HK33_HKD *allowed-markets*) '(:platinum))
(setf (gethash :IN50_USD *allowed-markets*) '(:platinum))
(setf (gethash :JP225_USD *allowed-markets*) '(:platinum))
(setf (gethash :NL25_EUR *allowed-markets*) '(:platinum))
(setf (gethash :SG30_SGD *allowed-markets*) '(:platinum))
(setf (gethash :TWIX_USD *allowed-markets*) '(:platinum))
(setf (gethash :UK100_GBP *allowed-markets*) '(:platinum))
(setf (gethash :NAS100_USD *allowed-markets*) '(:platinum))
(setf (gethash :US2000_USD *allowed-markets*) '(:platinum))
(setf (gethash :SPX500_USD *allowed-markets*) '(:platinum))
(setf (gethash :US30_USD *allowed-markets*) '(:platinum))

;; commodities
(setf (gethash :BCO_USD *allowed-markets*) '(:platinum))
(setf (gethash :XCU_USD *allowed-markets*) '(:platinum))
(setf (gethash :CORN_USD *allowed-markets*) '(:platinum))
(setf (gethash :NATGAS_USD *allowed-markets*) '(:platinum))
(setf (gethash :SOYBN_USD *allowed-markets*) '(:platinum))
(setf (gethash :SUGAR_USD *allowed-markets*) '(:platinum))
(setf (gethash :WTICO_USD *allowed-markets*) '(:platinum))
(setf (gethash :WHEAT_USD *allowed-markets*) '(:platinum))

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

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  ;; (merge-pathnames #P"_errors/404.html"
  ;;                  *template-directory*)
  )

;; (ql-dist:install-dist "http://dists.cl21.org/cl21.txt")
;; (ql:update-all-dists)
