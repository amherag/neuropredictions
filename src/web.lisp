(in-package :cl-user)
(defpackage neuropredictions.web
  (:use :cl
        :caveman2
        :neuropredictions.config
        :neuropredictions.view
        :neuropredictions.db
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

(defroute "/" ()
  (render #P"index.html"))

(defroute "/index.html" ()
  (render #P"index.html"))

(defroute "/index2.html" ()
  (render #P"index2.html"))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))


;;(ql:quickload :neuropredictions)
;;(neuropredictions:start :port 8080)
