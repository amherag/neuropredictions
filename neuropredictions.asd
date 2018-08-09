(in-package :cl-user)
(defpackage neuropredictions-asd
  (:use :cl :asdf))
(in-package :neuropredictions-asd)

(defsystem neuropredictions
  :version "0.1"
  :author "Amaury Hernandez-Aguila"
  :license ""
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop
               :uuid

               ;; for predict module
               :cl21
               :lparallel
               :local-time
               :clnuplot
               :dexador
               :alexandria
               :cl-json
               ;; :clml
               :fare-memoization
               
               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "predict"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config")
                 (:file "predict"))))
  :description ""
  :in-order-to ((test-op (load-op neuropredictions-test))))
