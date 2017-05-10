(in-package :cl-user)
(defpackage neuropredictions-test-asd
  (:use :cl :asdf))
(in-package :neuropredictions-test-asd)

(defsystem neuropredictions-test
  :author "Amaury Hernandez-Aguila"
  :license ""
  :depends-on (:neuropredictions
               :prove)
  :components ((:module "t"
                :components
                ((:file "neuropredictions"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
