;;;joetriv

(defagent joetriv
  :timegrain 10
  :beliefs '()
  :commit-rules
  '(

   ((?agent REQUEST (if (b (?time (?fact))) (do ?time2 (?action))))
    ()
    ?agent
    (if (b (?time (?fact))) (do ?time2 (?action))) )
)
)


(defun becool () (print "i am cool"))

  