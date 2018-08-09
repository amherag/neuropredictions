
;;;AOP facility agent

(defagent fa
  :timegrain 10
  :beliefs '(
            (1 (link-resource '(C1 C3) 3))
  )
  :commit-rules
  '(
    ((?agent REQUEST (do ?time (restore-link ?link)))
     (B (?time (link-resource ?link ?cap))) ; was now
     ?agent
     (do ?time (restore-link ?link)))	; was now
    )
)


(defun restore-link (link) (format t "~%Restore-link ~S" link))
