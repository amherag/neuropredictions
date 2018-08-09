
;;;  NEWJOETRIV.LISP
;;;  Mark C. Torrance
;;;  torrance@cs.stanford.edu
;;;
;;;  A New Trivial Agent
;;;
;;;  Responds to requests to (becool) subject to the mental condition
;;;  that he believes (now... at the moment of receipt of the message)
;;;  that he will be cool (i_am_cool) at the time stated in the
;;;  message, when he is requested to be cool.
;;;
;;;  Defined in AGENT0 Lisp-like Syntax

;;;AOP customer agent

(defagent ca
  :timegrain 10
  :beliefs '(
            (1 (failed-link '(C1 C3)))
	     )
  :commit-rules
  '(
    (()
     (B (now (failed-link ?link)))
     ca
     (REQUEST now fa (DO now (restore-link ?link))))
    )
)

