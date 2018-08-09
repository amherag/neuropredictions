;;;  CONTROL.LISP
;;;  George John
;;;  gjohne@cs.stanford.edu
;;;
;;;  A Ground Control Agent
;;;
;;;  Ground Control is used in the transport planes scenario to direct
;;;  the planes to fly to certain locations.


(defagent control
	:timegrain 10
	:beliefs
	'()
;;; (1 (at 100 100)) (1 (cmt (inform world (plane p1 100 100)))))
	:commit-rules
	'(

	  )
	)

