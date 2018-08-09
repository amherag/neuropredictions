
;;;  TEST
;;;
;;;  Test functions and structures for AOP


;;;  for <AGENT> prompt:

state [80, not foo()]

bel? [100, foo()]

inform joetriv [100, foo()]



(send-message 'request 'joe-triv 'agent 
	      (make-action :type 'inform 
			   :time (make-time :universal (get-universal-time))
			   :agent 'agent
			   :fact (list (make-time :universal (get-universal-time))
				       (list 'foo))))

(send-message 'inform 'joe-triv 'agent
	      (make-fact
	       :pred (list 'foo)
	      :time (make-time :universal (get-universal-time))
	      :agent 'agent
	      :fact (list (make-time :universal (get-universal-time))
			  (list 'foo))))


;;;  THIS IS OLD STUFF

(beliefs-add-fact (make-fact-inform :time (make-time :universal 1000)
				    :pred '(p (on a b))))
			     

(beliefs-add-fact (make-fact-inform :time (make-time :universal 1000)
				    :pred '(p (not (on a b))))
		  


;;;  Worries:

#|

when you send an inform message, what exactly is its content?  Is it a fact?  What about the
time?  I think it should be a fact consisting of a predicate and a single time at which the
predicate became true (false if predicate has (not ..) around it).  It could be called a
fact-inform.


|#


