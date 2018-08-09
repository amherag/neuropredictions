
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

(defagent joetriv
    :timegrain 10			; in seconds (not used yet)
    :beliefs '((2881906857 (i_am_cool))
	       (2881906857 (on a b)))
    :commit-rules
    '( (nil
	(and (B (now (not (i_am_cool))))
	     (not (B (now (i_am_stupid)))))
	?agent
	(do now (becool))) )
    )


;;;  PRIVATE ACTIONS
;;;
;;;  (BECOOL)
;;;  Prints a message of coolness on the screen

(defun becool ()
  (format t "~&Yo, dudes, I am the cool Joe Triv Agent. ~%"))
