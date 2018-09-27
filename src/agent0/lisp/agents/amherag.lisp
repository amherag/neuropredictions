(defagent amherag
    :timegrain 10
    :beliefs '()
    :commit-rules
    '(
      (
       (?agent REQUEST (DO ?time (?action)))
       ()
       ?agent
       (do ?time (becool))
       )
      )
    )


;;;  PRIVATE ACTIONS
;;;
;;;  (BECOOL)
;;;  Prints a message of coolness on the screen

(defun becool ()
  (format t "~&Yo, dudes, I am the cool Joe Triv Agent. ~%"))
