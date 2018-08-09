

;;;  TEST.CL
;;;
;;;

;;;  Test functions for MCT, Mark's CLX Toolkit
;;;
;;;
;;;
;;;  Uses functions:
;;;
;;;  mct-install-window button vframe hframe label edit text



(defun my-quit-window (w)
  (format t "Pressed QUIT ~%")
  (mct-close-window w))


(setq belieftext
"(ON A B)   [.. U] [11:35:16 T]
(I_AM_COOL)   [.. U] [11:35:16 T] [11:37:43 F]")

(setq cmtruletext
"MSGCOND:  (?AGENT1 REQUEST (DO ?TIME1 (BECOOL))))
MNTLCOND: (B (?TIME1 (I_AM_COOL)))
AGENT:    ?AGENT1
ACTION:   (DO ?TIME1 (BECOOL))")

(setq cmttext
"Agent (do 16:03:55 (becool)) [.. U] [11:35:16 T]")

(setq messagetext
"Agent REQUEST Joetriv (do 16:03:55 (becool))")




(defun my-quit (w)
  (setq *quit* *event-level*))

;;;  MY-KILL
;;;
;;;  Kill off the agent and close his window

(defun my-kill (w)
  (mct-close-window w))

(defun my-edit-down (w)
  (format t "Editing... "))

(defun my-edit-up (w)
  (format t "          done~%"))


(defun test ()
  (let* ((beliefs (stext :getf #'(lambda (w) belieftext)
			 :font *small-font* :lines 5))
	 (cmtrules (stext :getf #'(lambda (w) cmtruletext)
			  :font *small-font* :lines 5))
	 (cmts (stext :getf #'(lambda (w) cmttext)
			  :font *small-font* :lines 2))
	 (messages (stext :getf #'(lambda (w) messagetext)
			  :font *small-font* :lines 2))
	 (load (button " Load "))
	 (kill (button " Kill " :upf #'my-kill))
	 (editbutton (button " Edit " :downf #'my-edit-down
			     :upf #'my-edit-up
			     :expand-x t))
	 (loadbutton (button " Load " :expand-x t))
	 (cmtctrls (vframe editbutton (space 3) loadbutton))
	 (cmdframe (hframe (espace) load (space 20) kill (espace)))
	 (main (vframe
		(space 5) cmdframe (space 5)
		(hframe (espace) (label "Beliefs") (espace))
		beliefs (space 5)
		(hframe (espace) (label "Commitment Rules") (espace))
		(hframe cmtrules cmtctrls (space 5)) (space 5)
		(hframe (espace) (label "Commitments") (espace))
		cmts (space 5)
		(hframe (espace) (label "Messages") (espace))
		messages (space 5))))
    (mct-install-window
     :name "Joetriv"
     :x 500 :y 350
     :kid main)))



(defun test3 ()
  (let* ((up-button (button :name "^" :function #'my-start))
	 (down-button (button :name "V" :function #'my-start))
	 (quit-button (button :name " Quit Agent0 "
			      :function #'my-quit-window))
	 (arrow-frame
	  (vframe :kids (list (space 3) up-button (space 10) down-button)))
	 (text (text :getf #'(lambda (w) pane-text)))
	 (main (vframe :kids (list (space 3)
				   (hframe :kids (list (space 3)
						       quit-button
						       (space 3)
						       (label "Hi there")))
				   (space 5)
				   (hframe :kids
					   (list (space 3)
						 arrow-frame
						 (space 3)
						 text
						 (space 3)))
				   (space 3))))
	 )
    (mct-install-window
     :name "My-window"
     :background *grey50*
     :foreground *white*
     :kid main)))









;	 (edit (edit :getf #'(lambda (w) field-val)
;		     :setf #'(lambda (x) (setf field-val x))))
;	 (edit-frame
;	  (vframe :fill t :kids (list (label "Edit: ") (space 3) edit)))
;

(defun test2 ()
  (let ((start-button (button :name " Start " :function #'my-start))
	(quit-button (button :name " QUIT " :function #'my-quit-window))
	(text (text :getf #'(lambda (w) pane-text)))
	(start-label (label "Here we go!!!")))
    (mct-install-window
     :name "My-other-window"
     :background *grey50*
     :foreground *white*
     :kid (vframe :fill t :kids (list start-button quit-button text)))))

