;;;  PLANE.LISP
;;;  George John
;;;  gjohn@cs.stanford.edu
;;;
;;;  An Airplane Agent
;;;
;;;  Broadly, a transport plane that is to respond to directives from
;;;  ground control.  The plane will fly somewhere if it is requested
;;;  to do so, dodge friendly aircraft, land when fuel is low, land
;;;  when enemy aircraft are near.  

(defagent plane
  :timegrain 10
  :beliefs '((1 (at 100 100))
             (1 (max-speed 5))  
             (1 (CMT plane plane (INFORM 2 world (2 (plane p1 100 100))))))
  
  :commit-rules
  '(
    
    ;;If I am requested to be at a certain place at a certain time,
    ;;then I do the private action cap-check, to see if I am capable
    ;;of performing the requested action.  If so, I commit to perform
    ;;the action.  If not, nothing happens.
    ( (control REQUEST (DO ?time (be-at ?gx ?gy)))
      ()
      control
      (DO now (cap-check ?time 'be-at ?gx ?gy)) )


    ;;If the control tower changes its mind, then I uncommit to
    ;;fly wherever it requested me to fly
    ;;( (control UNREQUEST (DO ?time (be-at ?gx ?gy)))
    ;;  (CMT control (DO ?time2 (be-at ?gx ?gy)))   
    ;;   plane
    ;;  (DO now (uncommit ?time2 'be-at ?gx ?gy)) )
    
    
    ;;If I believe I am low on fuel and I believe I am committed to
    ;;control to fly to ?z1 ?z2 at time ?time2 then I want to ask
    ;;control to release me from my committment to fly to (gx,gy).
    ;;( ()
    ;;  (and (B (now (lowfuel))) (CMT ?agent (DO ?time2 (be-at ?z1 ?z2))))
    ;;  plane
    ;;  (REQUEST now control (UNREQUEST now plane (DO ?time2 (be-at ?z1 ?z2)))) )
    
    ;;If I believe I am low on fuel and I believe I am in the air
    ;;and I believe I am not committed to anyone to fly anywhere
    ;;then I want to ask CONTROL where I can land.
    ;;	  (NIL
    ;;	   (and (B (now (lowfuel))) (not (CMT ?agent (DO ?time2 (be-at ?z1 ?z2)))))
    ;;	   plane
    ;;	   (DO now (land)) )
    
    ;;If I am informed of a safe landing spot at (slx,sly) and
    ;;I believe I am at (x,y) and I believe I am low on fuel,
    ;;I want to fly to that safe landing spot in 2 minutes.
    ;;	  ( (?agent INFORM (?time (safelanding ?slx ?sly)))
    ;;          (B (?time (lowfuel)))
    ;;	    plane
    ;;	    (REQUEST now plane (DO now (be-at ?slx ?sly))) )
    
    ;;If I believe a plane on my radar is not friendly then
    ;;I want to be released from my commitment to fly wherever
    ;;I'm heading. 	
    ;;	(NIL
    ;;	 ((B (now (lowfuel))) (B (now (not (committed))))
    ;;	  (CMT ?agent (DO ?time2 (be-at ?z1 ?z2))))
    ;;	  plane
    ;;	  (REQUEST now ?agent (UNREQUEST now plane (DO ?time2 (be-at ?z1 ?z2)))) )
    
    ;;If I believe a plane is on my radar and I do not believe
    ;;that the plane is friendly, then I want to find the
    ;;nearest airstrip
    ;;	(
    ;;	(B [now onradar(?plane)]) (B [now unfriendly(?plane)])
    ;;	plane
    ;;	REQUEST(now control DO(now helpland(plane))) )
    
    ;;If I am informed of a safe landing spot at (slx,sly) and
    ;;if I believe that an unfriendly plane is on radar then
    ;;I want to fly to that safe landing spot soon
    ;;( (?agent INFORM [?time safelanding(?slx ?sly)])
    ;;  (B [now onradar(?plane)]) (B [now unfriendly(?plane)])
    ;;  plane
    ;;  DO(?time be-at(?slx ?sly)) )
    
    )					;ends commitment rules
  
  )					;ends defagent



;;;private actions for the plane agent.
;;;
;;;

(defvar *move-step* 5)

(defun fly(x y gx gy)
  (cond ( (or (not (equal x gx)) (not (equal y gy)))
	  (let ((dx (- gx x)) (dy (- gy y)))
	    (let ((dist (/ (distance dx dy) *move-step*)))
	      (cond ((< dist 1)
		     (print "here!")
		     (inform 'PLANE (list 'at gx gy) (now))
		     (inform 'WORLD (list 'plane 'p1 gx gy) (now)))
		    (t (let ((unitdx (/ dx dist)) (unitdy (/ dy dist)))
			 (inform-fact 'PLANE `(now (at ,(+ unitdx x) ,(+ unitdy y))))
			 (inform 'WORLD `(not (plane p1 ,x ,y)) (now))
			 (inform-fact 'plane
				      `(now (cmt plane plane
						 (inform now world (now (not (plane p1 
									  ,x
									  ,y)))))))
			 (inform-fact 'plane
				      `(now (cmt plane plane
						 (inform now world (now (plane p1 
									  ,(+ unitdx x)
									  ,(+ unitdy y)))))))
;;(inform 'WORLD (list 'plane 'p1 (+ unitdx x) (+ unitdy y)) (now))
;;(inform 'PLANE (list 'cmt 'plane 'plane `(do now (fly ,x ,y ,gx ,gy))) (now))
			 ))))))
	( (and (equal x gx) (equal y gy))
	  ())
	(t (print "nowhere to go, nothing to do")))
  ())


;;; CAP-CHECK : given the committment under consideration to be-at
;;; some location gx gy at time time, look at all other beliefs
;;; about committments and see if they preclude commiting to the
;;; new action.  Only works for be-at actions.
(defun cap-check (time act gx gy)
  (let* ((whereamI (get-belief 'plane '(at #?x #?y)))
        (x (cdr (assoc 'x whereamI)))
        (y (cdr (assoc 'y whereamI)))
        (all-cmts (get-all-beliefs 
                   'plane 
                   '(cmt plane #?agent 
                     (action do #?time () () () (private-action #?p) nil))))
        (ok t))
    (format t "~%checking nowat ~d ~d goalat ~d ~d" x y gx gy)
    (if (< (distance (- gx x) (- gy y))
           (* (- time (now)) *move-step*))
      (progn 
        (dolist (one-cmt all-cmts)
          (let ((prev-cmt (cdr (assoc 'p one-cmt)))
                (prev-time (cdr (assoc 'time one-cmt))))
            (if (equal (car prev-cmt) 'be-at)
              (if (not (capable-of (cons (car prev-cmt)
                                         (cons prev-time
                                               (cdr prev-cmt)))
                                   (list act time gx gy)))
                (setf ok nil)))))
        (if ok
          (progn
            (make-distance (* (- time (now)) *move-step*) gx gy)
            (inform-fact 'plane `(now (cmt control plane
                                           (do ,time (be-at ,gx ,gy)))))))))))



;;; CAPABLE-OF : given an action under consideration and some
;;; previous action already committed to, returns t or nil
;;; depending on whether the agent is capable of performing
;;; the first action given its committment to the previous one.
;;; NOTE: this function only works with be-at actions.
(defun capable-of (action previous-cmt)
  (> (- (second action) (second previous-cmt))
     (distance (- (third action) (third previous-cmt))
               (- (fourth action) (fourth previous-cmt)))))






;;; BE-AT is a "dummy" private action -- it's really a persistent
;;; goal rather than a private action, so we do nothing but we
;;; must define it anyway
(defun be-at (x y) )


				
				
				
(defun make-distance (dist gx gy)
;;  (format t "~%Make-Distance ~d to ~d ~d" dist gx gy)
  (let* ((whereamI (get-belief 'plane '(at #?x #?y)))
	 (x (cdr (assoc 'x whereamI)))
	 (y (cdr (assoc 'y whereamI)))
	 (distnow (distance (- gx x) (- gy y))))
    (if (> distnow dist)
	(progn
	  (format t "~%FLYING Nowdst ~D Goaldist ~D" distnow dist)
	  (fly x y gx gy)))
    (format t "~%DISTNOW = ~d" distnow)
    (if (> distnow 1)
          (inform-fact 'plane 
                       `(now (cmt plane 
                                  plane 
                                  (do ,(+ (now) 1) (make-distance ,(- dist 5)
                                                                  ,gx ,gy)))))
          ))
  ())
		

(defun plan-trip (now then gx gy)
  (make-distance (* (- then now) *move-step*) gx gy))
    
(defun distance (dx dy)
  (sqrt (+ (* dx dx) (* dy dy))))


				
;;; misc:
;;; (agent-name *current-agent*)