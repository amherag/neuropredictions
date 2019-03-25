(defpackage neuropredictions.plot
  (:use :cl
	:eazy-gnuplot
	:local-time
        :neuropredictions.config)
  (:export ))
(in-package :neuropredictions.plot)

(defparameter *last-pair* nil)
(defun do-all-plots ()
  (cl21:doeach (config (neuropredictions.agents::combinations '(4 10 20) '(4 10 20) '(4 10 20)
							      '("aud_usd" "eur_gbp"
								"eur_usd" "gbp_usd" "usd_cad")))

    (defparameter *file-name* (format nil "~a_h1-~aagents-~arules-~aind-100gen"
				      (nth 3 config) (nth 0 config) (nth 1 config) (nth 2 config)))

    (defparameter *training-error* (ms:unmarshal (read-from-string (neuropredictions.agents::file-get-contents (format nil "results/~a_training_error.txt" *file-name*)))))

    (defparameter *testing-error* (ms:unmarshal (read-from-string (neuropredictions.agents::file-get-contents (format nil "results/~a_testing_error.txt" *file-name*)))))

    (unless (string= *last-pair* (nth 3 config))
      (defparameter *training-times*
	(let ((all-times (mapcar (lambda (tuple)
				   (cdr (assoc :time tuple)))
				 (ms:unmarshal (read-from-string (neuropredictions.agents::file-get-contents (format nil "data/~a.dat" (nth 3 config))))))))
	  (mapcar (lambda (time)
		    (/ (read-from-string time) 1000000))
		  (reverse (subseq (reverse (subseq all-times 0 500)) 0 100)))
	  ))
      (defparameter *testing-times*
	(let ((all-times (mapcar (lambda (tuple)
				   (cdr (assoc :time tuple)))
				 (ms:unmarshal (read-from-string (neuropredictions.agents::file-get-contents (format nil "data/~a.dat" (nth 3 config))))))))
	  (mapcar (lambda (time)
		    (/ (read-from-string time) 1000000))
		  (reverse (subseq (reverse (subseq all-times 100 600)) 0 100)))
	  )))

    (defparameter *last-pair* (nth 3 config))

    (defparameter *training-data* (cdr (cl-csv:read-csv (pathname (format nil "results/~a_training_report.csv" *file-name*)))))
    (defparameter *testing-data* (cdr (cl-csv:read-csv (pathname (format nil "results/~a_testing_report.csv" *file-name*)))))

    (defparameter *error-minimization*
      (mapcar (lambda (tuple)
		(read-from-string (fifth tuple)))
	      *training-data*))

    (defparameter *training-reals*
      (mapcar (lambda (tuple)
		(read-from-string (first tuple)))
	      *training-data*))
    (defparameter *testing-reals*
      (mapcar (lambda (tuple)
		(read-from-string (first tuple)))
	      *testing-data*))

    (defparameter *training-sims*
      (mapcar (lambda (tuple)
		(read-from-string (second tuple)))
	      *training-data*))

    (defparameter *testing-sims*
      (mapcar (lambda (tuple)
		(read-from-string (second tuple)))
	      *testing-data*))

    (defparameter *training-profits*
      (mapcar (lambda (tuple)
		(* 10000 (read-from-string (fourth tuple))))
	      *training-data*))
    (defparameter *testing-profits*
      (mapcar (lambda (tuple)
		(* 10000 (read-from-string (fourth tuple))))
	      *testing-data*))

    ;; training fit
    (with-plots (*standard-output* :debug t)
      (gp-setup :xlabel "Date"
		;;:ylabel '("\"Price\"" :offset "3,0,0")
		:ylabel "Price"
		:output (format nil "plots/~a_training_fit.pdf" *file-name*)
		:terminal :pdf
		:title (format nil "Training Stage (ε=~a)" *training-error*)
		:key '(:bottom :right :font "Times New Roman, 6")
		:pointsize "0.3px")

      (format t "~%set key inside")
      (format t "~%set key center top")
      (format t "~%set xdata time")
      (format t "~%set timefmt \"%s\"")
      (format t "~%set format x \"%d-%m\"")
      (format t "~%set xtics rotate by 270")
      (format t "~%set ytics rotate by 30")
      (format t "~%set ytics font \",8\"")
      (format t "~%set xtics font \",8\"")

      (plot (lambda ()
	      (map nil (lambda (time price)
			 (format t "~&~a ~a" time price))
		   *training-times*
		   *training-reals*))
	    :using '(1 2)
	    :title "Real Prices"
	    :with '(:linespoint))

      (plot (lambda ()
	      (map nil (lambda (time price)
			 (format t "~&~a ~a" time price))
		   *training-times*
		   *training-sims*))
	    :using '(1 2)
	    :title "Simulated Prices"
	    :with '(:linespoint))
      )

    ;; training profits
    (with-plots (*standard-output* :debug t)
      (gp-setup :xlabel "Date"
		;; :ylabel '("\"Units\"" :offset "3,0,0")
		:ylabel "Units"
		:output (format nil "plots/~a_training_profits.pdf" *file-name*)
		:terminal :pdf
		:title "Training Profits"
		:key '(:bottom :right :font "Times New Roman, 6")
		:pointsize "0.3px")

      (format t "~%set key inside")
      (format t "~%set key center top")
      (format t "~%set xdata time")
      (format t "~%set timefmt \"%s\"")
      (format t "~%set format x \"%d-%m\"")
      (format t "~%set xtics rotate by 270")
      (format t "~%set ytics rotate by 30")
      (format t "~%set ytics font \",8\"")
      (format t "~%set xtics font \",8\"")

      (plot (lambda ()
	      (map nil (lambda (time price)
			 (format t "~&~a ~a" time price))
		   *training-times*
		   *training-profits*))
	    :using '(1 2)
	    :title "Profit in Asset Units"
	    :with '(:linespoint))
      )

    ;; testing fit
    (with-plots (*standard-output* :debug t)
      (gp-setup :xlabel "Date"
		;;:ylabel '("\"Price\"" :offset "3,0,0")
		:ylabel "Price"
		:output (format nil "plots/~a_testing_fit.pdf" *file-name*)
		:terminal :pdf
		:title (format nil "Testing Stage (ε=~a)" *testing-error*)
		:key '(:bottom :right :font "Times New Roman, 6")
		:pointsize "0.3px")

      (format t "~%set key inside")
      (format t "~%set key center top")
      (format t "~%set xdata time")
      (format t "~%set timefmt \"%s\"")
      (format t "~%set format x \"%d-%m\"")
      (format t "~%set xtics rotate by 270")
      (format t "~%set ytics rotate by 30")
      (format t "~%set ytics font \",8\"")
      (format t "~%set xtics font \",8\"")

      (plot (lambda ()
	      (map nil (lambda (time price)
			 (format t "~&~a ~a" time price))
		   *testing-times*
		   *testing-reals*))
	    :using '(1 2)
	    :title "Real Prices"
	    :with '(:linespoint))

      (plot (lambda ()
	      (map nil (lambda (time price)
			 (format t "~&~a ~a" time price))
		   *testing-times*
		   *testing-sims*))
	    :using '(1 2)
	    :title "Simulated Prices"
	    :with '(:linespoint))
      )

    ;; testing profits
    (with-plots (*standard-output* :debug t)
      (gp-setup :xlabel "Date"
		;; :ylabel '("\"Units\"" :offset "3,0,0")
		:ylabel "Units"
		:output (format nil "plots/~a_testing_profits.pdf" *file-name*)
		:terminal :pdf
		:title "Testing Profits"
		:key '(:bottom :right :font "Times New Roman, 6")
		:pointsize "0.3px")

      (format t "~%set key inside")
      (format t "~%set key center top")
      (format t "~%set xdata time")
      (format t "~%set timefmt \"%s\"")
      (format t "~%set format x \"%d-%m\"")
      (format t "~%set xtics rotate by 270")
      (format t "~%set ytics rotate by 30")
      (format t "~%set ytics font \",8\"")
      (format t "~%set xtics font \",8\"")

      (plot (lambda ()
	      (map nil (lambda (time price)
			 (format t "~&~a ~a" time price))
		   *testing-times*
		   *testing-profits*))
	    :using '(1 2)
	    :title "Profit in Asset Units"
	    :with '(:linespoint))
      )

    ;; training error minimization
    (with-plots (*standard-output* :debug t)
      (gp-setup :xlabel "Iteration"
		;; :ylabel '("\"Error\"" :offset "3,0,0")
		:ylabel "Error"
		:output (format nil "plots/~a_error_minimization.pdf" *file-name*)
		:terminal :pdf
		:title "Training Error Minimization"
		:key '(:bottom :right :font "Times New Roman, 6")
		:pointsize "0.3px")

      (format t "~%set key inside")
      (format t "~%set key center top")
      (format t "~%set xtics rotate by 270")
      (format t "~%set ytics rotate by 30")
      (format t "~%set ytics font \",8\"")
      (format t "~%set xtics font \",8\"")
      ;; (format t "~%set format y \"%.2tx10^{%T}\"")
      (format t "~%set format y \"%.2tx10^{%T}\"")

      (plot (lambda ()
	      (map nil (lambda (time price)
			 (format t "~&~a ~a" time price))
		   (cl21:iota (length *error-minimization*))
		   *error-minimization*))
	    :using '(1 2)
	    :title "Error"
	    :with '(:linespoint))
      )
    ))

;; (do-all-plots)
