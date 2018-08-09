(ql:quickload :daemon)

(daemon:daemonize :exit-parent t)

(ql:quickload :neuropredictions)
(neuropredictions:start :port 8080)

(quote
 (with-open-file (out #P "/tmp/daemonlog" :direction :output :if-exists :supersede)
   (format out "~A ~A~%~A~%"
           (lisp-implementation-type)
           (lisp-implementation-version)
           (daemon::getpid))))

(loop (sleep 5))
;; (daemon:exit)

;; sbcl --load script.lisp
;; ps axo user,pid,ppid,command | grep "sbcl"
;; kill -9 PID
