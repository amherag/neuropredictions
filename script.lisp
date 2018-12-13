(ql:quickload :daemon)

(daemon:daemonize :exit-parent t)

;; (ql-dist:install-dist "http://dists.cl21.org/cl21.txt")
;; (ql:register-local-projects)
;; (push "C:/Users/Amaury/neuropredictions/" asdf:*central-registry*)

(ql:quickload :neuropredictions)
(neuropredictions:start :port 2001)

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
