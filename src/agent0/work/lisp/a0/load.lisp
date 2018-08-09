
;;;  LOAD
;;;
;;;  Loader file for the AOP interpreter


(setq *load-verbose* nil)
(setq *aop-load-path* "/u1/nobotics/aop/lisp/a0/")

;;;  *aop-agent-path* is a string which is the path for aop agent programs.
;;;  Programs written in agent0 should have filenames ending with
;;;  .aop, and may have Lisp support files ending with .lisp.  Both
;;;  types of files should be stored in the directory given here:

(setq *aop-agent-path* "/u1/nobotics/aop/lisp/agents/")
(setq *compiled-ext* ".fasl")


(defun strcat (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(setq *lisp-ext* ".lisp")

(defun aop-load (filename)
  (if (probe-file (strcat *aop-load-path* filename *compiled-ext*))
      (if (> (file-write-date (strcat *aop-load-path* filename *lisp-ext*))
	     (file-write-date (strcat *aop-load-path* filename
				      *compiled-ext*)))
	  (load (strcat *aop-load-path* filename *lisp-ext*))
	(load (strcat *aop-load-path* filename *compiled-ext*)))
    (if (probe-file (strcat *aop-load-path* filename *lisp-ext*))
	(load (strcat *aop-load-path* filename *lisp-ext*)))))

;;;  Load user personal setup

(if (probe-file "~/.a0") (load "~/.a0"))

(aop-load "unify")
(aop-load "structures")
(aop-load "lisp-parser")
(aop-load "interpreter")
(aop-load "user-interface")
(aop-load "x-graphics")

(format t "\
,____________________________________________________________________\
|                                                                    |\\\
|                          AOP Version 1.0                           | |\
|               by Mark C. Torrance and Paul A. Viola                | |\
|     Copyright (c) 1991 Stanford University  All Rights Reserved    | |\
|                                                                    | |\
|           Type (aop) to run the interactive interpreter            | |\
|               Type help to the prompt to learn more                | |\
|             Type new to the prompt to see new features             | |\
|____________________________________________________________________| |\
 \\____________________________________________________________________\\|\
\
")
