;;;  LOAD
;;;
;;;  Loader file for the AOP interpreter

(defpackage :user (:use :cl))
(in-package :user)

(setq *load-verbose* nil)
(setq *warn-if-redefine* nil)
(setq *compiler-warnings* nil)

;; (setq *aop-load-path* "/home/puyol/agents/agent0/lisp/a0/")
(setq *aop-load-path* "/Users/Amherag/Dropbox/hacking/cl/neuropredictions/src/agent0/lisp/a0/")

;;;  *aop-agent-path* is a string which is the path for aop agent programs.
;;;  Programs written in agent0 should have filenames ending with
;;;  .lisp and should be stored in the directory given here:

;; (setq *aop-agent-path* "/home/puyol/agents/agent0/lisp/agents/")
(setq *aop-agent-path* "/Users/Amherag/Dropbox/hacking/cl/neuropredictions/src/agent0/lisp/agents/")
(setq *compiled-ext* ".fasl")


(defun strcat (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(setq *lisp-ext* ".lisp")

(defun aop-load (filename)
  (let ((compiled (strcat *aop-load-path* filename *compiled-ext*))
        (uncompiled (strcat *aop-load-path* filename *lisp-ext*)))
    (if (probe-file compiled)
      (if (> (file-write-date uncompiled)
             (file-write-date compiled))
        (load uncompiled)
        (load compiled))
      (if (probe-file uncompiled)
        (load uncompiled)))))

;;;  Load user personal setup
;;;  On the mac, loads a file a0 from the system folder if
;;;  it exists.  On a unix platform, loads the file ~/.a0 in 
;;;  the user's home directory if it exists.  Currently we only
;;;  use these files to setq the aop and agent paths, but they
;;;  can be used for other customization purposes as well.
#+:ccl
(progn
(setq *mac-harddrive* (pathname-directory (user-homedir-pathname)))
(setq *aop-load-path* (strcat *mac-harddrive* "aop:LISPa0:"))
(setq *aop-agent-path* (strcat *mac-harddrive* "aop:LISPagents:"))
(if (probe-file (strcat *mac-harddrive* "system folder:a0"))
  (load (strcat *mac-harddrive* "system folder:a0")))
)

;(if (probe-file "~/.a0") (load "~/.a0"))


;;;  Load the AGENT0 interpreter.
;;;  Better error handling capabilities exist on the mac, so if
;;;  AGENT0 is being loaded in ccl then we load in the aoperror
;;;  file.
(aop-load "unify")
(aop-load "structures")
(aop-load "lisp-parser")
(aop-load "interpreter")
(aop-load "user-interface")
#+:ccl
(progn
(aop-load "aoperror")
(aop-load "structures")
)

#+(not :ccl)
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
