
;;;  LOAD.CL
;;;
;;;

;;;  Loader for MCT, Mark's CLX Toolkit


(defvar *mct-load-path* "/u1/nobotics/torrance/Clx/mct/")


;;;  STRCAT
;;;
;;;  Concatenate two strings resulting in a string

(defun strcat (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(defun mct-load (file)
  (load (strcat *mct-load-path* file)))

(defun mct-compile (file)
  (compile-file (strcat *mct-load-path* file))
  (mct-load file))


(defun mct-load-all ()
  (mct-load "event")
  (mct-load "widgets")
  (mct-load "window"))

(defun mct-load-lisp ()
  (mct-load "event.cl")
  (mct-load "widgets.cl")
  (mct-load "window.cl"))

(defun compile-mct ()
  (mct-compile "event")
  (mct-compile "widgets")
  (mct-compile "window"))
