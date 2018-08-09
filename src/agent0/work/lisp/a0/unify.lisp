;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *unify-var-bindings* nil)

(defstruct inequality
  operation
  value
  instantiation)

(defun <> (a b)
  (not (= a b)))

(defconstant *inverses*
  (list (cons #'=  #'= )
	(cons #'< #'>=)
	(cons #'>= #'<)
	(cons #'<= #'>)
	(cons #'> #'<=)
	(cons #'<> #'<>)))
	       
(defun operation-invert (func)
  (cdr (assoc func *inverses*)))

(defun unify (s1 s2 bindings)
  ;; Return: 'fail if the lists cannot be unified, and either the
  ;;    list of binding or nil otherwise.
  (let ((*unify-var-bindings* bindings))
    (labels ((unify1 (struct1 struct2)
	       (cond
		 ((var-p struct1)
		  (cond  ((unify-var-boundp struct1)
			  (unify1 (unify-var-lookup struct1) struct2))
			 (t
			  (unify-var-bind struct1 struct2)
			  (throw 'success 't))))
		 ((var-p struct2)
		  (cond  ((unify-var-boundp struct2)
			  (unify1 (unify-var-lookup struct2) struct1))
			 (t
			  (unify-var-bind struct2 struct1)
			  (throw 'success 't))))
		 ((or (atom struct1) (atom struct2))
		  (when (equal struct1 struct2) (throw 'success 't)))
		 (t
		  (and 
		   (catch 'success (unify1 (car struct1) (car struct2)))
		   (catch 'success (unify1 (cdr struct1) (cdr struct2)))
		   (throw 'success 't))))))
      (if
       (catch 'success
	 (unify1 s1 s2))
       *unify-var-bindings*
       'fail))))

(defun add-inequality (inequality bindings)
  (cons inequality bindings))

(defun unify-get-value (value bindings)
  "Completely resolve a variable reference."
  (cond ((var-p value)
	 (unify-get-value (unify-bond-expression value bindings) bindings))
	(t
	 value)))

(defun unify-bond-expression (expression bindings)
  (let ((*unify-var-bindings* bindings))
    (cond ((null expression) expression)
	  ((and (var-p expression)
		(unify-var-boundp expression))
	   (unify-bond-expression (unify-var-lookup expression) bindings))
	  ((var-p expression)
	   expression)
	  ((atom expression) expression)
	  ((consp expression) 
	   (cons (unify-bond-expression (car expression) bindings)
		 (unify-bond-expression (cdr expression) bindings))))))

(defun unify-non-variable-p (expression)
  (cond ((null expression) t)
	((var-p expression) nil)
	((atom expression) t)
	((consp expression) 
	 (and (unify-non-variable-p (car expression))
	       (unify-non-variable-p (cdr expression))))))

;(defun make-var (var) var)
;(defun var-p (var) (and (symbolp var) (eq (aref (symbol-name var) 0) #\?)))
;(defun var-name (var) (subseq (symbol-name var) 1))

(defstruct (var (:print-function print-var))
  name)

;;; For some reason AKCL seems brain damaged and will not print out
;;; the structure without adding extra cr's.  This keeps the output
;;; from looking good.

(defun print-var (var stream depth)
  (if (and (not (null *print-level*)) (< *print-level* depth))
      (format stream "")
      (format stream "?~a" (var-name var))))

(set-dispatch-macro-character
    #\# #\?
    #'(lambda (stream char foo) (make-var :name (read stream t nil t))))

(defun unify-var-boundp (variable)
  ;; Is this 
  (not (null (cdr (assoc (var-name variable) *unify-var-bindings*)))))

(defun unify-var-lookup (variable)
  (or (cdr (assoc (var-name variable) *unify-var-bindings*))
      variable))

(defun unify-var-bind (variable value)
  (push (cons (var-name variable) value) *unify-var-bindings*))


