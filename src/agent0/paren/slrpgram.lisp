;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: ParEn -*-
;;;=====================================================================
;;;
;;;  		 ParEn (tm) Common LISP Parser Environment
;;;
;;;  Unpublished-rights reserved under the copyright laws of the United
;;;  States.
;;;  
;;;  This data and information is proprietary to, and a valuable trade
;;;  secret of, SRI International.  It is given in confidence by SRI
;;;  International.  Its use, duplication, or disclosure is subject to
;;;  the restrictions set forth in the License Agreement under which it
;;;  has been distributed.
;;;				    
;;;
;;;	    Unpublished Copyright (c) 1987, SRI International
;;;	         ParEn is a Trademark of SRI International
;;;
;;;=====================================================================


;;;================================================================================
;;;                 SLRPGRAM.lisp
;;;
;;;                  Sandy Wells
;;;
;;;               Converted to Common LISP by
;;;                  Barney Pell
;;;
;;; converted:  8/13/87 10:33:28
;;;
;;; modified:   8/17/87 6:46:50      Read augmented grammars only.  If
;;; 				     don't want  lambdas, use NIL to
;;; 				     fill space.   BP
;;; 		3/21/88 18:39:46     Create AUGMENTED-START production,
;;; 				     using the first production in the
;;; 				     grammar as the *START-SYMBOL*.
;;; 				     Also A-S lambda.  BP
;;;		3/22/88 15:58:32     Default-lambda now builds a
;;;				     simple parse tree using *LHS* from
;;;				     LR-PARSE.  BP
;;;		4/12/88 18:55:01     Lambdas use symbols from USER
;;;				     package so that ParEn need not be around.
;;;		4/13/88 17:13:57     PERHAPS we should describe the
;;;				     representation of metagrammars
;;;				     also/instead of basic ones.
;;;						  
;;;             Jun 13, 1988 -- Sandy changed names of internal grammar symbols to have bang
;;;
;;;================================================================================


;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :slrpgram)
(in-package :ParEn
	    :use '(:lisp :user))

(require :util
	 (lookup-pathname :util))

(require :oset
	 (lookup-pathname :oset))

(require :g-symbol
	 (lookup-pathname :g-symbol))

;;; (export '(*trace-lr-table-generator*))
;;; Jun 23, 1988 -- Sandy.  Punted exporting this guy.

;;; SLRPGRAM.LISP

;;; About the LISP REPRESENTATIONS OF GRAMMARS in files:
;;; NON TERMINALS are represented by lisp symbols, TERMINALS by
;;; lisp symbols, or strings. for example:
;;;
;;; A ::= B | C | "foo" | "c" | !the-empty-string
;;; would be encoded:
;;; (A B)
;;; (A C)
;;; (A "foo")
;;; (A "c")
;;; (A !the-empty-string)

;;; the grammer should start with the non-terminal <START>
;;; ex:
;;; (<START> E)
;;; (E E "+" T)
;;; (F IDENTIFIER)

;;; Stuff to read in a file containing a grammar
;;; use SLURP-GRAMMAR to internalize a grammar in the above syntax
;;; *PRODUCTIONS* holds a list of all the productions
;;; *NON-TERMINALS* holds a list of all the non-terminals
;;; Each non-terminal symbols has a list of the productions it
;;; appears in the left hand side of under its OWN-PRODUCTIONS
;;; property
;;; *G-SYMBOL-ALIST* holds an alist whose cars are the string or symbol
;;;   which is read from the grammar, and whose cdrs hold corresponding
;;;   g-symbol structures; the order is in the reverse sense of *SYMBOL-ARRAY*

(defvar *symbols*) ; a list of the grammar symbols
(defvar *preterms*) ; a list of the preterminal grammar symbols
(defvar *symbol-array*) ; indexed by the symbol's index, of g-symbols
(defvar *productions*)
(defvar *production-count*)
(defvar *g-symbol-count*)
(defvar *g-symbol-alist*)
(defvar *start-symbol*)
(defvar *empty-string-g-symbol*)
(defvar *augmented-start-g-symbol*)
(defvar *the-end-g-symbol*)
(defvar *lambdas*) ; the list (in reverse order) of the lambda functions for each production.
(defvar *trace-lr-table-generator* nil)


;;; INITIALIZE-GRAMMAR
;;; ==================
;;;
;;;	Initialize  global variables.

(defun initialize-grammar ()
       (setf *symbols* nil)
       (setf *productions* nil)
       (setf *production-count* 0)
       (setf *g-symbol-count* 0)
       (setf *g-symbol-alist* nil)
       (setf *start-symbol* nil)
       (setf *lambdas* nil)
)


;;; LOOKUP-G-SYMBOL
;;; ===============
;;;
;;;	Sort of like interning.  Returns a g-symbol.  string-or-symbols
;;;	return identical g-symbols.   If the object is a symbol, it is
;;;	stringified to remove package information so that the fn is
;;;	package insensitive.  Note that if not a strong or a symbol, no
;;;	stringification is done, but the pure equal test is still in
;;;	force.

(defun lookup-g-symbol (string-or-symbol)
  (when (symbolp string-or-symbol)
    (setf string-or-symbol (string string-or-symbol)))
  (let ((whatever (assoc string-or-symbol *g-symbol-alist* :test #'equal)))
    (if whatever (cdr whatever)
	nil)))


;;; PROCESS-SYMBOL
;;; ==============
;;;
;;;	Creates a cell in the *g-symbol-alist* which looks like:
;;;	(STRING-OR-SYMBOL G-SYMBOL), where STRING-OR-SYMBOL is the way
;;;	this symbol is read from the grammar, and G-SYMBOL is this
;;;	converted into a grammar symbol.  Then adds this symbol to the
;;;	list of symbols.

(defun process-symbol (string-or-symbol)
  (let ((symbol (lookup-g-symbol string-or-symbol)))
    (if symbol
	symbol
	(let* ((symbol-name 
		 (if (symbolp string-or-symbol)
		     (string string-or-symbol)
		     string-or-symbol))
	       (new-symbol
		 (new-g-symbol symbol-name
			       (post-inc *g-symbol-count*))))
	  (push (cons symbol-name new-symbol) *g-symbol-alist*)
	  (push new-symbol *symbols*)
	  new-symbol))))




(defsyntax (lhs prod) (cadr prod))
(defsyntax (rhs prod) (cddr prod))

;;; handy macro

(defsyntax (production-index production)
        (car production))


;;; PROCESS-PRODUCTION
;;; ==================
;;;
;;;	Do various things, fixing up global data structures and
;;;	fields of grammar symbols.
;;;	About the internal representation of productions: they are
;;; 	a list of the production index followed by the lhs
;;; 	and then the rhs symbols represented as g-symbols.

(defun process-production (input-production)
       (let ((production
               (cons (post-inc *production-count*)
                     (mapcar #'process-symbol input-production))))
         (push production *productions*)
	 (when *trace-lr-table-generator*
	   (format t "~&Processing production ")
	   (print-production production))
         (when (eq (lhs production) *augmented-start-g-symbol*)
               (unless (equal 2 (length input-production))
                       (error "<start> should only derive one symbol"
                              input-production))
               (unless (null *start-symbol*)
                       (error "<start> on lhs of more than one prod"
                              input-production))
               (when *verbose*
		 (format t "~&Start symbol: ~a.~%"
			 (g-symbol-print-name (car (rhs production)))))
               (setf *start-symbol* (car (rhs production))))
         (g-symbol-add-production (lhs production) production)
         (let ((rhs-symbol-set (make-oset :order-fn #'g-symbol-order-function)))
           (mapc #'(lambda (gs)
                   (oset-insert! gs rhs-symbol-set))
                 (rhs production))
           (oset-for-each #'(lambda (gs)
                            (push production (g-symbol-rhs-productions gs)))
                          rhs-symbol-set))))



;;; *DEFAULT-LAMBDA*
;;; ================
;;;
;;;	Stores the client lambda expression to use when the user didn't
;;;	give one, or gave NIL.  This lambda generates a simple parse
;;;	tree, using the *LHS* global variable which is bound in
;;;	LR-PARSE.  Uses symbols in the USER package so that the files
;;;	can be used with ParEn.

(defvar *default-lambda*
	'(function (lambda (&rest user::rhs)
	   (declare (special *lhs*))
	   (cons *lhs* user::rhs))))


;;; PROCESS-LAMBDA
;;; ==============
;;;
;;;	Process a lambda function, by adding it to the *lambdas*, the
;;;	list of lambda functions.  If the function is nil, add the
;;;	lambda function which returns nil regardless of parameters.

(defun process-lambda (input-lambda)
  (if (null input-lambda)
      (push *default-lambda* *lambdas*)
      (push input-lambda *lambdas*)))


;;; PROCESS-PRETERMS
;;; ================
;;;
;;;	Processes the list of preterminal atoms, which is the
;;;	first list read in from the grammar.  Given the list, this
;;;	function sets *preterms* to be the list of symbols which now
;;;	represent the preterminal grammar symbols. 

(defun process-preterms (preterms)
  (setf *preterms*
	(mapcar #'process-symbol preterms)))


;;; READ-PRETERMS
;;; =============
;;;
;;;	Read the list of preterminal atoms from stream, and process the
;;;	list.  This function assumes that it will be called as the first
;;;	read, so that the preterm-list is the first expression in the
;;;	grammar.

(defun read-preterms (&optional (stream nil))
  (let ((preterms (read stream nil :done)))
    (process-preterms preterms)))


;;; READ-PRODUCTIONS-AND-LAMBDAS
;;; ============================
;;;
;;;	Read the productions into LR package, and lambdas into USER package,
;;;	processing as we go.  Return :done when there is nothing left to
;;;	read.

(defun read-productions-and-lambdas (&optional (stream nil))
  (read-first-production-and-lambda stream)
  (loop
     (let* (;; Read production
	    (prod (read stream nil :done))
	    ;; Read the associated function
	    (lamb (read stream nil :done)))
	 (if (eq prod :done)
	     (return :done)
	     (progn (process-production prod)
		    (process-lambda lamb))))))



;;; READ-FIRST-PRODUCTION-AND-LAMBDA
;;; ================================
;;;
;;;	Reads and processes the first production and lambda function.
;;;	Creates the <START> production, using the LHS of the
;;;	first production as the START-SYMBOL.  Creates the
;;;	AUGMENTED-START lambda, which just returns whatever the start
;;;	production returns.

(defun read-first-production-and-lambda (&optional (stream nil))
  (let* (;; Read production into the LR package
	 (prod (read stream nil :done))
	 ;; Read the associated function into the USER package
	 (lamb (read stream nil :done))
	 ;; Add the production:  <START> --> START-SYMBOL
	 ;; where START-SYMBOL is the LHS of the first production.
	 (augmented-start-production (list '<start>
					   (car prod)))
	 (augmented-start-lambda '#'(lambda (user::start)
				      user::start)))	;Needs
						;to be sent unevaluated.
						; Thus '#'.  BP

    ;; Process the augmented start production and lambda
    (process-production augmented-start-production)
    (process-lambda augmented-start-lambda)
    
    ;; Now process the first entered production
    (process-production prod)
    (process-lambda lamb))) 




;;; SLURP-GRAMMAR
;;; =============
;;;
;;; 	Internalize a grammar in the lisp syntax described above.   Set up
;;;	data structures as described above.   Every grammar interns the empty
;;;	string and <start> as grammar symbols.

(defun slurp-grammar (filename)
  (initialize-grammar)
  (setf *empty-string-g-symbol* (process-symbol '<empty>))
  (setf *augmented-start-g-symbol* (process-symbol '<start>))
  (setf *the-end-g-symbol* (process-symbol '<$>))
  ;; When reading the lisp syntax grammar file, we use the user package
  ;; since it was written out relative to user.
  (let ((*package* (find-package :user)))
    (with-open-file (stream filename)
      (read-preterms stream)
      (read-productions-and-lambdas stream))
    (when *verbose*
      (format t "~&~d productions, ~d symbols read.~%"
	      *production-count* *g-symbol-count*))
    (setf *symbol-array* (list->vector (reverse *symbols*)))
    (if (null *start-symbol*) (error "no start symbol" nil))))


;;; PRINT-PRODUCTION
;;; ================
;;;
;;;	

(defun print-production (prod &optional (stream t))
  (format stream "~a : ~a -> ~{~a ~}"
	  (car prod)			
	  (g-symbol-print-name (lhs prod))
	  (mapcar
	    #'(lambda (x)
		(g-symbol-print-name x))
	    (rhs prod))
	  nil))

;;; PRINT-PRODUCTIONS
;;; =================
;;;
;;;	

(defun print-productions (&optional (stream t))
  (format stream "~&~%")	
  (mapc
    #'(lambda (x)
	(progn
	  (format stream "~&")	      
	  (print-production x stream)))	
    (reverse *productions*))
  (format stream "~%~%")
  nil)

;;; PRINT-SYMBOLS
;;; =============
;;;
;;;	

(defun print-symbols (&optional (stream t))
  (format stream "~{~&~a~}~%" 
	  (reverse *symbols*))
  nil)

;;; PRINT-OWN-PRODUCTIONS
;;; =====================
;;;
;;;	

(defun print-own-productions (sym &optional (stream t))
  (format stream "~&")
  (mapc
    #'(lambda (x)
	(progn
	  (format stream "~&     ")		;indent productions
	  (print-production x stream)))
    (g-symbol-own-productions sym))
  (format stream "~%")
  nil)


;;; PRINT-RHS-PRODUCTIONS
;;; =====================
;;;
;;;	

(defun print-rhs-productions (sym &optional (stream t))
  (format stream "~&")
  (mapc 
    #'(lambda (x)
	(progn
	  (format stream "~&     ")
	  (print-production x stream)))
    (g-symbol-rhs-productions sym))
  (format stream "~%")
  nil)


;;; CRUISE-SYMBOLS-0
;;; ================
;;;
;;;	For each symbol, print its index, name, own productions (if
;;;	any), and rhs productions (if any).  If the symbol is terminal
;;;	or preterminal, say so.  Output defaults to screen, but can be
;;;	sent to any stream.

(defun cruise-symbols-0 (&optional (stream t))
  (format stream "~&~%~%index : symbol-name~%====================~%~%")	
  (mapc
    #'(lambda (sym)
	(format stream "~a : ~A~%"
		(g-symbol-index sym)
		(g-symbol-print-name sym))
	(cond
	  ((g-symbol-own-productions sym)
	   (progn
	     (format stream "~&~%own productions:~%")	;
	     (print-own-productions sym stream)))
	  ((member sym *preterms*)
	   (format stream "~&~%     ***pre-terminal symbol***~%"))
	  (t
	   (format stream "~&~%     ***terminal symbol***~%")))
	(format stream "~%----------------------------~%"))
    (reverse *symbols*))
  nil)


;;; CRUISE-SYMBOLS-1
;;; ================
;;;
;;;	For each symbol, print its index, name, own productions (if
;;;	any), and rhs productions (if any).  Output defaults to screen,
;;;	but can be sent to any stream.

(defun cruise-symbols-1 (&optional (stream t))
  (format stream "~&~%~%index : symbol-name~%====================~%~%")
  (mapc
    #'(lambda (sym)
	(format stream "~a : ~A~%"
		(g-symbol-index sym)
		(g-symbol-print-name sym))
	(when (g-symbol-own-productions sym)
	  (format stream "~&~%own productions:~%")
	  (print-own-productions sym stream))
	(when (g-symbol-rhs-productions sym)
	  (format stream "~&~%rhs productions:~%")
	  (print-rhs-productions sym stream))
	(format stream "----------------------------~%"))
    (reverse *symbols*))
  nil)

;;; CRUISE-SYMBOLS-2
;;; ================
;;;
;;;	For each symbol, print its index and name.

(defun cruise-symbols-2 (&optional (stream t))
  (format stream "~&~%~%index : symbol-name~%====================~%~%")
  (mapc #'(lambda (sym)
	    (format stream "~&~a : ~A~%"
		    (g-symbol-index sym)
		    (g-symbol-print-name sym)))
	(vector->list *symbol-array*))
  nil)


;;; (compile2 "slrpgram")
;;; (slurp-grammar "gram1")
;;; (print-symbols)
;;; (cruise-symbols)
;;; (cruise-symbols-2)




