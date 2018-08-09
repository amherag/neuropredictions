;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ParEn -*-

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


;;;=====================================================================
;;;	A Default Parser based on the General SLR Parser Driver
;;;
;;;		   Stuart M. Shieber and Barney Pell
;;;
;;; written: 8/31/87 19:28:14
;;; modified: 3/25/88 10:49:03 		Parser now takes an optional
;;; 					readtable, which defaults to the
;;; 					standard COMMON LISP readtable
;;; 					(p. 361, Steele CL book).  BP
;;; 	      3/25/88 17:38:59          Wrote initialization functions
;;; 	      				and functions to parse strings
;;; 	      				and files.  Now users don't have
;;; 	      				to write any functions, they
;;; 	      				only need to set global
;;; 	      				variables and call ours.
;;; 	      				(However, we still want to
;;; 	      				automate READTABLE hacking).  BP
;;;
;;;	      5/13/88 16:13:48		Wrote SET-PAREN-READTABLE.  Now
;;;					user's need only call this
;;;					function before initialize
;;;					parser, and then not worry about
;;;					the readtable again.  BP
;;;
;;;	      5/20/88 17:24:31		Improved the documentation, to
;;;	      				make the parser easier to
;;;	      				customize.  BP & SW
;;; 
;;;           Jun 13, 1988  -- SW, cleaned up documentation a bit more.
;;;           Jun 13, 1988 -- Sandy changed names of internal grammar symbols to have bang,
;;;                           exported more stuff.
;;; 
;;;           Jun 20, 1988 -- SW, Changed some names.  Re-organized.  Flushed the labels
;;;                           construct.  Broke out definition of make-default-tokenizer,
;;;                           and make-default-error-function.
;;;=====================================================================


;;;=====================================================================
;;;	To create their own parser based on ours, users have to do
;;;	the following:
;;;		Write a grammar in meta-grammar format (ends in	.src)
;;;		(MAKE-TABLES-FROM-SOURCE users-gram-file.src)
;;;		(SET-PAREN-READTABLE User's readtable) 
;;;			(defaults to CL readtable)
;;;		(INITIALIZE-PARSER  users-gram-file.tabl)
;;;		(PARSE-STRING  string)   or (PARSE-FILE file)  
;;;=====================================================================

;;;---------------------------------------------------------------------
;;;	   Module Initialization and Interface Specification
;;;---------------------------------------------------------------------

(provide :default-parser)
(in-package :ParEn
	    :use '(:user :lisp))


(export '(make-default-parser initialize-parser lookup-terminal
	  parse-string parse-file set-paren-readtable ))

(require :lr-parse
	 (lookup-pathname :lr-parse))


;;; *PAREN-PARSER*
;;; ==============
;;;
;;;	The name of the parser being used. 

(defvar *paren-parser*)				


;;; *PAREN-PARSER-DBG-FLG*
;;; ======================
;;;
;;;	If true, helps to debug during parsing.

(defvar *paren-parser-dbg-flg* nil)





;;; -------------------
;;; MAKE-DEFAULT-PARSER
;;; -------------------
;;; 
;;;	Function returns an LR parser using the tables read from
;;;	PARSE-TABLE-FILENAME.  The parser is itself a function that
;;;	takes two optional arguments, a stream and a readtable, and reads tokens from the
;;;	stream (until end of file), using the readtable,  parsing them according to the
;;;	grammar.  The stream argument defaults to terminal-io, in which
;;;	case end-of-file is given by the token :eof.  
;;;     The parser uses the default tokenizer as described below in this file.

(defun make-default-parser (parse-table-filename)
  ;; First we call MAKE-LR-PARSER with our parse-tables.  This gives us:
  ;;
  ;;	PARSER-FN is bound to the generic LR-parser.  It takes as
  ;;	arguments a tokenizer and an error function, and returns the top
  ;;	of the client-stack, if the parse was successful, or else it
  ;;	calls the error function.
  ;;    
  ;;    and
  ;; 
  ;;    TERMINAL-HASH-TABLE, which can be used to help classify tokens.

  (multiple-value-bind
	(parser-fn terminal-hash-table)
      (make-lr-parser parse-table-filename)

    ;; The value of this lambda expression is returned as the parser.
    ;; Notice that *readtable* is dynamically bound when the tokenizer
    ;; is run, and that parser-fn and terminal-hash-table are bound
    ;; in the closure of this lambda.
    #'(lambda (&optional (stream nil) 
	       (*readtable* *paren-readtable*)) ;Default to *paren-readtable*

	;; When the parser is fired up, it creates a default tokenizer and error function.
	;; The tokenizer and error function constructors are described below.
	(let* ((tokenizer (make-default-tokenizer stream terminal-hash-table))
	       (error-function (make-default-error-function tokenizer))
	       ;; Let bind the package for two reasons: first so that we
	       ;; have a handle on where are all the reading goes, and
	       ;; second so that if some action smashes the package by
	       ;; side-effect, no permanent damage results.  (The
	       ;; metagrammar does this in fact.)
	       (*package* (find-package :user)))
	  (catch 'error

	    ;; Now we call the generic lr-parser with the tokenizer
	    ;; and error function which were locally defined
	    ;; above.  This will return the top of the client
	    ;; stack (representing the whole expression parsed) or
	    ;; will call the error function.
	    (funcall parser-fn
		     tokenizer
		     error-function
		     ))))))




;;; ----------------------
;;; MAKE-DEFAULT-TOKENIZER
;;; ----------------------
;;; 
;;; This function makes a default tokenizer.  The tokenizer uses lisp's read function
;;; and *readtable* (which is dynamically bound by the parser in make-default-parser).
;;; The tokenizer takes a a list of the indicies which may legally appear next in
;;; the parse and returns a a dotted pair consisting of the token as read
;;; from the text, and the grammar symbol index of the token (or NIL in
;;; the case of an error).  The stream is modified by side-effect.
;;; 
;;; Notice that stream, terminal-hash-table, user-package, number-index, number-is-terminal,
;;; id-index and id-is-terminal are bound in the closure of the lambda.

(defun make-default-tokenizer (stream terminal-hash-table)
  
  ;; Now we cache information about our potential preterminals.  For each
  ;; preterminal type, we lookup the index and whether or not it is
  ;; terminal (t for terminal).
  
  (multiple-value-bind (number-index number-is-terminal)
      (lookup-terminal 'number terminal-hash-table)
    (multiple-value-bind (id-index id-is-terminal)
	(lookup-terminal 'identifier terminal-hash-table)
      
      ;; This lambda defines the default tokenizer.  next-possibilities is a list of
      ;; indices of grammar symbols which may appear next in the text.  If this
      ;; argument is set to T, then the tokenizer doesn't try to classify the
      ;; token, and just returns it immediately.
      
      #'(lambda (next-possibilities)
	  (let ((token (read stream nil :eof)))	; Uses LISP's READ function
	    
	    ;; Tokenizer should return :eof when done.
	    (when (eq token :eof) (setf token '<$>))
	    
	    ;; If there was an error, then the error function calls the parser with
	    ;; next-possibilities set to T.  In this case, just return the token with
	    ;; no further processing.
	    (if (eq next-possibilities t)
		token
		
		;; Here we lookup the index of the token, and whethere or not it is terminal.
		(multiple-value-bind (index term?)
		    (lookup-terminal token terminal-hash-table)
		  (when *paren-parser-dbg-flg*	
		    (format t "~&token ~s next ~s term? ~s~%" token next-possibilities term?))
		  (block found
		    
		    ;; Check to see if the token was listed as one of the terminal symbols in the
		    ;; grammar that can come next and furthermore, the user hasn't quoted it
		    ;; to force interpretation under a preterminal.  
		    ;;
		    ;; For more info about the feature which has the parser tell the
		    ;; tokenizer which symbols it is willing to accept, see the
		    ;; function MAKE-LR-PARSER.
		    (if (and term? (member index next-possibilities)
			     (not (stringp token)))
			
			;; Terminal symbol found.  Return it.
			(return-from found `(,token . ,index))
			
			;; From here down, we are doing special cases on preterminals.
			;; We need to do this because lexicon lookup is not a viable
			;; way to determine the category of preterminals.  
			;;
			;; For example, if you encounter a particular identifier, it will
			;; not appear explicitly in the lexicon.  Here is where we
			;; define the characteristics of a token which qualify it to be
			;; a certain preterminal (for example, a number...)
			(progn
			  ;; Check that there is a preterminal for NUMBERs.
			  (when (and (numberp token) number-index (not number-is-terminal))
			    ;; Found preterm NUMBER
			    (return-from found `(,token . ,number-index)))
			  
			  ;; Token isn't a number, or NUMBER isn't a preterminal.
			  ;; See if there is a preterminal for IDENTIFIERs.
			  (when (and id-index (not id-is-terminal))
			    
			    ;; There is. Classify token as an identifier.
			    (return-from found `(,token . ,id-index)))
			  
			  ;; By this point, we have gone through all of our possibilities for
			  ;; preterminals, and nothing matches.  This is an error.
			  (return-from found nil)))))))))))



;;; ---------------------------
;;; MAKE-DEFAULT-ERROR-FUNCTION
;;; ---------------------------
;;; 
;;; This function makes a default error function.  The error function takes
;;; a format control string and corresponding args.  The constructor takes
;;; as argument a tokenizer function with which to obtain and print
;;; the next few symbols in the input text.  Notice that the tokenizer
;;; is bound in the closure of the lambda expression.

(defun make-default-error-function (the-tokenizer)
  #'(lambda (ctl-string &rest args)
      (format t "~&Error: ")
      (apply #'format t ctl-string args)
      (format t "~&Next few symbols in input:~%~8t")
      (dotimes (i 5)
	(let ((skipped-tok (funcall the-tokenizer t)))
	  (format t "~a  " skipped-tok)))
      (throw 'error nil)))


;;; ---------------
;;; LOOKUP-TERMINAL
;;; ---------------
;;; 
;;; A function that returns two values, INDEX and TERMINAL?
;;; INDEX will be non nil only if the token was found in
;;; the terminal-hash-table.  In this case, INDEX will be the index
;;; of the grammar symbol and TERMINAL? will be true if the 
;;; token is NOT PRETERMINAL.  
;;; If the token isn't in the terminal-hash-table, both values
;;; will be NIL.  
	  
(defun lookup-terminal (symbol-or-string terminal-hash-table)
  (multiple-value-bind
	(contents found)
      ;; Lookup the object
      (gethash 
       ;; But convert it to a string first.  Note that if the thing is a
       ;; string, no conversion is necessary, and if it is some other
       ;; kind of object (like a sexpr or array) we don't bother
       ;; converting it anyway for speed, since they shouldn't be in the
       ;; symbol table anyway.
       (if (symbolp symbol-or-string)
	   (format nil "~:@(~a~)" symbol-or-string)
	   symbol-or-string)
       ;; in the hash table
       terminal-hash-table)
    (values-list
     (if found
	 contents
	 '(nil nil)))))






;;;================================================================================
;;;		    Read Table Stuff
;;;================================================================================

;;; *PAREN-READTABLE*
;;; =================
;;;
;;;	The readtable used during parsing.  Defaults to the standard
;;;	COMMON-LISP readtable.

(defvar *paren-readtable* (copy-readtable nil))


;;; SET-PAREN-READTABLE
;;; ===================
;;;
;;;	Sets the global value *paren-readtable* to READTABLE, which
;;;	defaults to the standard COMMON-LISP readtable.

(defun set-paren-readtable (&optional (readtable (copy-readtable nil)))
  (setq *paren-readtable* readtable))


;;; READ-SINGLE-CHAR
;;; ================
;;;
;;;	Macro character function that returns characters as
;;;	single-character atoms.

(defun read-single-char (stream char)
  (declare (ignore stream))
  (values (intern (coerce (list char) 'string))))


;;; READ-DOUBLE-CHAR
;;; ================
;;;
;;;	Macro character function that returns characters as
;;;	single-character atoms except in certain cases, when it uses the
;;;	next character to form a two-character atom.  The intended usage
;;;	is for the following sequences:   :=  <>  <=  >=  ..

;;;(defun read-double-char (stream char)
;;;  (if (member (peek-char nil stream nil :eof) '(#\. #\= #\>))
;;;      (values (intern (coerce (list char (read-char stream)) 'string)))
;;;      (values (intern (coerce (list char) 'string)))))
;
;(defun read-arrow (stream ignore)
;  (cond ((not (char= (peek-char nil stream nil :eof) #\-))
;	 '-)
;	(t  (assert (and (char= (read-char stream nil :eof) #\-)
;			 (char= (read-char stream nil :eof) #\>))
;		    nil
;		    "Malformed arrow.  Dashes may only occur in arrows: -->"
;		    nil)
;	     '|-->|)))

;(set-macro-character #\@ #'read-single-char nil *sample-readtable*)
;(set-macro-character #\$ #'read-single-char nil *sample-readtable*)
;(set-macro-character #\{ #'read-single-char nil *sample-readtable*)
;(set-macro-character #\} #'read-single-char nil *sample-readtable*)
;(set-macro-character #\( #'read-single-char nil *sample-readtable*)
;(set-macro-character #\) #'read-single-char nil *sample-readtable*)
;(set-macro-character #\+ #'read-single-char nil *sample-readtable*)
;(set-macro-character #\/ #'read-single-char nil *sample-readtable*)
;(set-macro-character #\= #'read-single-char nil *sample-readtable*)
;(set-macro-character #\. #'read-single-char nil *sample-readtable*)
;(set-macro-character #\: #'read-single-char nil *sample-readtable*)
;(set-macro-character #\| #'read-single-char nil *sample-readtable*)
;
;(set-macro-character #\- #'read-arrow nil *sample-readtable*)

;;;================================================================================
;;;   End of READTABLE stuff
;;;================================================================================
  


;;; INITIALIZE-PARSER
;;; =================
;;;
;;;	Function sets *PAREN-PARSER* to an LR parser using the tables
;;;	read from PARSE-TABLE-FILENAME.

(defun initialize-parser (parse-table-filename)
  (declare (special *paren-parser* *paren-parser-dbg-flg*))
  (setf *paren-parser*
	(make-default-parser parse-table-filename)))
 

;;; PARSE-STRING
;;; ============
;;;
;;;	Parses the string according to the syntax defined by
;;;	PARSE-TABLE-FILENAME when used in INITIALIZE-PARSER.  Assumes
;;;	that INITIALIZE-PARSER has already been called.  Uses the
;;;	readtable stored in *PAREN-READTABLE*

(defun parse-string (string)
  (declare (special *paren-parser*))
  (with-input-from-string (stream string)
    (funcall *paren-parser* stream)))


;;; PARSE-FILE
;;; ==========
;;;
;;;	Parses the FILE according to the syntax defined by
;;;	PARSE-TABLE-FILENAME when used in INITIALIZE-PARSER.  Assumes
;;;	that INITIALIZE-PARSER has already been called.  Uses the
;;;	readtable stored in *PAREN-READTABLE*.

(defun PARSE-FILE (file)
  (declare (special *paren-parser*))
  (with-open-file (str file :direction :input)
    (funcall *paren-parser* str)))







