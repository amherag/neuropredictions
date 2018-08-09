;;; -*- Syntax: Zetalisp; Package: ZL-USER -*-


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




(defpackage :ParEn
  (:use :common-lisp :cl-user))

(defsystem Paren 
    (:pretty-name "The ParEn Common LISP Parser Environment System"
     :default-pathname "Paren:Paren;"
     :patchable NIL
     :initial-status :experimental
     :source-category :restricted
     :distribute-sources T
     :distribute-binaries T)
  
  (:module sysdef ("paren-3600" "paren")
	   (:source-category :basic))
  (:module util ("util")
	   (:uses-definitions-from sysdef))
  (:module oset ("oset")
	   (:uses-definitions-from sysdef util))
  (:module g-symbol ("g-symbol")
	   (:uses-definitions-from sysdef util oset))
  (:module slrpgram ("slrpgram")
	   (:uses-definitions-from sysdef util oset g-symbol))
  (:module item ("item")
	   (:uses-definitions-from sysdef util oset g-symbol slrpgram))
  (:module closure ("closure0" "closure1")
	   (:uses-definitions-from sysdef oset g-symbol item))
  (:module lr0-sets ("lr0-sets")
	   (:uses-definitions-from sysdef util oset item closure))
  (:module empty-st ("empty-st")
	   (:uses-definitions-from sysdef g-symbol slrpgram))
  (:module first ("first")
	   (:uses-definitions-from sysdef util oset g-symbol slrpgram
				   empty-st))
  (:module follow ("follow")
	   (:uses-definitions-from sysdef util slrpgram empty-st first))
  (:module dump ("dump")
	   (:uses-definitions-from sysdef util g-symbol slrpgram
				   empty-st first follow lr0-sets))
  (:module tables ("tables")
	   (:uses-definitions-from sysdef oset slrpgram empty-st first
				   follow lr0-sets dump))
  (:module lalr1 ("lalr1")
	   (:uses-definitions-from sysdef oset slrpgram empty-st first 
				   follow lr0-sets dump tables))
  (:module generator ("gm-to-tab")
	   (:uses-definitions-from sysdef oset slrpgram empty-st first 
				   follow lr0-sets dump tables lalr1))
  (:module driver (ParEn-driver)
	   (:type :system)
	   (:uses-definitions-from sysdef))
  )

(defsystem ParEn-driver
    (:pretty-name "The ParEn System driver"
     :default-pathname "Paren:Paren;"
     :initial-status :experimental
     :source-category :restricted
     :patchable NIL
     :distribute-sources T
     :distribute-binaries T)

  (:module sysdef ("paren-3600" "paren")
	   (:source-category :basic))
  (:module util ("util")
	   (:uses-definitions-from sysdef))
  (:module driver ("lr-parse"
		   "def-parse"
		   "metaparse")
	   (:uses-definitions-from sysdef util)
	   (:source-category :basic))
  (:module metagrammar ("metagramr-tables")
	   (:source-category :basic))
  (:module metagrammar-source ("metagramr.src")
	   (:source-category :basic)
	   (:type :text))
  )


