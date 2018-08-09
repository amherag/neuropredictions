;;;  Start symbol: file
;;;  Nonterminals: on lhs
;;;  Preterminal:  identifier (and string?)
;;;  Terminals:    quoted strings (e.g., "->") and symbols not on lhs of a rule
;;;  Empty string: null

;;;			     List of Rules

'(

(file		( stmt-list					) )

(stmt-list 	( null						) )
(stmt-list 	( stmt stmt-list				) )

(dg-spec 	( dg-atom					) )
(dg-spec 	( dg-non-atom					) )

(dg-atom 	( identifier					) )
		; must be lower-case

(dg-non-atom 	( root						) )
(dg-non-atom 	( "<" dg-non-atom feature-list ">"		) )
(dg-non-atom 	( "<" feature-list ">"				) )
(dg-non-atom 	( macro						) )
(dg-non-atom 	( macro dg-non-atom				) )
(dg-non-atom 	( "[" literal-list "]"				) )
(dg-non-atom 	( "(" dg-list-spec ")"				) )
(dg-non-atom 	( "{" literal-list "}"				) )
(dg-non-atom 	( "~" dg-non-atom				) )

(root		( identifier					) )
		; must be upper-case

(feature-list 	( null						) )
(feature-list 	( feature feature-list				) )

(feature 	( identifier					) )
		; must be lower case and previously declared

(literal-list 	( null						) )
(literal-list 	( literal literal-list				) )

(literal 	( feature ":" dg-unif-spec			) )
(literal 	( dg-unif-spec					) )

(dg-list-spec 	( dg-unif-spec					) )
(dg-list-spec 	( dg-unif-spec "," dg-list-spec 		) )
(dg-list-spec 	( dg-unif-spec "|" dg-list-spec 		) )

(dg-unif-spec 	( dg-spec "=" dg-unif-spec			) )
(dg-unif-spec 	( dg-spec					) )

(macro		( identifier					) )
		; must be upper case and declared on previous pass

(dg-constraints	( null						) )
(dg-constraints	( dg-constraint dg-constraints			) )

(dg-constraint 	( dg-specifier "=" dg-specifier			) )
(dg-constraint 	( dg-specifier					) )

(stmt		( profile-stmt					) )
(stmt		( control-stmt					) )
(stmt		( grammar-rule					) )
(stmt		( macro-defn					) )
(stmt		( lexical-stmt					) )

(profile-stmt 	( start ":" dg-constraints "."			) )
(profile-stmt 	( categories ":" token-list "."			) )
(profile-stmt 	( category path ":" path "."			) )
(profile-stmt 	( features ":" identifier-list "."		) )
		; first pass: declare identifiers in 
		; identifier-list as features
(profile-stmt 	( restrictor ":" path-list "."			) )
(profile-stmt 	( abbreviation ctl-string ":" string "."	) )
(profile-stmt 	( abbreviation ":" path-list "."		) )
(profile-stmt 	( semantics path ":" path "."			) )
(profile-stmt 	( external macro root "."			) )
		; first pass: declare root as 0-arg macro
(profile-stmt 	( external macro root dg-spec "."		) )
		; first pass: declare root as 1-arg macro

(control-stmt 	( input string "."				) )
(control-stmt 	( evaluate string "."				) )

(grammar-rule 	( rule identifier lhs "->" rhs ":" 
		  dg-constraints "."				) )

(lhs		( root						) )

(rhs		( null						) )
(rhs		( root rhs					) )

(macro-defn 	( macro root ":" dg-constraints "."	) )
		; first pass: declare root as 0-arg macro
(macro-defn 	( macro root dg-spec ":" 
		  dg-constraints "."				) )
		; first pass: declare root as 1-arg macro

(lexical-stmt 	( stem i