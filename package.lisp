(defpackage :thierry-technologies.com/2011/07/lambda
  (:use :cl)
  (:shadow :variable :reduce)
  (:export #:expression #:expr-free
	   #:scalar #:variable #:var-name
	   #:abstraction #:abs-var #:abs-body
	   #:application #:app-fun #:app-arg
	   #:free?
	   #:hidden-abstraction
	   #:bound-value #:bind-value #:merge-environments #:*environment*
	   #:make-expression #:%make-expression
	   #:make-environment
	   
	   #:var-eq?
	   #:beta-reduce
	   #:contains?
	   #:reduce-redex #:reduce
	   #:beta-candidates?
	   #:normal-order #:applicative-order
	   #:normalize #:normalization-steps #:show-normalization-steps
	   #:reduce-until-abstraction
	   #:make-expression*
	   
	   #:render
	   #:*redex-marker*

	   #:l1 #:ll1 #:ll2 #:lll312
	   #:*delta* #:*omega*
	   #:ski
	   #:*true* #:*false* #:*if* #:*and* #:*or* #:*not*
	   #:booleans-operators #:booleans
	   #:*Y* #:*Z* #:*theta*
	   #:church-num #:unchurch-num
	   #:*c_zero?* #:*c_plus* #:*c_succ* #:*c_pred* #:*c_sub* #:*c_mult* #:*c_exp*
	   #:church-operators #:church
	   #:*p_zero?* #:*p_plus* #:*p_succ* #:*p_pred* #:*p_sub* #:*p_mult* #:*p_exp*
	   #:peano-operators #:peano
	   #:*c_pair* #:*c_nil*
	   #:pair_operators #:church-list

	   ;#:decode #:type? #:%type?
	   #:procedure #:proc-fun #:proc-args
	   #:make-procedure
	   #:proc-operators #:proc/church #:proc/peano))
