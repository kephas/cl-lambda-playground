(defpackage :thierry-technologies.com/2011/11/lambda-test
  (:use :common-lisp :lisp-unit :thierry-technologies.com/2011/07/lambda)
  (:shadowing-import-from :thierry-technologies.com/2011/07/lambda #:variable #:reduce))


(in-package :thierry-technologies.com/2011/11/lambda-test)

(defvar *expr1* (make-expression '(lambda x y)))
(defvar *expr2* (make-expression '(lambda x (y (lambda y (z x y))))))
(defvar *expr3* (make-expression '(x y)))
(defvar *expr4* (make-expression (list *expr3* *expr3*)))

(define-test constructs
  (assert-true (typep *expr1* 'abstraction))
  (assert-true (typep (abs-body *expr1*) 'variable))
  (assert-true (typep *expr3* 'application)))

(define-test freeness
  (assert-true (free? "y" *expr1*))
  (assert-false (free? "x" *expr1*))
  (assert-true (free? "y" *expr2*))
  (assert-true (free? "z" *expr2*)))

(define-test no-app-reuse
  (assert-false (eq (app-fun *expr4*) (app-arg *expr4*))))
