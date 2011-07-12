(in-package :thierry-technologies.com/2011/07/lambda)

#|
 | Common lambda expressions
 |#


(defvar *id* (make-expression '(lambda x x)))
(defvar *delta* (make-expression '(lambda x (x x))))
(defvar *omega* (make-expression (list *delta* *delta*)))


#| Booleans |#

(defvar *true* (make-expression '(lambda x (lambda y x))))
(defvar *false* (make-expression '(lambda x (lambda y y))))

(defvar *if* (make-expression '(lambda cond (lambda then (lambda else (cond then else))))))

(defvar *and* (make-expression '(lambda p (lambda q (p q p)))))
(defvar *or* (make-expression '(lambda p (lambda q (p p q)))))
(defvar *not* (make-expression `(lambda x (x ,*false* ,*true*))))
