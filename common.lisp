(in-package :thierry-technologies.com/2011/07/lambda)

#|
 | Common lambda expressions
 |#


(defvar *id* (make-expression '(lambda x x)))
(defvar *delta* (make-expression '(lambda x (x x))))
(defvar *omega* (make-expression (list *delta* *delta*)))
