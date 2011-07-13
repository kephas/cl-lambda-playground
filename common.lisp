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


#| Fixed-point combinators |#

(defvar *Y* (make-expression '((lambda p (lambda f (p f (p f)))) (lambda f (lambda x (f (x x)))))))
(defvar *theta* (make-expression '((lambda q (q q)) (lambda x (lambda y (y (x x y)))))))


#| Church numerals |#

(defvar *zero* (make-expression '(lambda f (lambda x x))))

(defvar *plus* (make-expression '(lambda m (lambda n (m f (n f x))))))
(defvar *succ* (normalize #'normal-order (make-expression (list *plus* (church-num 1))))
(defvar *pred* (make-expression '(lambda n (lambda f (lambda x (n (lambda g (lambda h (h (g f)))) (lambda u x) (lambda u u)))))))
(defvar *sub* (make-expression `(lambda m (lambda n (n ,*pred* m)))))

(defvar *mult* (make-expression '(lambda m (lambda n (m (n f) x)))))
(defvar *exp* (normalize #'normal-order (make-expression `(lambda m (lambda n (m (,*mult* n) ,(church-num 1)))))))

(defvar *zero?* (make-expression `(lambda n (n (lambda x ,*false*) ,*true*))))

(defun church-num (n)
  (labels ((rec (n acc)
	     (if (zerop n)
		 acc
		 (rec (1- n) (list 'f acc)))))
    (make-expression `(lambda f (lambda x ,(rec n 'x))))))
