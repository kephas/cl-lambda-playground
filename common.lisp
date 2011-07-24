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

(defun bool->lambda (bool)
  (make-instance 'hidden-abstraction
		 :name (if bool "true" "false")
		 :abs (if bool *true* *false*)))

(defmethod make-expression ((sexpr (eql t)) &optional environment)
  (declare (ignore environment))
  (bool->lambda sexpr))
(defmethod make-expression ((sexpr (eql nil)) &optional environment)
  (declare (ignore environment))
  (bool->lambda sexpr))


(defvar *if* (make-expression '(lambda cond (lambda then (lambda else (cond then else))))))

(defvar *and* (make-expression '(lambda p (lambda q (p q p)))))
(defvar *or* (make-expression '(lambda p (lambda q (p p q)))))
(defvar *not* (make-expression `(lambda x (x ,*false* ,*true*))))


#| Fixed-point combinators |#

;supposedly both equivalent (they seem to be, when used)
(defvar *Y* (make-expression '((lambda p (lambda f (p f (p f)))) (lambda f (lambda x (f (x x)))))))
(defvar *Z* (make-expression '(lambda f ((lambda x (f (lambda y (x x y)))) (lambda x (f (lambda y (x x y))))))))

(defvar *theta* (make-expression '((lambda q (q q)) (lambda x (lambda y (y (x x y)))))))


#| Church numerals |#

(defvar *zero* (make-expression '(lambda f (lambda x x))))

(defun church-num (n &optional (hide? t))
  (labels ((rec (n acc)
	     (if (zerop n)
		 acc
		 (rec (1- n) (list 'f acc)))))
    (let ((bare-expression (make-expression `(lambda f (lambda x ,(rec n 'x))))))
      (if hide? (make-instance 'hidden-abstraction :name n :abs bare-expression) bare-expression))))

(defvar *plus* (make-expression '(lambda m (lambda n (m f (n f x))))))
(defvar *succ* (normalize #'normal-order (make-expression (list *plus* (church-num 1)))))
(defvar *pred* (make-expression '(lambda n (lambda f (lambda x (n (lambda g (lambda h (h (g f)))) (lambda u x) (lambda u u)))))))
(defvar *sub* (make-expression `(lambda m (lambda n (n ,*pred* m)))))

(defvar *mult* (make-expression '(lambda m (lambda n (lambda f (lambda x (m (n f) x)))))))
(defvar *exp* (normalize #'normal-order (make-expression `(lambda m (lambda n (m (,*mult* n) ,(church-num 1)))))))

(defvar *zero?* (make-expression `(lambda n (n (lambda x ,*false*) ,*true*))))

(defun unchurch-num (expression)
  (let ((rendering (render (normalize #'normal-order (make-expression (list expression '1+ 'zero))))))
    (if (position #\+ rendering)
	(eval (read-from-string (format nil "(let ((zero 0))(~a))" rendering)))
	0)))

(defmethod make-expression ((sexpr integer) &optional environment)
  (declare (ignore environment))
  (church-num sexpr))


#| to demonstrate fixed-point combinators |#

(defvar *fac* (make-expression `(lambda fac (lambda n ((,*zero?* n) ,(church-num 1) (,*mult* n (fac (,*pred* n))))))))
