(in-package :thierry-technologies.com/2011/07/lambda)

#|
 | Common lambda expressions
 |#

(defvar l1 (make-expression '(lambda x x)))
(defvar ll1 (make-expression '(lambda x (lambda y x))))
(defvar ll2 (make-expression '(lambda x (lambda y y))))
(defvar lll123 (make-expression '(lambda x (lambda y (lambda z (x y z))))))
(defvar lll312 (make-expression '(lambda x (lambda y (lambda z (z x y))))))

(defvar *delta* (make-expression '(lambda x (x x))))
(defvar *omega* (make-expression (list *delta* *delta*)))

(defvar ski
  (make-environment (("i" l1)
		     ("k" ll1)
		     ("s" (make-expression '(lambda x (lambda y (lambda z (x z (y z))))))))))

#| Booleans in Church/Scott encoding|#

(defvar *true* ll1)
(defvar *false* ll2)

(defvar *if* lll123)

(defvar *and* (make-expression '(lambda p (lambda q (p q p)))))
(defvar *or* (make-expression '(lambda p (lambda q (p p q)))))
(defvar *not* (make-expression `(lambda x (x ,*false* ,*true*))))

(defvar booleans-operators
  (make-environment (("true" *true*)
		     ("false" *false*)
		     ("if" *if*)
		     ("and" *and*)
		     ("or" *or*)
		     ("not" *not*))))

(defclass booleans (proxy-environment) nil)

(defmethod %make-expression ((sexpr (eql t)) (environment booleans))
  (%make-expression "true" environment))
(defmethod %make-expression ((sexpr (eql nil)) (environment booleans))
  (%make-expression "false" environment))
  
(defvar booleans (merge-environments (make-instance 'booleans) booleans-operators))

#| Fixed-point combinators |#

;supposedly both equivalent (they seem to be, when used)
(defvar *Y* (reduce-until-abstraction (make-expression '((lambda p (lambda f (p f (p f)))) (lambda f (lambda x (f (x x))))))))
(defvar *Z* (make-expression '(lambda f ((lambda x (f (lambda y (x x y)))) (lambda x (f (lambda y (x x y))))))))

(defvar *theta* (reduce-until-abstraction (make-expression '((lambda q (q q)) (lambda x (lambda y (y (x x y))))))))


#| Church numerals |#

(defun church-num (n &optional (hide? t))
  "Returns the Church numeral representing N, by default as a hidden abstraction."
  (labels ((rec (n acc)
	     (if (zerop n)
		 acc
		 (rec (1- n) (list 'f acc)))))
    (let ((bare-expression (make-expression `(lambda f (lambda x ,(rec n 'x))))))
      (if hide? (make-instance 'hidden-abstraction :name n :abs bare-expression) bare-expression))))

(defun unchurch-num (expression)
  "Returns the integer represented by the λ-expression taken as a Church numeral (in NF)."
  (let ((rendering (render (normalize #'normal-order (make-expression (list expression '1+ 'zero))))))
    (if (position #\+ rendering)
	(eval (read-from-string (format nil "(let ((zero 0))(~a))" rendering)))
	0)))

(defvar *c_zero?* (make-expression '(lambda n (n (lambda x false) true)) booleans))
(defvar *c_plus* (make-expression '(lambda m (lambda n (lambda f (lambda x (m f (n f x))))))))
(defvar *c_succ* (make-expression '(lambda n (lambda f (lambda x (f (n f x)))))))
(defvar *c_pred* (make-expression '(lambda n (lambda f (lambda x (n (lambda g (lambda h (h (g f)))) (lambda u x) (lambda u u)))))))
(defvar *c_sub* (make-expression '(lambda m (lambda n (n pred m))) (bind-value "pred" *c_pred* nil)))
(defvar *c_mult* (make-expression '(lambda m (lambda n (lambda f (lambda x (m (n f) x)))))))
(defvar *c_exp* (make-expression `(lambda m (lambda n (n m)))))

(defvar church-operators
  (make-environment (("zero?" *c_zero?*)
		     ("succ" *c_succ*)
		     ("pred" *c_pred*)
		     ("+" *c_plus*)
		     ("-" *c_sub*)
		     ("*" *c_mult*)
		     ("^" *c_exp*))))

;;;

(defclass church (booleans) nil)

(defmethod %make-expression ((sexpr integer) (environment church))
  (declare (ignore environment))
  (church-num sexpr))

(defvar church (merge-environments (make-instance 'church) booleans church-operators))


#| Peano numerals |#

(defvar *p_zero* (make-expression '(lambda s (lambda z z)))) ; could be defined as ll2 but this makes peano numbers a tinsy bit more reabdable
(defvar *p_succ* (make-expression '(lambda n (lambda s (lambda z (s n))))))

(defun peano-num (n &optional (hide? t))
  (let ((bare-expression (normalize #'normal-order (make-expression (let@ rec ((n n)
									       (acc *p_zero*))
								      (if (zerop n) acc (rec (1- n) (list *p_succ* acc))))))))
    (if hide? (make-instance 'hidden-abstraction :name n :abs bare-expression) bare-expression)))

(defvar *p_zero?* (make-expression '(lambda n (n (lambda x false) true)) booleans))
(defvar *p_pred* (make-expression `(lambda n (n i ,(peano-num 0))) ski))

(defvar *p_plus* (reduce-until-abstraction (make-expression (list *Y* `(lambda + (lambda m (lambda n (n (lambda p (succ (+ p m))) m)))))
							    (bind-value "succ" *p_succ* nil))))
(defvar *p_plus* (reduce-until-abstraction (make-expression (list *Y* `(lambda + (lambda m (lambda n (n (lambda p (pred (+ p m))) m)))))
							    (bind-value "pred" *p_pred* nil))))

(defvar *p_mult* (reduce-until-abstraction (make-expression (list *Y* `(lambda * (lambda m (lambda n (m (lambda p (+ n (* p n))) ,(peano-num 0))))))
							    (bind-value "+" *p_plus* nil))))
(defvar *p_exp* (reduce-until-abstraction (make-expression (list *Y* `(lambda ^ (lambda m (lambda n (m (lambda p (+ n (^ p n))) ,(peano-num 1))))))
							    (bind-value "*" *p_mult* nil))))

(defvar peano-operators
  (make-environment (("zero?" *p_zero?*)
		     ("succ" *p_succ*)
		     ("pred" *p_pred*)
		     ("+" *p_plus*)
		     ("-" *p_pred*)
		     ("*" *p_mult*)
		     ("^" *p_exp*))))

;;;

(defclass peano (booleans) nil)

(defmethod %make-expression ((sexpr integer) (environment peano))
  (declare (ignore environment))
  (peano-num sexpr))

(defvar peano (merge-environments (make-instance 'peano) booleans peano-operators))

#| Pairs (in Church encoding?) |#

(defvar *c_pair* lll312)
(defvar *c_nil* ll2)

(defvar pair-operators
  (make-environment (("pair" *c_pair*)
		     ("nil" *c_nil*))))

(defun church-list (&rest list)
  (normalize #'normal-order
	     (make-expression (let@ rec ((list list))
				(if list
				    (list *c_pair* (first list) (rec (rest list)))
				    "nil"))
			      (bind-value "nil" *c_nil* *environment*))))

#| to demonstrate fixed-point combinators |#

(defun fac ()
  "Builds the factorial function in the current environment."
  (make-expression '(lambda fac (lambda n ((zero? n) 1 (* n (fac (pred n))))))))
