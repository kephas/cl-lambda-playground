(in-package :thierry-technologies.com/2011/07/lambda)

(defclass expression () ())

(defclass variable (expression)
  ((name :accessor var-name :initarg :name)))

(defclass abstraction (expression)
  ((variable :accessor abs-var :initarg :var)
   (body :accessor abs-body :initarg :body)))

(defclass application (expression)
  ((function :accessor app-fun :initarg :fun)
   (argument :accessor app-arg :initarg :arg)))


(defun make-expression (sexpr)
  (typecase sexpr
    ((or character string)
     (make-instance 'variable :name sexpr))
    (symbol
     (make-instance 'variable :name (string-downcase (symbol-name sexpr))))
    (cons
     (case (first sexpr)
       ((lambda) (make-instance 'abstraction :var (make-expression (second sexpr)) :body (make-expression (third sexpr))))
       (t (make-applications-chain
	   (make-instance 'application :fun (make-expression (first sexpr)) :arg (make-expression (second sexpr)))
	   (cddr sexpr)))))))

(defun make-applications-chain (fun sexpr)
  (if (null sexpr)
      fun
      (make-applications-chain (make-instance 'application
					      :fun fun
					      :arg (make-expression (first sexpr)))
			       (rest sexpr))))
