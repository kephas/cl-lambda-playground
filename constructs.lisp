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


(defgeneric make-expression (sexpr &optional environment))

(defmethod make-expression ((sexpr expression) &optional environment)
  sexpr)

(defmethod make-expression ((sexpr string)  &optional environment)
  (make-instance 'variable :name sexpr))

(defmethod make-expression ((sexpr symbol)  &optional environment)
  (make-expression (string-downcase (symbol-name sexpr))))

(defmethod make-expression ((sexpr cons)  &optional environment)
  (case (first sexpr)
    ((lambda) (make-instance 'abstraction :var (make-expression (second sexpr)) :body (make-expression (third sexpr) environment)))
    (t (make-applications-chain
	(make-instance 'application :fun (make-expression (first sexpr) environment) :arg (make-expression (second sexpr) environment))
	(cddr sexpr)))))

(defun make-applications-chain (fun sexpr)
  (if (null sexpr)
      fun
      (make-applications-chain (make-instance 'application
					      :fun fun
					      :arg (make-expression (first sexpr)))
			       (rest sexpr))))
