(in-package :thierry-technologies.com/2011/07/lambda)


#| Basic constructs of lambda calculus |#

(defclass expression ()
  ((free-variables :reader expr-free :initarg :free)))

(defclass scalar (expression) ()
  (:documentation "Any expression that doesn't contains anything or encapsulates it opaquely"))

(defclass variable (scalar)
  ((name :accessor var-name :initarg :name)))

(defclass abstraction (expression)
  ((variable :accessor abs-var :initarg :var :initform nil)
   (body :accessor abs-body :initarg :body :initform nil)))

(defclass application (expression)
  ((function :accessor app-fun :initarg :fun)
   (argument :accessor app-arg :initarg :arg)))


(defgeneric free? (variable expression))

(defmethod free? ((variable variable) (expression expression))
  (find (var-name variable) (expr-free expression) :test #'string=))


#| A hidden abstraction is rendered by its name
instead of its content |#

(defclass hidden-abstraction (abstraction)
  ((content :reader hid-abs :initarg :abs)
   (name :reader hid-name :initarg :name)))

(defmethod expr-free ((object hidden-abstraction))
  (expr-free (hid-abs object)))

(defmethod abs-var ((object hidden-abstraction))
  (abs-var (hid-abs object)))

(defmethod abs-body ((object hidden-abstraction))
  (abs-body (hid-abs object)))


#| Building of lambda expressions for symbolic expressions |#

(defgeneric make-expression (sexpr &optional environment))

(defmethod make-expression ((sexpr expression) &optional environment)
  (declare (ignore environment))
  sexpr)

(defmethod make-expression ((sexpr string)  &optional environment)
  (let ((hidden (find sexpr environment :key #'hid-name :test #'equal)))
    (if hidden
	hidden
	(make-instance 'variable :name sexpr :free (list sexpr)))))

(defmethod make-expression ((sexpr symbol)  &optional environment)
  (make-expression (string-downcase (symbol-name sexpr)) environment))

(defmethod free? ((variable string) expression)
  (free? (make-expression variable) expression))


(defun free-in-either (expr1 expr2)
  (let ((free1 (expr-free expr1))
	(free2 (expr-free expr2)))
    (remove-duplicates (append free1 free2) :test #'string=)))

(defmethod make-expression ((sexpr cons)  &optional environment)
  (case (first sexpr)
    ((lambda) (let ((var (make-expression (second sexpr)))
		    (body (make-expression (third sexpr) environment)))
		(make-instance 'abstraction :var var :body body
			       :free (remove-if (lambda (name) (equal name (var-name var))) (expr-free body)))))
    (t (let ((fun (make-expression (first sexpr) environment)))
	 (make-applications-chain fun (rest sexpr) environment)))))

(defun make-applications-chain (fun sexpr &optional environment)
  (if (null sexpr)
      fun
      (let ((arg (make-expression (first sexpr) environment)))
	(make-applications-chain (make-instance 'application :fun fun :arg arg :free (free-in-either fun arg))
				 (rest sexpr)
				 environment))))
