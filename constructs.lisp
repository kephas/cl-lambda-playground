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

;;;

(defgeneric free? (variable expression)
  (:documentation "Returns wether a variable is free in the expression"))

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

(defgeneric %make-expression (sexpr environment)
  (:documentation "Takes an s-expression and builds the corresponding λ-expression."))

(defgeneric bound-value (name environment)
  (:documentation "Returns the abstraction associated with the name in the environment."))

(defgeneric bind-value (name value environment)
  (:documentation "Creates a new environment containing the binding of the value to the name
and the existing bindings of the environment."))

(defgeneric merge-environment (env1 env2)
  (:documentation "Creates a new environment containing the bindings of both environments."))

;;;

(defun merge-environments (&rest environments)
  "Merges several environments together."
  (when environments
    (merge-environment (first environments) (apply #'merge-environments (rest environments)))))

(defvar *environment* nil)

(defun make-expression (sexpr &optional (environment *environment*))
  "Takes an s-expression and builds the corresponding λ-expression. By
default, uses *environment* as environment."
  (%make-expression sexpr environment))

;;;

(defmethod %make-expression ((sexpr expression) environment)
  (declare (ignore environment))
  sexpr)

(defmethod %make-expression ((sexpr string) environment)
  (cif hidden (bound-value sexpr environment)
       hidden
       (make-instance 'variable :name sexpr :free (list sexpr))))

(defmethod %make-expression ((sexpr symbol) environment)
  (make-expression (string-downcase (symbol-name sexpr)) environment))

(defmethod free? ((variable string) expression)
  (free? (make-expression variable) expression))

;;;

(defun free-in-either (expr1 expr2)
  "Returns the names of the variables free in either expression."
  (let ((free1 (expr-free expr1))
	(free2 (expr-free expr2)))
    (remove-duplicates (append free1 free2) :test #'string=)))

(defmethod %make-expression ((sexpr cons) environment)
  (case (first sexpr)
    ((lambda) (let ((var (make-expression (second sexpr)))
		    (body (make-expression (third sexpr) environment)))
		(make-instance 'abstraction :var var :body body
			       :free (remove-if (lambda (name) (equal name (var-name var))) (expr-free body)))))
    (t (let ((fun (make-expression (first sexpr) environment)))
	 (make-applications-chain fun (rest sexpr) environment)))))

(defun make-application (fun arg)
  (make-instance 'application :fun fun :arg arg :free (free-in-either fun arg)))

(defun make-applications-chain (fun sexpr environment)
  (if (null sexpr)
      fun
      (let ((arg (%make-expression (first sexpr) environment)))
	(make-applications-chain (make-application fun arg)
				 (rest sexpr)
				 environment))))

(defmethod %make-expression ((sexpr application) environment)
  "As identity of an application is used to find a redex,
reuse of an application in building a new expression is a special case."
  (declare (ignore environment))
  (make-application (app-fun sexpr) (app-arg sexpr)))

;;;

(defmethod bound-value (name (environment list))
  (find name environment :key #'hid-name :test #'equal))

(defmethod bind-value (name value (environment list))
  (cons (make-instance 'hidden-abstraction :name name :abs value) environment))

(defmethod merge-environment ((env1 list) (env2 list))
  (remove-duplicates (append env1 env2) :key #'hid-name :test #'equal))

(defmacro make-environment (bindings)
  (let@ rec ((bindings bindings))
    (when bindings
      (list 'bind-value (caar bindings) (cadar bindings) (rec (cdr bindings))))))

;;;

(defclass proxy-environment ()
  ((bindings :reader env-bindings :initarg :bind :initform nil))
  (:documentation "The goal of this class is to be subclassed, so that a method for
%make-expression can be specialized for the subclass."))

(defmethod bound-value (name (environment proxy-environment))
  (bound-value name (env-bindings environment)))

(defmethod bind-value (name value (environment proxy-environment))
  (make-instance (class-of environment) ; use of class-of for subclasses
		 :bind (bind-value name value (env-bindings environment))))

(defmethod merge-environment ((env1 proxy-environment) env2)
  (make-instance (class-of env1)
		 :bind (merge-environment (env-bindings env1) env2)))

(defmethod merge-environment (env1 (env2 proxy-environment))
  (merge-environment env1 (env-bindings env2)))
