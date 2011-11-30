(in-package :thierry-technologies.com/2011/07/lambda)

#| Extensions to the lambda-calculus |#

(defgeneric decode (expression type environment)
  (:documentation "Returns as the primary value the Lisp object encoded by the expression in the environment, with type as a hint.
Secondary value is T if decoding was actually successful."))

(defmethod decode (expression type environment)
  (values nil nil))

(defmethod decode ((expression hidden-abstraction) type environment)
  (values (hid-name expression) t))

(defmethod decode ((expression hidden-abstraction) (type (eql 'boolean)) environment)
  (values (read-from-string (render (normalize #'normal-order (make-expression (list expression t nil) nil))) nil) t))

;;;

(defgeneric %type? (expression type environment)
  (:documentation "Returns wether the expression satisfies the type in this environment."))

(defun type? (expression type &optional (environment *environment*))
  (%type? expression type environment))

(defmethod %type? (expression (type (eql 'numeral)) environment)
  (multiple-value-bind (value decoded?) (decode expression type environment)
    (when decoded?
      (typep value '(integer 0 *)))))

(defmethod %type? (expression (type (eql 't)) environment)
  t)

;;;

(defclass procedure (scalar hidden-abstraction)
  ((function :reader proc-fun :initarg :fun)
   (argument-types :reader proc-args :initarg :args)))

(defmethod abs-var ((object procedure))
  object) ; for identification purposes, cf. beta-reduce

(defmethod abs-body ((object procedure))
  object)

(defmethod expr-free ((object procedure))
  nil)
   
(defmacro make-function (name args &body body)
  (let ((args (mapcar (lambda (arg) (if (consp arg) arg (list arg t))) args)))
    `(make-instance 'procedure :name ,name :args ',(mapcar #'second args)
		    :fun ,(named-let rec ((args args))
				     (destructuring-bind (var type) (first args)
				       (labels ((conv-lambda (expr)
						  `(lambda (,var)
						     (let ((,var (decode ,var ',type *environment*)))
						       ,expr))))
					 (if (rest args)
					     (conv-lambda (rec (rest args)))
					     (conv-lambda `(make-expression (progn ,@body))))))))))

(defmethod normal-order ((expression procedure))
  nil)

(defmethod applicative-order ((expression procedure))
  nil)

(defmethod beta-candidates? ((abstraction procedure) value)
  (type? value (first (proc-args abstraction))))

(defmethod %beta-reduce (variable value (expression procedure))
  (if (eq variable expression) ; cf. abs-var
      (let ((result (funcall (proc-fun expression) value)))
	(if (functionp result)
	    (make-instance 'procedure :fun result :args (rest (proc-args expression))
			   :name (let ((current (hid-name expression)))
				   (if (listp current)
				       (append (butlast current)
					       (list (render value))
					       (last current))
				       (list current (render value) '…))))
	    result))
      expression))

;;;

(defvar proc-operators
  (list (make-function "zero?" ((n numeral)) (zerop n))
	(make-function "succ" ((n numeral)) (1+ n))
	(make-function "pred" ((n numeral)) (1- n))
	(make-function "+" ((x numeral) (y numeral)) (+ x y))
	(make-function "-" ((x numeral) (y numeral)) (- x y))
	(make-function "*" ((x numeral) (y numeral)) (* x y))
	(make-function "^" ((x numeral) (y numeral)) (expt x y))))

(defvar proc/church (merge-environments (make-instance 'church) booleans proc-operators))
(defvar proc/peano (merge-environments (make-instance 'peano) booleans proc-operators))
