(in-package :thierry-technologies.com/2011/07/lambda)

#| Extensions to the lambda-calculus |#

(defgeneric decode (expression type environment)
  (:documentation "Returns the Lisp object encoded by the expression in the environment, with type as a hint."))

(defmethod decode ((expression hidden-abstraction) type environment)
  (hid-name expression))

(defmethod decode ((expression hidden-abstraction) (type (eql 'boolean)) environment)
  (read-from-string (render (normalize #'normal-order (make-expression (list expr t nil) nil))) nil))

;;;

(defgeneric %type? (expression type environment)
  (:documentation "Returns wether the expression satisfies the type in this environment."))

(defun type? (expression type &optional (environment *environment*))
  (%type? expression type environment))

(defmethod %type? (expression (type (eql 'numeral)) environment)
  (typep (decode expression type environment) '(integer 0 *)))

(defmethod %type? (expression (type (eql 't)) environment)
  t)

;;;

(defclass lisp-function (scalar hidden-abstraction)
  ((function :reader lisp-fun :initarg :fun)
   (argument-types :reader lisp-args :initarg :args)))

(defmethod abs-var ((object lisp-function))
  object) ; for identification purposes, cf. beta-reduce

(defmethod abs-body ((object lisp-function))
  object)

(defmethod expr-free ((object lisp-function))
  nil)
   
(defmacro make-function (name args &body body)
  (let ((args (mapcar (lambda (arg) (if (consp arg) arg (list arg t))) args)))
    `(make-instance 'lisp-function :name ,name :args ',(mapcar #'second args)
		    :fun ,(named-let rec ((args args))
				     (destructuring-bind (var type) (first args)
				       (labels ((conv-lambda (expr)
						  `(lambda (,var)
						     (let ((,var (decode ,var ',type *environment*)))
						       ,expr))))
					 (if (rest args)
					     (conv-lambda (rec (rest args)))
					     (conv-lambda `(make-expression (progn ,@body))))))))))

(defmethod normal-order ((expression lisp-function))
  nil)

(defmethod applicative-order ((expression lisp-function))
  nil)

(defmethod beta-reduce (variable value (expression lisp-function))
  (if (eq variable expression) ; cf. abs-var
      (let ((result (funcall (lisp-fun expression) value)))
	(if (functionp result)
	    (make-instance 'lisp-function :fun result :args (rest (lisp-args expression))
			   :name (let ((current (hid-name expression)))
				   (if (listp current)
				       (append (butlast current)
					       (list (render value))
					       (last current))
				       (list current (render value) '…))))
	    result))
      expression))
