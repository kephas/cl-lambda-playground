(in-package :thierry-technologies.com/2011/07/lambda)

#| Extensions to the lambda-calculus |#

(defclass lisp-function (scalar hidden-abstraction)
  ((function :reader lisp-fun :initarg :fun)))

(defmethod abs-var ((object lisp-function))
  object) ; for identification purposes, cf. beta-reduce

(defmethod abs-body ((object lisp-function))
  object)

(defmethod expr-free ((object lisp-function))
  nil)
   
(defmacro make-function (name args return &body body)
  `(make-instance 'lisp-function :name ,name :fun
		  ,(named-let rec ((args args))
			      (destructuring-bind (var conv) (first args)
				(labels ((conv-lambda (expr)
					   `(lambda (,var)
					      (let ((,var (,conv ,var)))
						,expr))))
				  (if (rest args)
				      (conv-lambda (rec (rest args)))
				      (conv-lambda `(,return (progn ,@body)))))))))

(defmethod normal-order ((expression lisp-function))
  nil)

(defmethod applicative-order ((expression lisp-function))
  nil)

(defmethod beta-reduce (variable value (expression lisp-function))
  (if (eq variable expression) ; cf. abs-var
      (let ((result (funcall (lisp-fun expression) value)))
	(if (functionp result)
	    (make-instance 'lisp-function :fun result
			   :name (let ((current (hid-name expression)))
				   (if (listp current)
				       (append (butlast current)
					       (list (render value))
					       (last current))
				       (list current (render value) '…))))
	    result))
      expression))
