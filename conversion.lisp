(in-package :thierry-technologies.com/2011/07/lambda)


#| Beta reduction |#

(defun var-eq? (var1 var2)
  (equal (var-name var1) (var-name var2)))

(defgeneric beta-reduce (variable value expression))

(defmethod beta-reduce (variable value (expression variable))
  (if (var-eq? variable expression) value expression))

(defmethod beta-reduce (variable value (expression application))
  (if (free? variable expression)
      (make-expression (list (beta-reduce variable value (app-fun expression))
			     (beta-reduce variable value (app-arg expression))))
      expression))

(defmethod beta-reduce (variable value (expression abstraction))
  (if (free? variable expression)
      (make-expression (list 'lambda (abs-var expression)
			     (beta-reduce variable value (abs-body expression))))
      expression))


#| Reduction of a known redex |#

(defgeneric contains? (container containee))

(defmethod contains? ((container scalar) containee)
  (eq container containee))

(defmethod contains? ((container abstraction) containee)
  (or (eq container containee)
      (contains? (abs-var container) containee)
      (contains? (abs-body container) containee)))

(defmethod contains? ((container application) containee)
  (or (eq container containee)
      (contains? (app-fun container) containee)
      (contains? (app-arg container) containee)))


(defgeneric reduce-redex (redex expression))

(defmethod reduce-redex (redex (expression variable))
  expression)

(defmethod reduce-redex (redex (expression abstraction))
  (if (contains? expression redex)
      (make-expression (list 'lambda (abs-var expression) (reduce-redex redex (abs-body expression))))
      expression))

(defmethod reduce-redex (redex (expression application))
  (if (eq redex expression)
      (beta-reduce (abs-var (app-fun expression))
		   (app-arg expression)
		   (abs-body (app-fun expression)))
      (if (contains? expression redex)
	  (make-expression (list (reduce-redex redex (app-fun expression))
				 (reduce-redex redex (app-arg expression))))
	  expression)))

(defun reduce (strategy expression)
  (let ((redex (funcall strategy expression)))
    (if redex
	(values (reduce-redex redex expression) t)
	(values expression nil))))


#| Evaluation strategies |#

(defgeneric beta-candidates? (abstraction value)
  (:documentation "Is this a possible beta reduction?"))

(defmethod beta-candidates? (abstraction value)
  "Base case: nope."
  nil)

(defmethod beta-candidates? ((abstraction abstraction) value)
  "Beta reducing the application of a value to an actual abstraction does work."
  t)


(defgeneric normal-order (expression))

(defmethod normal-order ((expression variable))
  nil)

(defmethod normal-order ((expression abstraction))
  (normal-order (abs-body expression)))

(defmethod normal-order ((expression application))
  (with-accessors ((fun app-fun) (arg app-arg)) expression
    (if (beta-candidates? fun arg)
	expression
	(cif left (normal-order fun)
	     left
	     (normal-order arg)))))


(defgeneric applicative-order (expression))

(defmethod applicative-order ((expression variable))
  nil)

(defmethod applicative-order ((expression abstraction))
  (applicative-order (abs-body expression)))

(defmethod applicative-order ((expression application))
  (with-accessors ((fun app-fun) (arg app-arg)) expression
    (cif right (applicative-order arg)
	 right
	 (if (beta-candidates? fun arg)
	     expression
	     (applicative-order fun)))))


#| Finding a normal form |#

(defun normalize (strategy expression)
  (multiple-value-bind (reduction progress) (reduce strategy expression)
    (if progress
	(normalize strategy reduction)
	reduction)))

(defun normalization-steps (strategy expression)
  (labels ((rec (sub-expression acc)
	     (multiple-value-bind (reduction progress) (reduce strategy sub-expression)
	       (if progress
		   (rec reduction (cons reduction acc))
		   (reverse acc)))))
    (rec expression (list expression))))

(defun show-normalization-steps (strategy expression &key interactive (stream t))
  (labels ((rec (sub-expression)
	     (format stream "~a~%" (render sub-expression :redex (funcall strategy sub-expression)))
	     (multiple-value-bind (reduction progress) (reduce strategy sub-expression)
	       (when progress
		 (when interactive
		   (read-line *query-io*))
		 (rec reduction)))))
    (rec expression)))
