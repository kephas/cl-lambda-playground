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

(defgeneric normal-order (expression &optional candidate right-sides))

(defmethod normal-order ((expression variable) &optional candidate right-sides)
  (when right-sides
    (normal-order (first right-sides) nil (rest right-sides))))

(defmethod normal-order ((expression abstraction) &optional candidate right-sides)
  (if candidate
      candidate
      (normal-order (abs-body expression) nil right-sides)))

(defmethod normal-order ((expression application) &optional candidate right-sides)
  (normal-order (app-fun expression) expression (cons (app-arg expression) right-sides)))


(defgeneric applicative-order (expression &optional candidate previous-candidates))

(defmethod applicative-order ((expression variable) &optional candidate previous-candidates)
  (first previous-candidates))

(defmethod applicative-order ((expression abstraction) &optional candidate previous-candidates)
  (applicative-order (abs-body expression) nil (if candidate (cons candidate previous-candidates) previous-candidates)))

(defmethod applicative-order ((expression application) &optional candidate previous-candidates)
  (applicative-order (app-fun expression) expression previous-candidates))


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
