(in-package :thierry-technologies.com/2011/07/lambda)

(defun var-eq? (var1 var2)
  (equal (var-name var1) (var-name var2)))

(defgeneric beta-reduce (variable value expression))

(defmethod beta-reduce (variable value (expression variable))
  (if (var-eq? variable expression) value expression))

(defmethod beta-reduce (variable value (expression application))
  (make-instance 'application
		 :fun (beta-reduce variable value (app-fun expression))
		 :arg (beta-reduce variable value (app-arg expression))))

(defmethod beta-reduce (variable value (expression abstraction))
  (if (var-eq? variable (abs-var expression))
      expression ; we only substitute free occurrences
      (make-instance 'abstraction
		     :var (abs-var expression)
		     :body (beta-reduce variable value (abs-body expression)))))


(defgeneric reduce-redex (redex expression))

(defmethod reduce-redex (redex (expression variable))
  expression)

(defmethod reduce-redex (redex (expression abstraction))
  (make-instance 'abstraction
		 :var (abs-var expression)
		 :body (reduce-redex redex (abs-body expression))))

(defmethod reduce-redex (redex (expression application))
  (if (eq redex expression)
      (beta-reduce (abs-var (app-fun expression))
		   (app-arg expression)
		   (abs-body (app-fun expression)))
      (make-instance 'application
		     :fun (reduce-redex redex (app-fun expression))
		     :arg (reduce-redex redex (app-arg expression)))))

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

(defun show-normalization-steps (strategy expression &optional (stream t))
  (dolist (step (normalization-steps strategy expression))
    (format stream "~a~%" (render step :redex (funcall strategy step)))))
