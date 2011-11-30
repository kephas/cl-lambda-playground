(in-package :thierry-technologies.com/2011/07/lambda)


#| Beta reduction |#

(defun var-eq? (var1 var2)
  (equal (var-name var1) (var-name var2)))


(defgeneric beta-reduce (abstraction value)
  (:documentation "Takes the appropriate steps to beta-reduce ABSTRACTION with VALUE."))

(defgeneric %beta-reduce (variable value expression)
  (:documentation "Returns EXPRESSION with VALUE substituted to each free occurrence of VARIABLE."))


(defmethod beta-reduce ((abstraction abstraction) value)
  (%beta-reduce (abs-var abstraction) value (abs-body abstraction)))

;; beta-reduction has a special case for hidden-abstraction merely for
;; UI considerations: without it, when doing beta-reduction, the user
;; has to guess, from the result, what the actual value of the
;; abstraction was. This method adds a reduction step where the
;; hidden-abstraction is replaced by the abstraction it hides.
(defmethod beta-reduce ((abstraction hidden-abstraction) value)
  (make-expression (list (hid-abs abstraction) value)))


(defmethod %beta-reduce (variable value (expression variable))
  (if (var-eq? variable expression) value expression))

(defmethod %beta-reduce (variable value (expression application))
  (if (free? variable expression)
      (make-expression (list (%beta-reduce variable value (app-fun expression))
			     (%beta-reduce variable value (app-arg expression))))
      expression))

(defmethod %beta-reduce (variable value (expression abstraction))
  (if (free? variable expression)
      (make-expression (list 'lambda (abs-var expression)
			     (%beta-reduce variable value (abs-body expression))))
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
      (beta-reduce (app-fun expression)
		   (app-arg expression))
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

; Important design consequence
; ============================
; An evaluation strategy is just a function that returns the
; application that should be reduce next; as everything is based on
; the identity of applications, no operation should ever reuse an
; application to create a new expression (or the same application
; object could be present twice in the syntax tree).

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

(defun normalize (strategy expression &optional (count 0))
  (declare (type (integer 0 *) count))
  (multiple-value-bind (reduction progress) (reduce strategy expression)
    (if progress
	(normalize strategy reduction (1+ count))
	(values reduction count))))

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

; this function is used to bind in an environment
; an abstraction that is initially defined as an application
; (most notably recursive functions defined as (Y f))
(defun reduce-until-abstraction (expression &optional (strategy #'normal-order))
  (if (typep expression 'abstraction) ; could be a call to a GF abstraction? to remain free from inheritance
      expression
      (reduce-until-abstraction (reduce strategy expression) strategy)))

(defun make-expression* (sexpr &optional (environment *environment*))
  (reduce-until-abstraction (%make-expression sexpr environment)))
