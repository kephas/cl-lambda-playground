(defgeneric render (expression &key rightmost))

(defmethod render ((expression abstraction) &key (rightmost t))
  (let ((rendering (format nil "λ~a.~a" (render (abs-var expression)) (render (abs-body expression)))))
    (if rightmost rendering (format nil "(~a)" rendering))))

(defmethod render ((expression variable) &key rightmost)
  (var-name expression))

(defmethod render ((expression application) &key (rightmost t))
  (format nil "~a ~a"
	  (render (app-fun expression) :rightmost nil)
	  (let* ((arg (app-arg expression))
		 (arg-rendering (render arg :rightmost rightmost)))
	    (if (typep arg 'application)
		(format nil "(~a)" arg-rendering)
		arg-rendering))))

(defmethod print-object ((object expression) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (render object) stream)))
