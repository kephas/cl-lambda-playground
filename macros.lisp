(in-package :thierry-technologies.com/2011/07/lambda)

(defmacro cif (var form then &optional else)
  `(let ((,var ,form))
     (if ,var ,then ,else)))

(defmacro named-let (name binds &body body)
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))
