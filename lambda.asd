(defpackage :thierry-technologies.com/2011/07/lambda-system
  (:use :common-lisp :asdf))

(in-package :thierry-technologies.com/2011/07/lambda-system)

(defsystem "lambda"
  :description "λ-calculus implementation"
  :version "0.0.1"
  :author "Pierre Thierry <pierre@nothos.net>"
  :licence "AGPL"
  :components ((:file "package")
	       (:file "macros")
	       (:file "constructs")
	       (:file "conversion")
	       (:file "render")
	       (:file "common")
	       (:file "extension"))
  :serial t)
