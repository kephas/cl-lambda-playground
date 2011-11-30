(defpackage :thierry-technologies.com/2011/11/lambda-test-system
  (:use :common-lisp :asdf))

(in-package :thierry-technologies.com/2011/11/lambda-test-system)

(defsystem "lambda-test"
  :description "λ-calculus implementation testsuite"
  :version "0.0.1"
  :author "Pierre Thierry <pierre@nothos.net>"
  :licence "AGPL"
  :components ((:file "test"))
  :depends-on ("lambda" "lisp-unit")
  :serial t)
