;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-

(if (uiop:find-package* '#:quicklisp nil)
    (uiop:symbol-call '#:quicklisp '#:quickload "fiveam-asdf")
    (asdf:load-system "fiveam-asdf"))


(asdf:defsystem "cl-swagger"
  :description "code generator for swagger.
   Originally developed by Inchul <ijung@mapr.com>.
   Forked and substantially rewritten at SIFT."
  :author "Robert P. Goldman <rpgoldman@sift.net>"
  :license "BSD"
  :version "2.0"
  :serial t
  :in-order-to ((test-op (test-op "cl-swagger/tests")))
  :depends-on ("drakma" "cl-json" "cl-ppcre" "cl-mustache" "iterate"
                        "cl-swagger/utils")
  :components ((:file "package")
               (:file "templates")
               (:file "code-gen")))


(asdf:defsystem "cl-swagger/utils"
  :description "Utilities that are used *both* by the swagger code
generator *and* by the generated clients."
  :author "Robert P. Goldman <rpgoldman@sift.net>"
  :license "BSD"
  :version "2.0"
  :serial t
  :depends-on ("cl-json" "drakma" "flexi-streams" "iterate")
  :components ((:file "utils-package")
               (:file "json-utils")))

(asdf:defsystem "cl-swagger/tests"
  :class :fiveam-tester-system
  :test-package :swagger-parsing-tests
  :depends-on ("fiveam" "cl-swagger")
  :pathname "tests/"
  :components ((:file "swagger-parsing-tests")))
