;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-

(asdf:defsystem "cl-swagger"
  :description "code generator for swagger.
   Originally developed by Inchul <ijung@mapr.com>.
   Forked and substantially rewritten at SIFT."
  :author "Robert P. Goldman <rpgoldman@sift.net>"
  :license "BSD"
  :serial t
  :depends-on ("drakma" "cl-json" "cl-ppcre" "cl-mustache")
  :components ((:file "package")
               (:file "templates")
               (:file "code-gen")))
