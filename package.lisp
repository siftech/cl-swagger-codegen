;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: cl-swagger -*-

(defpackage :cl-swagger
  (:use #:cl #:drakma #:cl-json #:cl-ppcre #:mustache #:iterate)
  (:export #:generate-client))
