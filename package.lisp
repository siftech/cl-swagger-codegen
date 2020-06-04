;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; -*-

(in-package :common-lisp-user)

(defpackage :cl-swagger
  (:use #:cl
        #:cl-swagger.utils
        #:iterate)
  (:import-from #:mustache #:define)
  (:export #:generate-client))
