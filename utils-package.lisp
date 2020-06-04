;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; -*-

(in-package :common-lisp-user)

(defpackage :cl-swagger.utils
  (:nicknames #:swagger-utils)
  (:use #:cl
        #:iterate)
  (:export
   ;; functions
   #:decode-json
   #:decode-json-from-source
   #:fetch-json
   #:convert-json
   #:rest-call
   #:stream-to-string

   ;; subtype of integer indicating
   ;; successful HTTP response
   #:success-response))
