(in-package :cl-swagger)

;;; FIXME: these could all be macros instead of strings.  With them being strings, 
;;; we can't really edit them easily in lisp mode.

(define wrapper-call-template-v2
"
;;
;; summary : {{summary}}
;; description : {{{description}}}
;; * path : {{paths}}
;;
(defun {{first-name}}-{{path-name}} (&key (base-url \"{{baseurl}}\") param content basic-authorization)
  (multiple-value-bind (stream code header)
      (drakma:http-request (concatenate 'string base-url \"/\" \"{{path-url}}?\" param) :basic-authorization basic-authorization :accept \"{{accept}}\" :content-type \"{{accept-type}}\" :content content :want-stream t :method {{method}})
    (if  (and (< code 300) (>= code 200))
         (progn (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
                (cl-json:decode-json stream))
        (format t \"failed - code : ~a\" code))))")

(define stream-to-string-function
"
(defun stream-to-string (stream)
  (with-output-to-string (out)
    (loop :for new = (read-line stream nil nil)
          :while new
          :do (print new out))))
")


(define rest-call-function
  "
(defun rest-call (host url-path
                  &key params content basic-authorization
                    (method :get)
                    (accept \"application/json\")
                    (content-type \"application/json\")
                    (debug nil))
  \"Call http-request with basic params, content and authorization.\"
  (flet ((make-request ()
           (drakma:http-request (format nil \"~a~a\" host url-path)
                           :parameters params
                           :content content
                           :basic-authorization basic-authorization
                           :accept accept
                           :content-type content-type
                           :want-stream t
                           :method method)))
  (multiple-value-bind (stream code)
      (if debug
          (let ((drakma:*header-stream* *standard-output*))
              (make-request))
            (make-request))
    (if (and (< code 300) (>= code 200))
        (progn (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
               (cl-json:decode-json stream))
        (let ((response-body (stream-to-string stream)))
            (error \"Unsuccessful HTTP ~a.  Code: ~a.~%Response header:~%~a~%Response body: ~a\"
                 method code header response-body))))))")


(define rest-call-template-v1
  "
;;
;; {{description}}
;; * path-url : {{paths}}
;;
(defun {{first-name}}-{{path-name}} (&key (base-url \"{{baseurl}}\") params content basic-authorization debug)
  (rest-call base-url \"{{path-url}}\" :params params :content content
                            :basic-authorization basic-authorization
                            :method {{method}}
                            :accept \"{{accept}}\"
                            :content-type \"{{accept-type}}\"
                            :debug debug))")

(define rest-call-template-v2
  "
;;
;; {{description}}
;; * path-url : {{paths}}
;;
(defun {{first-name}}-{{path-name}} (path-url &key (base-url \"{{baseurl}}\") params content basic-authorization debug)
  (rest-call base-url path-url :params params :content content
                                              :basic-authorization basic-authorization
                                              :method {{method}}
                                              :accept \"{{accept}}\"
                                              :content-type \"{{accept-type}}\"
                                              :debug debug))")


(define convert-json-template
  "
;;
;; (convert-json #'function \"/path\" content-json)
;;
(defun convert-json (query-fun path body)
  (multiple-value-bind (code stream head)
      (funcall query-fun path body)
    (if (and (>= code 200) (< code 300))
        (progn (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
               (let  ((cl-json:*json-identifier-name-to-lisp* (lambda (x) x)))
                 (cl-json:decode-json stream)))
        (error \"CONVERT-JSON failed with code ~a\" code))))")
