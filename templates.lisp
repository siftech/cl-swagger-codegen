(in-package :cl-swagger)

(define wrapper-call-template-v2
"
;;
;; summary : {{summary}}
;; description : {{{description}}}
;; * path : {{paths}}
;;
(defun {{first-name}}-{{path-name}} (&key param content basic-authorization)
  (multiple-value-bind (stream code header)
      (drakma:http-request (concatenate 'string \"{{baseurl}}/{{path-url}}?\" param) :basic-authorization basic-authorization :accept \"{{accept}}\" :content-type \"{{accept-type}}\" :content content :want-stream t :method {{method}})
    (if (equal code 200) (progn (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
                                (cl-json:decode-json stream))
        (format t \"failed - code : ~a\" code))))")


(define rest-call-function
  "
(defun rest-call (host url-path
                  &key params content basic-authorization
                    (method :get)
                    (accept \"application/json\")
                    (content-type \"application/json\"))
  \"call http-request with basic params and conteent and authorization\"
  (multiple-value-bind (stream code)
      (drakma:http-request (format nil \"~a~a\" host url-path) :parameters params :content content :basic-authorization basic-authorization :accept accept :content-type content-type :want-stream t :method method)
    (if (equal code 200)
        (progn (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
               (cl-json:decode-json stream))
        (format t \"HTTP CODE : ~A ~%\" code))))")


(define rest-call-template-v1
  "
;;
;; {{description}}
;; * path-url : {{paths}}
;;
(defun {{first-name}}-{{path-name}} (&key params content basic-authorization)
  (rest-call \"{{baseurl}}\" \"{{path-url}}\" :params params :content content
                            :basic-authorization basic-authorization
                            :method {{method}}
                            :accept \"{{accept}}\"
                            :content-type \"{{accept-type}}\"))")

(define rest-call-template-v2
  "
;;
;; {{description}}
;; * path-url : {{paths}}
;;
(defun {{first-name}}-{{path-name}} (path-url &key params content basic-authorization)
  (rest-call \"{{baseurl}}\" path-url :params params :content content
                                              :basic-authorization basic-authorization
                                              :method {{method}}
                                              :accept \"{{accept}}\"
                                              :content-type \"{{accept-type}}\"))")


(define convert-json-template
  "
;;
;; (convert-json #'function \"/path\" content-json)
;;
(defun convert-json (query-fun path body)
  (multiple-value-bind (code stream head)
      (funcall query-fun path body)
    (if (equal code 200) (progn (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
                                (cl-json:decode-json stream))
        (format t \"failed - code : ~a\" code))))")
