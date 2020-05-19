(in-package "CL-SWAGGER")

;;; drakma:*header-stream* for DEBUG
(setf drakma:*header-stream* *standard-output*)

(defun fetch-json (this-url)
  "gets JSON with this URL only when response-code is 200"
  (let ((cl-json:*json-identifier-name-to-lisp* (lambda (x) x)))
    (multiple-value-bind (body response-code)
        (http-request this-url :want-stream t)
      (setf (flex:flexi-stream-external-format body) :utf-8)
      (ecase response-code
        (200 (cl-json:decode-json body))))))

;;; RE Pattern 
(defparameter *parameter-pattern* "{([a-zA-Z\-]+)}")

(defun parse-path-parameters (path)
  "returns two values, 1st is non param path element, 2nd are the params.
   ex) /PARAM1/{PARAM2} ==> ((\"PARAM1\") (\"PARAM2\"))"
  (values-list (mapcar #'nreverse
                       (reduce
                        (lambda (acc v)
                          (if (string= "" v)
                              acc
                              (let ((param (cl-ppcre:register-groups-bind (param)
                                               (*parameter-pattern* v) param)))
                                (if param
                                    (list (first acc) (push param (second acc)))
                                    (list (push v (first acc)) (second acc))))))
                        (cl-ppcre:split "/" (string path))
                        :initial-value (list nil nil)))))

(defun normalize-path-name (name)
  "string --> A-B-C"
  (string-upcase (format nil "~{~A~^-~}" (parse-path-parameters name))))

(defun normalize-path-url (path-url)
  "string --> A/B/C"
  (string-upcase (format nil "~{~A~^/~}" (parse-path-parameters path-url))))

(defun get-in (this-items alist)
  "get lists related to this-items"
  (if (endp this-items) alist
      (get-in (rest this-items)
              (cdr (assoc (car this-items) alist)))))

(defun get-basepath (json)
  "gets base-path"
  (get-in '(:|basePath|) json))

(defun get-schemes (json)
  "gets schemes"
  (first (get-in '(:|schemes|) json)))

(defun get-host (json)
  "gets hostname"
  (get-in '(:|host|) json))

(defun make-urls (json)
  "scheme + hostname + basepath"
  (concatenate 'string (get-schemes json) "://" (get-host json) (get-basepath json)))



(defun rest-call (host url-path
                  &key params content basic-authorization
                    (method :get)
                    (accept "application/json")
                    (content-type "application/json"))
  "call http-request with basic params and conteent and authorization"
  (multiple-value-bind (stream code)
      (drakma:http-request (format nil "~a~a" host url-path) :parameters params :content content :basic-authorization basic-authorization :accept accept :content-type content-type :want-stream t :method method)
    (if (and (>= code 200) (< code 300))
        (progn (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
               (cl-json:decode-json stream))
        (error "REST ~a call failed with code ~a" method code))))

(defparameter +http-methods+
  (list :|get| :|post| :|delete| :|patch|))


(defun generate-client-with-json (json filepath
                                  &key (accept "application/json")
                                    (accept-type "application/json")
                                    (package-name "swagger-client"))
  "Generate lisp code for a swagger client with using a Swgger/OpenAPI spec in JSON,
and write it to FILEPATH."
  (with-open-file (*standard-output* filepath :direction :output :if-exists :supersede)
    ;; (format t "(ql:quickload \"drakma\")~%(ql:quickload \"cl-json\")~%")
    (format t "(in-package :~a)\n\n" (string-downcase package-name))
    ;; emit the REST-CALL function into the output file
    (rest-call-function)
    (loop :for paths :in (get-in '(:|paths|) json)
          do (loop :for path :in (rest paths)
                   :do ;;(format t "~%~A==>~A~%" (first paths) (first path))
                      (when (member (first path) +http-methods+)
                        (multiple-value-bind (fnames options)
                            (parse-path-parameters (first paths))
                          (declare (ignorable fnames))
                          ;;(format t " ~A ==> ~A ~%" fnames options)
                          (let ((tmp  `((:baseurl . ,(lambda () (make-urls json)))
                                        (:paths . ,(lambda () (car paths)))
                                        (:path-name . ,(lambda () (string-downcase (normalize-path-name (first paths)))))
                                        (:path-url . ,(first paths))
                                        (:first-name . ,(lambda () (string-downcase (format nil "~A" (first path)))))
                                        (:method . ,(lambda() (format nil ":~A" (first path))))
                                        (:description . ,(lambda() (format nil "~A" (cl-ppcre:regex-replace-all "\\n" (get-in '(:|description|) (cdr path)) "\\n"))))
                                        (:accept . ,accept)
                                        (:accept-type . ,accept-type))))
                            (if options
                                (rest-call-template-v2 tmp)
                                (rest-call-template-v1 tmp)))))))
    (convert-json-template)))

#+ignore
(defun parse-responses (json-list)
  (assert (member (first json-list) +http-methods+))
  (let ((response-list (get-in '(:|responses|) (rest json-list))))
    (iter (for (response-code . properties) in response-list)
      (as response-numeral = (parse-integer (symbol-name response-code)))
      (as success = (and (< response-numeral 300) (>= response-number 200)))
      (let ((response)))
      )
    )
  )


(defun generate-client (url-or-pathname filepath
                        &key (accept "application/json") (accept-type "application/json"))
  "Client code generation function.  Takes either a URL or a CL:PATHNAME as argument,
and writes a swagger client to FILEPATH.
   The optional arguments are, at the moment, ignored."
  (let ((json (if (typep url-or-pathname 'pathname)
                  (cl-json:decode-json-from-source url-or-pathname)
                  (fetch-json url-or-pathname))))
    (generate-client-with-json json
                               filepath :accept accept :accept-type accept-type)))

;;(with-output-to-string (st) (run-program "curl" '("-ks" "-u" "mapr:mapr" "https://172.16.28.138:8443/rest/alarm/list") :output st))
