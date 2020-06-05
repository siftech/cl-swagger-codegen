(in-package "CL-SWAGGER")

;;; drakma:*header-stream* for DEBUG

;;; RE Pattern 
(defparameter *parameter-pattern* "({.*})")

(defun split-path (path-desig)
  "Return a list of strings for the slash-separated path components."
  (remove ""
          (cl-ppcre:split "/" (string path-desig))
          :test 'equalp))

(defun match-param (x)
  "Either matches a parameter from a path component (a string surrounded by
curly braces), and returns it, or returns NIL."
  (cl-ppcre:register-groups-bind (param)
                    (*parameter-pattern* x)
                  param))

(defun parse-path-parameters (path)
  "returns two values, 1st is non param path element, 2nd are the params.
   ex) /PARAM1/{PARAM2} ==> ((\"PARAM1\") (\"PARAM2\"))"
  (iter (for path-component in (split-path path))
    (as param = (match-param path-component))
    (if param
        (collecting param into param-list)
        (collect path-component into path-list))
    (finally (return (values path-list param-list)))))

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

(defun make-url (scheme hostname basepath)
  (concatenate 'string scheme "://" hostname basepath))

(defun join (separator list)
  "The same as perl or python join: return a string with the
components of LIST separated by SEPARATOR."
  (with-output-to-string (out)
    (loop for (element . more) on list do (princ element out)
          when more
            do (princ separator out))))

(defparameter +http-methods+
  (list :|get| :|post| :|delete| :|patch|))

(defun path-name (path)
  "Return a name for a path.  A path is a partial URL.
This will be used as part of the function name"
  (string-downcase (normalize-path-name path)))


(defun get-servers-url (json-api)
  "Either gets server information (like versioning) to
add to the base url, or returns nil."
  (alexandria:if-let (servers-info (cl-swagger::get-in '(:|servers|) json-api))
    (alexandria:if-let (url (cl-swagger::get-in '(:|url|) (first servers-info)))
      ;; because of the way this is spliced together to make a URL, we have
      ;; to remove any initial /
      (string-left-trim "/" url))))


(defun generate-client-with-json (json filepath
                                  &key (accept "application/json")
                                    (accept-type "application/json")
                                    (package-name "swagger-client"))
  "Generate lisp code for a swagger client with using a Swgger/OpenAPI spec in JSON,
and write it to FILEPATH."
  (with-open-file (*standard-output* filepath :direction :output :if-exists :supersede)
    (format t "(in-package :~a)~%~%" (string-downcase package-name))
    (let ((server-info (get-servers-url json)))
      (iter (for path-set in (get-in '(:|paths|) json))
        (as pathset-name = (path-name (first path-set)))
        (multiple-value-bind (fnames options)
            (parse-path-parameters (first path-set))
          (as base-url = (make-url (get-schemes json)
                                   (get-host json)
                                   (if server-info
                                       (join #\/ (cons server-info fnames))
                                       (join #\/ fnames))))
          (iter (for path in (rest path-set))
            (as http-method = (first path))
            (when (member http-method +http-methods+)
              (let ((tmp  `((:baseurl . ,base-url)
                            (:paths . (first path-set))
                            (:path-name . ,pathset-name)
                            (:path-url . ,(first path-set))
                            (:first-name . ,(string-downcase (format nil "~A" http-method)))
                            (:method . ,(format nil ":~A" http-method))
                            (:description . ,(format nil "~A" (cl-ppcre:regex-replace-all "\\n" (get-in '(:|description|) (rest path)) "\\n")))
                            (:accept . ,accept)
                            (:accept-type . ,accept-type))))
                (if options
                    (rest-call-template-v2 tmp)
                    (rest-call-template-v1 tmp))))))))))


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
                        &key (accept "application/json") (accept-type "application/json")
                          (package-name 'cl-swagger))
  "Client code generation function.  Takes either a URL or a CL:PATHNAME as argument,
and writes a swagger client to FILEPATH.
   The optional arguments are, at the moment, ignored."
  (let ((json
          (if (typep url-or-pathname 'pathname)
              (decode-json-from-source url-or-pathname)
              (fetch-json url-or-pathname))))
    (generate-client-with-json json filepath
                               :package-name package-name
                               :accept accept :accept-type accept-type)))

;;(with-output-to-string (st) (run-program "curl" '("-ks" "-u" "mapr:mapr" "https://172.16.28.138:8443/rest/alarm/list") :output st))
