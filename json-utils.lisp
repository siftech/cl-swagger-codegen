(in-package "CL-SWAGGER")

(defun decode-json (string)
  "Decode JSON with settings that cl-swagger expects."
  (cl-json:with-decoder-simple-list-semantics 
    (let ((json:*json-identifier-name-to-lisp* 'identity))
      (cl-json:decode-json body))))

(defun decode-json-from-source (source)
  "Decode JSON with settings that cl-swagger expects."
  (cl-json:with-decoder-simple-list-semantics
    (let ((json:*json-identifier-name-to-lisp* 'identity))
      (cl-json:decode-json-from-source source))))

(defun fetch-json (this-url)
  "Gets JSON with this URL only when response-code is 2xx.
For JSON decoding, use settings expected by cl-swagger."
  (multiple-value-bind (body response-code)
      (http-request this-url :want-stream t)
    (setf (flex:flexi-stream-external-format body) :utf-8)
    (cond ((typep response-code 'success-response)
           (decode-json body))
          (t (error "Expected a 2xx response code, got ~d" response-code)))))

(defun 2xx-p (num)
  (and (>= 200 response-code) (<= response-code 300)))

(deftype success-response ()
  '(and integerp (satisifes 2xx-p)))
