(in-package :cl-swagger.utils)

(defun decode-json (string)
  "Decode JSON with settings that cl-swagger expects."
  (cl-json:with-decoder-simple-list-semantics 
    (let ((json:*json-identifier-name-to-lisp* 'identity))
      (cl-json:decode-json string))))

(defun decode-json-from-source (source)
  "Decode JSON with settings that cl-swagger expects."
  (cl-json:with-decoder-simple-list-semantics
    (let ((json:*json-identifier-name-to-lisp* 'identity))
      (cl-json:decode-json-from-source source))))

(defun fetch-json (this-url)
  "Gets JSON with this URL only when response-code is 2xx.
For JSON decoding, use settings expected by cl-swagger."
  (multiple-value-bind (body response-code)
      (drakma:http-request this-url :want-stream t)
    (setf (flex:flexi-stream-external-format body) :utf-8)
    (cond ((typep response-code 'success-response)
           (decode-json body))
          (t (error "Expected a 2xx response code, got ~d" response-code)))))


;; (convert-json #'function \"/path\" content-json)
(defun convert-json (query-fun path body)
  (multiple-value-bind (code stream head)
      (funcall query-fun path body)
    (declare (ignore head))
    (if (typep code 'success-response)
        (progn
          (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
          (decode-json stream))
        (error "CONVERT-JSON failed with code ~a" code))))

(defun rest-call (host url-path
                  &key params content basic-authorization
                    (method :get)
                    (accept "application/json")
                    (content-type "application/json")
                    (debug t))
  "call http-request with basic params and content and authorization"
  (let ((request-url (format nil "~a~a" host url-path)))
    (flet ((make-request ()
             (drakma:http-request request-url
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
        
        (if (typep code 'success-response)
            (progn (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
                   (decode-json stream))
            (error "REST ~a call to ~a failed with code ~a" method request-url code))))))

(defun 2xx-p (num)
  (and (>= 200 num) (<= num 300)))

(deftype success-response ()
  '(and integerp (satisifes 2xx-p)))

(defun stream-to-string (stream)
  (with-output-to-string (out)
    (loop :for new = (read-line stream nil nil)
          :while new
          :do (print new out))))
