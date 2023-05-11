(in-package :spacetraders)

(defparameter *default-api-uri*
  (uri:make-uri :scheme "https" :host "api.spacetraders.io" :path "/v2"))

(defparameter *mock-api-uri*
  (uri:make-uri :scheme "https" :host "stoplight.io"
                :path "/mocks/spacetraders/spacetraders/96627693"))

(defvar *api-uri* *default-api-uri*)

(defparameter *authentication-token* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun load-openapi-document ()
    (let ((path (asdf:system-relative-pathname "spacetraders" "data/api.json")))
      (openapi:parse-document
       (system:read-file path :external-format :utf-8)))))

(defvar *api-openapi-document* (load-openapi-document))

(define-condition http-error (error)
  ((response
    :type http:response
    :initarg :response))
  (:report
   (lambda (condition stream)
     (with-slots (response) condition
       (let ((status (http:response-status response))
             (body (ignore-errors
                    (text:decode-string (http:response-body response)))))
         (format stream "Request failed with status ~D~@[: ~A~]."
                 status body))))))

(define-condition api-error (http-error)
  ((message
    :type string
    :initarg :message
    :accessor api-error-message)
   (code
    :type integer
    :initarg :code
    :accessor api-error-code)
   (data
    :initarg :data
    :initform nil
    :accessor api-error-data))
  (:report
   (lambda (condition stream)
     (with-slots (response message code) condition
       (let ((status (http:response-status response)))
         (format stream "Request failed with status ~D: error ~D: ~A"
                 status code message))))))

(json:define-mapping api-error
  :object
  :members
  (("message" message (:string))
   ("code" code (:integer))
   ("data" data (:any)))
  :required
  ("message" "code"))

(json:define-mapping api-error-response
  :object
  :members
  (("error" error api-error))
  :required
  ("error"))

(defun decode-api-error (response)
  (declare (type http:response response))
  (let* ((body (text:decode-string (http:response-body response)))
         (response-value (json:parse body :mapping 'api-error-response))
         (error-value (cdr (assoc 'error response-value)))
         (error (make-condition 'api-error :response response)))
    (dolist (entry error-value error)
      (case (car entry)
        (message
         (setf (api-error-message error) (cdr entry)))
        (code
         (setf (api-error-code error) (cdr entry)))
        (data
         (setf (api-error-data error) (cdr entry)))))))

(defun call-api (operation-name &key parameters body public)
  (declare (type string operation-name)
           (type list parameters))
  (let ((openapi:*server-uri* *api-uri*)
        (http:*client-netrc-authorization-scheme*
          (when (and (not public) (null *authentication-token*))
            :bearer))
        (header nil))
    (when (and (not public) *authentication-token*)
      (push (cons "Authorization"
                  (http:bearer-authorization-header-field-value
                   *authentication-token*))
            header))
    (handler-case
        (let ((response-data
                (openapi:execute-operation *api-openapi-document*
                                           operation-name
                                           :parameters parameters
                                           :body body
                                           :header header)))
          ;; All endpoints but /register return the response as an object where
          ;; actual data are the value of the "data" entry.
          (or (cdr (assoc 'data response-data)) response-data))
      (openapi:unexpected-response-status (condition)
        (let* ((response
                 (openapi:unexpected-response-status-response condition))
               (api-error (ignore-errors (decode-api-error response))))
          (if api-error
              (error api-error)
              (error 'http-error :response response))))
      (openapi:unexpected-response-content-type (condition)
        (let* ((response
                 (openapi:unexpected-response-content-type-response condition)))
          (error 'http-error :response response))))))

(defmacro api-error-bind (forms &body body)
  `(block nil
     (handler-bind
         ((api-error
            (lambda (condition)
              (case (api-error-code condition)
                ,@forms))))
       (progn ,@body))))