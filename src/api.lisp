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
    (let ((response-data
            (openapi:execute-operation *api-openapi-document* operation-name
                                       :parameters parameters
                                       :body body
                                       :header header)))
      ;; All endpoints but /register return the response as an object where
      ;; actual data are the value of the "data" entry.
      (or (cdr (assoc 'data response-data)) response-data))))
