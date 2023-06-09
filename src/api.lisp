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
    :initarg :response
    :reader http-error-response))
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

(define-condition rate-limit-reached (api-error)
  ((delay
    :type float
    :initarg :delay
    :accessor rate-limit-reached-delay)))

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

(json:define-mapping rate-limit-error-data
  :object
  :members
  (("type" type (:string))
   ("retryAfter" retry-after (:integer))
   ("limitBurst" limit-burst (:integer))
   ("limitPerSecond" limit-per-second (:integer))
   ("remaining" remaining (:integer))
   ("reset" reset (:string)))
  :required
  ("retryAfter"))

(defun decode-api-error (response)
  (declare (type http:response response))
  (let* ((body (text:decode-string (http:response-body response)))
         (response-value (json:parse body :mapping 'api-error-response))
         (error-value (cdr (assoc 'error response-value)))
         (error-class (case (http:response-status response)
                        (429 'rate-limit-reached)
                        (t   'api-error)))
         (error (make-condition error-class :response response)))
    (dolist (entry error-value)
      (case (car entry)
        (message
         (setf (api-error-message error) (cdr entry)))
        (code
         (setf (api-error-code error) (cdr entry)))
        (data
         (setf (api-error-data error) (cdr entry)))))
    (case (http:response-status response)
      (429
       (let* ((data (api-error-data error))
              (value (ignore-errors
                      (json:validate data 'rate-limit-error-data)))
              (delay (or (cdr (assoc 'retry-after value)) 1)))
         (setf (rate-limit-reached-delay error) (float delay 0.0d0)))))
    error))

(defun call-api (operation-name &key parameters body paginated
                                     (pagination-limit 20))
  (declare (type string operation-name)
           (type list parameters)
           (type boolean paginated))
  (let ((page 1))
    (flet ((send-request (&aux (parameters (copy-seq parameters)))
             (loop
               (restart-case
                   (progn
                     (when paginated
                       (push `(:query "page" ,(princ-to-string page))
                             parameters)
                       (push
                        `(:query "limit" ,(princ-to-string pagination-limit))
                        parameters))
                     (let ((response-data
                             (send-api-request operation-name
                                               :parameters parameters
                                               :body body)))
                       (return-from send-request
                         (cdr (assoc 'data response-data)))))
                 (retry ()
                   :report "Send the same request again.")))))
      (cond
        (paginated
         (do ((all-data (make-array 0 :adjustable t :fill-pointer 0)))
             (nil)
           (let ((data (send-request)))
             (do ((i 0 (1+ i)))
                 ((>= i (length data))
                  nil)
               (vector-push-extend (aref data i) all-data))
             (cond
               ((< (length data) pagination-limit)
                (return all-data))
               (t
                (incf page))))))
        (t
         (send-request))))))

(defun send-api-request (operation-name &key parameters body public)
  (declare (type string operation-name)
           (type list parameters)
           (type boolean public))
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
        (openapi:execute-operation *api-openapi-document*
                                   operation-name
                                   :parameters parameters
                                   :body body
                                   :header header)
      (openapi:unexpected-response-status (condition)
        (let* ((response
                 (openapi:unexpected-response-status-response condition))
               (api-error (ignore-errors (decode-api-error response))))
          (if api-error
              (error api-error)
              (error 'http-error :response response))))
      (openapi:unexpected-response-content-type (condition)
        (let* ((response (openapi:unexpected-response-content-type-response
                          condition)))
          (error 'http-error :response response))))))

(defun register (symbol faction)
  (declare (type string symbol faction))
  (let ((body `((symbol . ,symbol)
                (faction . ,faction))))
    (send-api-request "register" :body body :public t)))

(defmacro api-error-bind (forms &body body)
  `(block nil
     (handler-bind
         ((api-error
            (lambda (condition)
              (case (api-error-code condition)
                ,@forms))))
       (progn ,@body))))

;;;
;;; Systems
;;;

(defun call-api/get-shipyard (waypoint-symbol)
  (declare (type string waypoint-symbol))
  (let* ((system-symbol (waypoint-symbol-system-symbol waypoint-symbol))
         (parameters `((:path "systemSymbol" ,system-symbol)
                       (:path "waypointSymbol" ,waypoint-symbol))))
    (call-api "get-shipyard" :parameters parameters)))

;;;
;;; Agents
;;;

(defun call-api/get-my-agent ()
  (let ((data (call-api "get-my-agent")))
    (create-from-api-data 'agent data)))

;;;
;;; Factions
;;;

(defun call-api/get-faction (symbol)
  (declare (type string symbol))
  (api-error-bind
      ((404 (error 'unknown-faction :symbol symbol)))
    (let* ((parameters `((:path "factionSymbol" ,symbol)))
           (data (call-api "get-faction" :parameters parameters)))
      (create-from-api-data 'faction data))))

(defun call-api/get-factions ()
  (let* ((data (call-api "get-factions" :paginated t :pagination-limit 20)))
    (create-from-api-data 'faction data)))

;;;
;;; Ships
;;;

(defun call-api/get-my-ships ()
  (let* ((data (call-api "get-my-ships" :paginated t :pagination-limit 20)))
    (create-from-api-data 'ship data)))

;;;
;;; Contracts
;;;

(defun call-api/get-contracts ()
  (let* ((data (call-api "get-contracts" :paginated t :pagination-limit 20)))
    (create-from-api-data 'contract data)))

(defun call-api/accept-contract (id)
  (declare (type string id))
  (api-error-bind
      ((404 (error 'unknown-contract :id id)))
    (let* ((parameters `((:path "contractId" ,id)))
           (data (call-api "accept-contract" :parameters parameters)))
      (values (alist-getf 'agent data)
              (alist-getf 'contract data)))))

(defun call-api/deliver-contract (id ship-symbol item-symbol units)
  (declare (type string id ship-symbol item-symbol)
           (type integer units))
  (api-error-bind
      ((404 (error 'unknown-contract :id id)))
    (let* ((parameters `((:path "contractId" ,id)))
           (body `((ship-symbol . ,ship-symbol)
                   (trade-symbol . ,item-symbol)
                   (units . ,units)))
           (data
             (call-api "deliver-contract" :parameters parameters :body body)))
      (values (alist-getf 'contract data)
              (alist-getf 'cargo data)))))

(defun call-api/fulfill-contract (id)
  (declare (type string id))
  (api-error-bind
      ((404 (error 'unknown-contract :id id)))
    (let* ((parameters `((:path "contractId" ,id)))
           (data (call-api "fulfill-contract" :parameters parameters)))
      (values (alist-getf 'agent data)
              (alist-getf 'contract data)))))
