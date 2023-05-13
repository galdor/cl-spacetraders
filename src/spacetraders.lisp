(in-package :spacetraders)

(define-condition unknown-faction (error)
  ((symbol
    :type string
    :initarg :symbol
    :reader unknown-faction-symbol))
  (:report
   (lambda (condition stream)
     (with-slots (symbol) condition
       (format stream "Unknown faction ~S." symbol)))))

(define-condition unknown-contract (error)
  ((id
    :type string
    :initarg :id
    :reader unknown-contract-symbol))
  (:report
   (lambda (condition stream)
     (with-slots (id) condition
       (format stream "Unknown contract ~S." id)))))

(defun register (symbol faction)
  (declare (type string symbol faction))
  (let ((body `((symbol . ,symbol)
                (faction . ,faction))))
    (send-api-request "register" :body body :public t)))

(defun fetch-agent ()
  (let ((data (call-api "get-my-agent")))
    (build-agent data)))

(defun fetch-faction (symbol)
  (declare (type string symbol))
  (api-error-bind
      ((404 (error 'unknown-faction :symbol symbol)))
    (let* ((parameters `((:path "factionSymbol" ,symbol)))
           (data (call-api "get-faction" :parameters parameters)))
      (build-faction data))))

(defun fetch-ships ()
  (let* ((data (call-api "get-my-ships" :paginated t :pagination-limit 20)))
    (mapcar 'build-ship data)))

(defun fetch-contracts ()
  (let* ((data (call-api "get-contracts" :paginated t :pagination-limit 20)))
    (mapcar 'build-contract data)))

(defun accept-contract (id)
  (declare (type string id))
  (api-error-bind
      ((404 (error 'unknown-contract :id id)))
    (let ((parameters `((:path "contractId" ,id))))
      (call-api "accept-contract" :parameters parameters))))
