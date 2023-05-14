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
    (create-from-api-data 'agent data)))

(defun fetch-faction (symbol)
  (declare (type string symbol))
  (api-error-bind
      ((404 (error 'unknown-faction :symbol symbol)))
    (let* ((parameters `((:path "factionSymbol" ,symbol)))
           (data (call-api "get-faction" :parameters parameters)))
      (create-from-api-data 'faction data))))

(defun fetch-ships ()
  (let* ((data (call-api "get-my-ships" :paginated t :pagination-limit 20)))
    (create-from-api-data 'ship data)))

(defun fetch-contracts ()
  (let* ((data (call-api "get-contracts" :paginated t :pagination-limit 20)))
    (create-from-api-data 'contract data)))

(defun accept-contract (id)
  (declare (type string id))
  (api-error-bind
      ((404 (error 'unknown-contract :id id)))
    (let* ((parameters `((:path "contractId" ,id)))
           (data (call-api "accept-contract" :parameters parameters)))
      (values (create-from-api-data 'agent (alist-getf 'agent data))
              (create-from-api-data 'contract (alist-getf 'contract data))))))

(defun deliver-contract (id ship-symbol item-symbol units)
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
      (values (create-from-api-data 'contract (alist-getf 'contract data))
              (alist-getf 'cargo data)))))

(defun fulfill-contract (id)
  (declare (type string id))
  (api-error-bind
      ((404 (error 'unknown-contract :id id)))
    (let* ((parameters `((:path "contractId" ,id)))
           (data (call-api "fulfill-contract" :parameters parameters)))
      (values (create-from-api-data 'agent (alist-getf 'agent data))
              (create-from-api-data 'contract (alist-getf 'contract data))))))
