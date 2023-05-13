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

(defun register (symbol faction)
  (declare (type string symbol faction))
  (let ((body `(("symbol" . ,symbol)
                ("faction" . ,faction))))
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
