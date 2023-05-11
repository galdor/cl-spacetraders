(in-package :spacetraders)

(defun register (symbol faction)
  (declare (type string symbol faction))
  (let ((body `(("symbol" . ,symbol)
                ("faction" . ,faction))))
    (call-api "register" :body body :public t)))

(defun find-faction (name)
  (declare (type string name))
  (api-error-bind
      ((404 (return-from find-faction nil)))
    (let* ((parameters `((:path "factionSymbol" ,name)))
           (data (call-api "get-faction" :parameters parameters)))
      (build-faction data))))
