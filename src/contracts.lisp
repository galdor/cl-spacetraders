(in-package :spacetraders)

(defun list-contracts ()
  (core:hash-table-values (client-contracts *client*)))

(defun contract (id)
  (declare (type string id))
  (or (gethash id (client-contracts *client*))
      (error 'unknown-contract :id id)))

(defun accept-contract (contract)
  (declare (type contract contract))
  (with-slots (agent contracts) *client*
    (multiple-value-bind (agent-data contract-data)
        (call-api/accept-contract (contract-id contract))
      (update-from-api-data agent agent-data)
      (update-from-api-data contract contract-data))))

(defun deliver-contract (contract ship item-symbol units)
  (declare (type contract contract)
           (type ship ship)
           (type string item-symbol)
           (type integer units))
  (with-slots (contracts) *client*
    (multiple-value-bind (contract-data cargo-data)
        (call-api/deliver-contract (contract-id contract)
                                   (ship-symbol ship)
                                   item-symbol
                                   units)
      (update-from-api-data contract contract-data)
      (update-ship-from-cargo-api-data ship cargo-data))))

(defun fulfill-contract (contract)
  (declare (type contract contract))
  (with-slots (agent contracts) *client*
    (multiple-value-bind (agent-data contract-data)
        (call-api/fulfill-contract (contract-id contract))
      (update-from-api-data agent agent-data)
      (update-from-api-data contract contract-data))))
