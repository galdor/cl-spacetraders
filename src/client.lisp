(in-package :spacetraders)

(defvar *client* nil)

(defclass client ()
  ((systems
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :accessor client-systems)
   (agent
    :type agent
    :accessor client-agent)
   (factions
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :accessor client-factions)
   (ships
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :accessor client-ships)
   (contracts
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :accessor client-contracts)))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type t)
    (when (slot-boundp client 'agent)
      (format stream "~A" (agent-symbol (client-agent client))))))

(defun make-client ()
  (make-instance 'client))

(defun initialize-client ()
  (let ((client (make-client)))
    (setf (client-systems client) (load-systems))
    (setf (client-agent client) (call-api/get-my-agent))
    (let ((factions (make-hash-table :test #'equal))
          (ships (make-hash-table :test #'equal))
          (contracts (make-hash-table :test #'equal)))
      (dolist (faction (call-api/get-factions))
        (setf (gethash (faction-symbol faction) factions) faction))
      (dolist (ship (call-api/get-my-ships))
        (setf (gethash (ship-symbol ship) ships) ship))
      (dolist (contract (call-api/get-contracts))
        (setf (gethash (contract-id contract) contracts) contract))
      (setf (client-factions client) factions
            (client-ships client) ships
            (client-contracts client) contracts))
    (setf *client* client)))

(defun find-system (symbol)
  (declare (type string symbol))
  (with-slots (systems) *client*
    (or (gethash symbol systems)
        (error 'unknown-system :symbol symbol))))

(defun agent ()
  (client-agent *client*))

(defun list-factions ()
  (core:hash-table-values (client-factions *client*)))

(defun faction (symbol)
  (declare (type string symbol))
  (or (gethash symbol (client-factions *client*))
      (error 'unknown-faction :symbol symbol)))

(defun list-ships ()
  (core:hash-table-values (client-ships *client*)))

(defun ship (symbol)
  (declare (type string symbol))
  (or (gethash symbol (client-ships *client*))
      (error 'unknown-ship :symbol symbol)))

(defmacro do-ships ((ship) &body body)
  `(maphash (lambda (symbol ship)
              (declare (ignore symbol))
              (let ((,ship ship))
                ,@body))
            (client-ships *client*)))

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
