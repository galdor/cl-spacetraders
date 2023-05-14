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
