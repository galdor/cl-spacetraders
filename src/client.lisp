(in-package :spacetraders)

(defvar *client* nil)

(defclass client ()
  ((factions
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :accessor client-factions)
   (agent
    :type agent
    :accessor client-agent)
   (ships
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :accessor client-ships)
   (contracts
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :accessor client-contracts)))

(defun make-client ()
  (make-instance 'client))

(defun initialize-client ()
  (let ((client (make-client)))
    (setf (client-agent client) (fetch-agent))
    (let ((factions (make-hash-table :test #'equal))
        (ships (make-hash-table :test #'equal))
        (contracts (make-hash-table :test #'equal)))
    (dolist (faction (fetch-factions))
      (setf (gethash (faction-symbol faction) factions) faction))
    (dolist (ship (fetch-ships))
      (setf (gethash (ship-symbol ship) ships) ship))
    (dolist (contract (fetch-contracts))
      (setf (gethash (contract-id contract) contracts) contract))
    (setf (client-factions client) factions
          (client-ships client) ships
          (client-contracts client) contracts))
    (setf *client* client)))

(defun agent ()
  (client-agent *client*))
