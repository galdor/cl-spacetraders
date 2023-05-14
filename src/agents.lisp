(in-package :spacetraders)

(defclass agent ()
  ((id :type string :accessor agent-id)
   (symbol :type string :accessor agent-symbol)
   (headquarters :type string :accessor agent-headquarters)
   (credits :type integer :accessor agent-credits)))

(defmethod print-object ((agent agent) stream)
  (print-unreadable-object (agent stream :type t)
    (with-slots (symbol) agent
      (format stream "~A" symbol))))

(defmethod update-from-api-data ((agent agent) data)
  (alist-case (value data agent)
    (account-id
      (setf (agent-id agent) value))
    (symbol
      (setf (agent-symbol agent) value))
    (headquarters
      (setf (agent-headquarters agent) value))
    (credits
      (setf (agent-credits agent) value))))
