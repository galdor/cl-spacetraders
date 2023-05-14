(in-package :spacetraders)

(defclass contract-good ()
  ((symbol :type string :accessor contract-good-symbol)
   (destination :type string :accessor contract-good-destination)
   (required-units :type integer :accessor contract-good-required-units)
   (delivered-units :type integer :accessor contract-good-delivered-units)))

(defmethod print-object ((good contract-good) stream)
  (print-unreadable-object (good stream :type t)
    (with-slots (symbol required-units delivered-units) good
      (format stream "~A ~D/~D" symbol delivered-units required-units))))

(defmethod update-from-api-data ((good contract-good) data)
  (alist-case (value data good)
    (trade-symbol
      (setf (contract-good-symbol good) value))
    (destination-symbol
      (setf (contract-good-destination good) value))
    (units-required
      (setf (contract-good-required-units good) value))
    (units-fulfilled
      (setf (contract-good-delivered-units good) value))))

(defclass contract ()
  ((id :type string :accessor contract-id)
   (faction :type string :accessor contract-faction)
   (type :type symbol :accessor contract-type)
   (goods :type list :accessor contract-goods)
   (accepted :type boolean :accessor contract-accepted)
   (fulfilled :type boolean :accessor contract-fulfilled)
   (expiration-time :type time:datetime :accessor contract-expiration-time)
   (deadline :type time:datetime :accessor contract-deadline)
   (initial-payment :type integer :accessor contract-initial-payment)
   (final-payment :type integer :accessor contract-final-payment)))

(defmethod print-object ((contract contract) stream)
  (print-unreadable-object (contract stream :type t)
    (with-slots (id) contract
      (format stream "~A" id))))

(defmethod update-from-api-data ((contract contract) data)
  (alist-case (value data contract)
    (id
      (setf (contract-id contract) value))
    (faction-symbol
      (setf (contract-faction contract) value))
    (type
      (setf (contract-type contract) value))
    (terms
      (alist-case (value value)
        (deadline
          (setf (contract-deadline contract)
                (time:parse-rfc3339-datetime value)))
        (payment
          (alist-case (value value)
            (on-accepted
              (setf (contract-initial-payment contract) value))
            (on-fulfilled
              (setf (contract-final-payment contract) value))))
        (deliver
          (setf (contract-goods contract)
                (create-from-api-data 'contract-good value)))))
    (accepted
      (setf (contract-accepted contract) value))
    (fulfilled
      (setf (contract-fulfilled contract) value))
    (expiration
      (setf (contract-expiration-time contract)
            (time:parse-rfc3339-datetime value)))))
