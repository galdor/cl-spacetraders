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

(defun build-contract-good (data)
  (let ((good (make-instance 'contract-good)))
    (dolist (entry data good)
      (case (car entry)
        (trade-symbol
         (setf (contract-good-symbol good) (cdr entry)))
        (destination-symbol
         (setf (contract-good-destination good) (cdr entry)))
        (units-required
         (setf (contract-good-required-units good) (cdr entry)))
        (units-fulfilled
         (setf (contract-good-delivered-units good) (cdr entry)))))))

(defclass contract ()
  ((id :type string :accessor contract-id)
   (faction :type string :accessor contract-faction)
   (type :type symbol :accessor contract-type)
   (goods :type list :accessor contract-goods)
   (accepted :type boolean :accessor contract-accepted)
   (fulfilled :type boolean :accessor contract-fulfilled)
   (expiration-time :type time:datetime :accessor contract-expiration-time)
   (deadline :type time:datetime :accessor contract-deadline)
   (contract-initial-payment :type integer :accessor contract-initial-payment)
   (contract-final-payment :type integer :accessor contract-final-payment)))

(defmethod print-object ((contract contract) stream)
  (print-unreadable-object (contract stream :type t)
    (with-slots (id) contract
      (format stream "~A" id))))

(defun build-contract (data)
  (let ((contract (make-instance 'contract)))
    (dolist (entry data contract)
      (case (car entry)
        (id
         (setf (contract-id contract) (cdr entry)))
        (faction-symbol
         (setf (contract-faction contract) (cdr entry)))
        (type
         (setf (contract-type contract) (cdr entry)))
        (terms
         (build-contract/terms (cdr entry) contract))
        (accepted
         (setf (contract-accepted contract) (cdr entry)))
        (fulfilled
         (setf (contract-fulfilled contract) (cdr entry)))
        (expiration
         (setf (contract-expiration-time contract)
               (time:parse-rfc3339-datetime (cdr entry))))))))

(defun build-contract/terms (data contract)
  (declare (type contract contract))
  (dolist (entry data contract)
    (case (car entry)
      (deadline
       (setf (contract-deadline contract)
             (time:parse-rfc3339-datetime (cdr entry))))
      (payment
       (build-contract/payment (cdr entry) contract))
      (deliver
       (setf (contract-goods contract)
             (map 'list 'build-contract-good (cdr entry)))))))

(defun build-contract/payment (data contract)
  (declare (type contract contract))
  (dolist (entry data contract)
    (case (car entry)
      (on-accepted
       (setf (contract-initial-payment contract) (cdr entry)))
      (on-fulfilled
       (setf (contract-final-payment contract) (cdr entry))))))
