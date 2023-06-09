(in-package :spacetraders)

(defgeneric update-from-api-data (object data))

(defun create-from-api-data (class data)
  (declare (type (or symbol class) class)
           (type (or list vector) data))
  (etypecase data
    (list
     (update-from-api-data (make-instance class) data))
    (vector
     (map 'list (lambda (value)
                  (create-from-api-data class value))
          data))))

;;;
;;; Points
;;;

(defstruct (point
            (:constructor nil))
  (x 0 :type integer)
  (y 0 :type integer))

(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream :type t)
    (with-slots (x y) point
      (serialize-point point :stream stream))))

(defun point (x y)
  (declare (type integer x y))
  (let ((point (make-instance 'point)))
    (setf (slot-value point 'x) x)
    (setf (slot-value point 'y) y)
    point))

(defun serialize-point (point &key stream)
  (declare (type point point))
  (with-slots (x y) point
    (format stream "~D,~D" x y)))

(defun data-point (data)
  (point (cdr (assoc 'x data))
         (cdr (assoc 'y data))))

;;;
;;; Waypoints
;;;

(defclass waypoint ()
  ((symbol :type string :accessor waypoint-symbol)
   (type :type symbol :accessor waypoint-type)
   (point :type point :initarg :point :accessor waypoint-point)))

(defmethod print-object ((waypoint waypoint) stream)
  (print-unreadable-object (waypoint stream :type t)
    (with-slots (symbol type point) waypoint
      (format stream "~A ~A " symbol type)
      (serialize-point point :stream stream))))

(defmethod update-from-api-data ((waypoint waypoint) data)
  (let (x y)
    (alist-case (value data)
      (symbol
        (setf (waypoint-symbol waypoint) value))
      (type
        (setf (waypoint-type waypoint) value))
      (x
        (setf x value))
      (y
        (setf y value)))
    (setf (waypoint-point waypoint) (point x y))
    waypoint))

;;;
;;; Systems
;;;

(defclass system ()
  ((symbol :type string :accessor system-symbol)
   (sector :type string :accessor system-sector)
   (type :type symbol :accessor system-type)
   (point :type point :initarg :point :accessor system-point)
   (waypoints :type list :accessor system-waypoints)))

(defmethod print-object ((system system) stream)
  (print-unreadable-object (system stream :type t)
    (with-slots (symbol type point) system
      (format stream "~A ~A " symbol type)
      (serialize-point point :stream stream))))

(defmethod update-from-api-data ((system system) data)
  (let (x y)
    (alist-case (value data)
      (symbol
        (setf (system-symbol system) value))
      (sector-symbol
        (setf (system-sector system) value))
      (type
        (setf (system-type system) value))
      (x
        (setf x value))
      (y
        (setf y value))
      (waypoints
        (setf (system-waypoints system)
              (create-from-api-data 'waypoint value))))
    (setf (system-point system) (point x y))
    system))

;;;
;;; Agents
;;;

(defclass agent ()
  ((id :type string :accessor agent-id)
   (symbol :type string :accessor agent-symbol)
   (headquarters :type string :accessor agent-headquarters)
   (credits :type integer :accessor agent-credits)
   (starting-faction :type string :accessor agent-starting-faction)))

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
      (setf (agent-credits agent) value))
    (starting-faction
      (setf (agent-starting-faction agent) value))))

;;;
;;; Factions
;;;

(defclass faction ()
  ((symbol :type string :accessor faction-symbol)
   (name :type string :accessor faction-name)
   (description :type string :accessor faction-description)
   (headquarters :type string :accessor faction-headquarters)
   (traits :type list :accessor faction-traits)
   (recruiting :type boolean :accessor faction-recruiting)))

(defmethod print-object ((faction faction) stream)
  (print-unreadable-object (faction stream :type t)
    (with-slots (symbol) faction
      (format stream "~A" symbol))))

(defmethod update-from-api-data ((faction faction) data)
  (alist-case (value data faction)
    (symbol
      (setf (faction-symbol faction) value))
    (name
      (setf (faction-name faction) value))
    (description
      (setf (faction-description faction) value))
    (headquarters
      (setf (faction-headquarters faction) value))
    (traits
      (setf (faction-traits faction)
            (map 'list (lambda (data)
                         (alist-getf 'symbol data))
                 value)))
    (is-recruiting
      (setf (faction-recruiting faction) value))))

;;;
;;; Contracts
;;;

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
    (deadline-to-accept
      (setf (contract-expiration-time contract)
            (time:parse-rfc3339-datetime value)))))

;;;
;;; Routes
;;;

(defclass route ()
  ((departure :type string :accessor route-departure)
   (destination :type string :accessor route-destination)
   (departure-time :type time:datetime :accessor route-departure-time)
   (arrival-time :type time:datetime :accessor route-arrival-time)))

(defmethod print-object ((route route) stream)
  (print-unreadable-object (route stream :type t)
    (with-slots (departure destination) route
      (format stream "~A - ~A" departure destination))))

(defmethod update-from-api-data ((route route) data)
  (alist-case (value data route)
    (departure
      (setf (route-departure route) (alist-getf 'symbol value)))
    (destination
      (setf (route-destination route) (alist-getf 'symbol value)))
    (departure-time
      (setf (route-departure-time route)
            (time:parse-rfc3339-datetime value)))
    (arrival
      (setf (route-arrival-time route)
            (time:parse-rfc3339-datetime value)))))

;;;
;;; Ships
;;;

(defclass ship ()
  ((symbol :type string :accessor ship-symbol)
   (faction :type string :accessor ship-faction)
   (role :type symbol :accessor ship-role)
   (system :type string :accessor ship-system)
   (waypoint :type string :accessor ship-waypoint)
   (route :type route :accessor ship-route)
   (navigation-status :type symbol :accessor ship-navigation-status)
   (flight-mode :type symbol :accessor ship-flight-mode)
   (cargo-units :type integer :accessor ship-cargo-units)
   (cargo-capacity :type integer :accessor ship-cargo-capacity)
   (cargo :type list :accessor ship-cargo)
   (fuel :type integer :accessor ship-fuel)
   (fuel-capacity :type integer :accessor ship-fuel-capacity)
   (last-fuel-consumption-amount
    :type integer :accessor ship-last-fuel-consumption-amount)
   (last-fuel-consumption-time
    :type time:datetime :accessor ship-last-fuel-consumption-time)))

(defmethod print-object ((ship ship) stream)
  (print-unreadable-object (ship stream :type t)
    (with-slots (symbol) ship
      (format stream "~A" symbol))))

(defmethod update-from-api-data ((ship ship) data)
  ;; TODO crew, frame, reactor, engine, modules, mounts.
  (alist-case (value data ship)
    (symbol
      (setf (ship-symbol ship) value))
    (registration
      (alist-case (value value)
        (faction-symbol
          (setf (ship-faction ship) value))
        (role
          (setf (ship-role ship) value))))
    (nav
      (alist-case (value value)
        (system-symbol
          (setf (ship-system ship) value))
        (waypoint-symbol
          (setf (ship-waypoint ship) value))
        (route
          (setf (ship-route ship)
                (create-from-api-data 'route value)))
        (status
          (setf (ship-navigation-status ship) value))
        (flight-mode
          (setf (ship-flight-mode ship) value))))
    (cargo
      (update-ship-from-cargo-api-data ship value))
    (fuel
      (alist-case (value value)
        (current
         (setf (ship-fuel ship) value))
        (capacity
         (setf (ship-fuel-capacity ship) value))
        (consumed
          (alist-case (value value)
            (amount
              (setf (ship-last-fuel-consumption-amount ship) value))
            (timestamp
              (setf (ship-last-fuel-consumption-time ship)
                    (time:parse-rfc3339-datetime value)))))))))

(defun update-ship-from-cargo-api-data (ship data)
  (declare (type ship ship))
  (alist-case (value data)
    (units
      (setf (ship-cargo-units ship) value))
    (capacity
      (setf (ship-cargo-capacity ship) value))
    (inventory
      (setf (ship-cargo ship)
            (map 'list (lambda (data)
                         (cons (alist-getf 'symbol data)
                               (alist-getf 'units data)))
                 value)))))

;;;
;;; Shipyards
;;;

(defclass shipyard-ship ()
  ((type :type symbol :accessor shipyard-ship-type)
   (name :type string :accessor shipyard-ship-name)
   (description :type string :accessor shipyard-ship-description)
   (price :type integer :accessor shipyard-ship-price)))

(defmethod print-object ((ship shipyard-ship) stream)
  (print-unreadable-object (ship stream :type t)
    (with-slots (symbol price) ship
      (format stream "~A ~D" symbol price))))

(defmethod update-from-api-data ((ship shipyard-ship) data)
  ;; TODO frame, reactor, engine, modules, mounts
  (alist-case (value data)
    (type
      (setf (shipyard-ship-type ship) value))
    (name
      (setf (shipyard-ship-name ship) value))
    (description
      (setf (shipyard-ship-description ship) value))
    (price
      (setf (shipyard-ship-price ship) value))))

(defclass shipyard-transaction ()
  ((waypoint :type string :accessor shipyard-transaction-waypoint)
   (ship :type string :accessor shipyard-transaction-ship)
   (price :type string :accessor shipyard-transaction-price)
   (agent :type string :accessor shipyard-transaction-agent)
   (time :type time:datetime :accessor shipyard-transaction-time)))

(defmethod print-object ((transaction shipyard-transaction) stream)
  (print-unreadable-object (transaction stream :type t)
    (with-slots (time price) transaction
      (format stream "~A ~D" time price))))

(defmethod update-from-api-data ((transaction shipyard-transaction) data)
  (alist-case (value data)
    (waypoint-symbol
      (setf (shipyard-transaction-waypoint transaction) value))
    (ship-symbol
      (setf (shipyard-transaction-ship transaction) value))
    (price
      (setf (shipyard-transaction-price transaction) value))
    (agent-symbol
      (setf (shipyard-transaction-agent transaction) value))
   (timestamp
      (setf (shipyard-transaction-time transaction)
            (time:parse-rfc3339-datetime value)))))

(defclass shipyard ()
  ((symbol :type string :accessor shipyard-symbol)
   (ship-types :type list :accessor shipyard-ship-types)
   (transactions :type list :accessor shipyard-transactions)
   (ships :type list :accessor shipyard-ships)))

(defmethod print-object ((shipyard shipyard) stream)
  (print-unreadable-object (shipyard stream :type t)
    (with-slots (symbol) shipyard
      (format stream "~A" symbol))))

(defmethod update-from-api-data ((shipyard shipyard) data)
  (alist-case (value data)
    (symbol
      (setf (shipyard-symbol shipyard) value))
    (ship-types
      (setf (shipyard-ship-types shipyard)
            (map 'list (lambda (data)
                         (alist-getf 'type data))
                 value)))
    (transactions
      (setf (shipyard-symbol shipyard)
            (create-from-api-data 'shipyard-transaction value)))
    (ships
      (setf (shipyard-symbol shipyard)
            (create-from-api-data 'shipyard-ship value)))))
