(in-package :spacetraders)

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
          (setf (ship-route ship) (build-route value)))
        (status
          (setf (ship-navigation-status ship) value))
        (flight-mode
          (setf (ship-flight-mode ship) value))))
    (cargo
      (alist-case (value value)
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
