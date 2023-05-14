(in-package :spacetraders)

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
