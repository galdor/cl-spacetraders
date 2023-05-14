(in-package :spacetraders)

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
