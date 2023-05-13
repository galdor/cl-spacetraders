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

(defun build-waypoint (data)
  (let ((waypoint (make-instance 'waypoint :point (data-point data))))
    (dolist (entry data waypoint)
      (case (car entry)
        (symbol
         (setf (waypoint-symbol waypoint) (cdr entry)))
        (type
         (setf (waypoint-type waypoint) (cdr entry)))))))
