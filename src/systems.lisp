(in-package :spacetraders)

(defclass system ()
  ((symbol :type string :accessor system-symbol)
   (sector-symbol :type string :accessor system-sector-symbol)
   (type :type symbol :accessor system-type)
   (point :type point :initarg :point :accessor system-point)
   (waypoints :type list :accessor system-waypoints)))

(defmethod print-object ((system system) stream)
  (print-unreadable-object (system stream :type t)
    (with-slots (symbol type point) system
      (format stream "~A ~A " symbol type)
      (serialize-point point :stream stream))))

(defun build-system (data)
  (let ((system (make-instance 'system :point (data-point data))))
    (dolist (entry data system)
      (case (car entry)
        (symbol
         (setf (system-symbol system) (cdr entry)))
        (sector-symbol
         (setf (system-sector-symbol system) (cdr entry)))
        (type
         (setf (system-type system) (cdr entry)))
        (waypoints
         (setf (system-waypoints system)
               (map 'list 'build-waypoint (cdr entry))))))))
