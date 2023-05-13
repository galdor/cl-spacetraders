(in-package :spacetraders)

(defclass route ()
  ((departure :type string :accessor route-departure)
   (destination :type string :accessor route-destination)
   (departure-time :type time:datetime :accessor route-departure-time)
   (arrival-time :type time:datetime :accessor route-arrival-time)))

(defmethod print-object ((route route) stream)
  (print-unreadable-object (route stream :type t)
    (with-slots (departure destination) route
      (format stream "~A - ~A" departure destination))))

(defun build-route (data)
  (let ((route (make-instance 'route)))
    (dolist (entry data route)
      (case (car entry)
        (departure
         (setf (route-departure route) (cdr (assoc 'symbol (cdr entry)))))
        (destination
         (setf (route-destination route) (cdr (assoc 'symbol (cdr entry)))))
        (departure-time
         (setf (route-departure-time route)
               (time:parse-rfc3339-datetime (cdr entry))))
        (arrival
         (setf (route-arrival-time route)
               (time:parse-rfc3339-datetime (cdr entry))))))))
