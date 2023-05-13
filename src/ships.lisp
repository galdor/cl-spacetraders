(in-package :spacetraders)

(defclass ship ()
  ((symbol :type string :accessor ship-symbol)
   (faction :type string :accessor ship-faction)
   (role :type symbol :accessor ship-role)
   (system :type string :accessor ship-system)
   (waypoint :type string :accessor ship-waypoint)
   (route :type route :accessor ship-route)
   (navigation-status :type symbol :accessor ship-navigation-status)
   (flight-mode :type symbol :accessor ship-flight-mode)))

(defmethod print-object ((ship ship) stream)
  (print-unreadable-object (ship stream :type t)
    (with-slots (symbol) ship
      (format stream "~A" symbol))))

(defun build-ship (data)
  (let ((ship (make-instance 'ship)))
    (dolist (entry data ship)
      ;; TODO crew, frame, reactor, engine, modules, mounts, cargo, fuel.
      (case (car entry)
        (symbol
         (setf (ship-symbol ship) (cdr entry)))
        (registration
         (build-ship/registration (cdr entry) ship))
        (nav
         (build-ship/nav (cdr entry) ship))))))

(defun build-ship/registration (data ship)
  (declare (type ship ship))
  (dolist (entry data)
    (case (car entry)
      (faction-symbol
       (setf (ship-faction ship) (cdr entry)))
      (role
       (setf (ship-role ship) (cdr entry))))))

(defun build-ship/nav (data ship)
  (declare (type ship ship))
  (dolist (entry data)
    (case (car entry)
      (system-symbol
       (setf (ship-system ship) (cdr entry)))
      (waypoint-symbol
       (setf (ship-waypoint ship) (cdr entry)))
      (route
       (setf (ship-route ship) (build-route (cdr entry))))
      (status
       (setf (ship-navigation-status ship) (cdr entry)))
      (flight-mode
       (setf (ship-flight-mode ship) (cdr entry))))))
