(in-package :spacetraders)

(defclass cargo-item ()
  ((symbol :type string :accessor cargo-item-symbol)
   (name :type string :accessor cargo-item-name)
   (description :type string :accessor cargo-item-description)
   (units :type integer :accessor cargo-item-units)))

(defmethod print-object ((item cargo-item) stream)
  (print-unreadable-object (item stream :type t)
    (with-slots (symbol) item
      (format stream "~A" symbol))))

(defclass ship-cargo ()
  ((units :type integer :accessor ship-cargo-units)
   (capacity :type integer :accessor ship-cargo-capacity)
   (inventory :type list :accessor ship-cargo-inventory)))

(defmethod print-object ((cargo ship-cargo) stream)
  (print-unreadable-object (cargo stream :type t)
    (with-slots (units capacity) cargo
      (format stream "~D/~D" units capacity))))

(defun build-ship-cargo (data)
  (let ((cargo (make-instance 'ship-cargo)))
    (dolist (entry data cargo)
      (case (car entry)
        (units
         (setf (ship-cargo-units cargo) (cdr entry)))
        (capacity
         (setf (ship-cargo-capacity cargo) (cdr entry)))
        (inventory
         (setf (ship-cargo-inventory cargo)
               (map 'list 'build-cargo-item (cdr entry))))))))

(defun build-cargo-item (data)
  (let ((item (make-instance 'cargo-item)))
    (dolist (entry data item)
      (case (car entry)
        (symbol
         (setf (cargo-item-symbol item) (cdr entry)))
        (name
         (setf (cargo-item-name item) (cdr entry)))
        (description
         (setf (cargo-item-description item) (cdr entry)))
        (units
         (setf (cargo-item-units item) (cdr entry)))))))

(defclass ship ()
  ((symbol :type string :accessor ship-symbol)
   (faction :type string :accessor ship-faction)
   (role :type symbol :accessor ship-role)
   (system :type string :accessor ship-system)
   (waypoint :type string :accessor ship-waypoint)
   (route :type route :accessor ship-route)
   (navigation-status :type symbol :accessor ship-navigation-status)
   (flight-mode :type symbol :accessor ship-flight-mode)
   (cargo :type ship-cargo :accessor ship-cargo)
   (fuel :type integer :accessor ship-fuel)
   (fuel-capacity :type integer :accessor ship-fuel-capacity)))

(defmethod print-object ((ship ship) stream)
  (print-unreadable-object (ship stream :type t)
    (with-slots (symbol) ship
      (format stream "~A" symbol))))

(defun build-ship (data)
  (let ((ship (make-instance 'ship)))
    (dolist (entry data ship)
      ;; TODO crew, frame, reactor, engine, modules, mounts.
      (case (car entry)
        (symbol
         (setf (ship-symbol ship) (cdr entry)))
        (registration
         (build-ship/registration (cdr entry) ship))
        (nav
         (build-ship/nav (cdr entry) ship))
        (cargo
         (setf (ship-cargo ship) (build-ship-cargo (cdr entry))))
        (fuel
         (build-ship/fuel (cdr entry) ship))))))

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

(defun build-ship/fuel (data ship)
  (declare (type ship ship))
  (dolist (entry data)
    ;; TODO consumed
    (case (car entry)
      (current
       (setf (ship-fuel ship) (cdr entry)))
      (capacity
       (setf (ship-fuel-capacity ship) (cdr entry))))))
