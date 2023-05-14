(in-package :spacetraders)

(defun waypoint-symbol-system-symbol (waypoint-symbol)
  (declare (type string waypoint-symbol))
  (let ((dash (position #\- waypoint-symbol :from-end t)))
    (subseq waypoint-symbol 0 dash)))

(defun system (symbol)
  (declare (type string symbol))
  (with-slots (systems) *client*
    (or (gethash symbol systems)
        (error 'unknown-system :symbol symbol))))

(defun waypoint (symbol)
  (declare (type string symbol))
  (with-slots (systems) *client*
    (let* ((system-symbol (waypoint-symbol-system-symbol symbol))
           (system (system system-symbol)))
      (or (find symbol (system-waypoints system)
                :key 'waypoint-symbol :test #'string=)
          (error 'unknown-waypoint :symbol symbol)))))

(defun fetch-waypoint-shipyard (waypoint)
  (declare (type waypoint waypoint))
  (let ((data (call-api/get-shipyard (waypoint-symbol waypoint))))
    (create-from-api-data 'shipyard data)))
