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
