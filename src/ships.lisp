(in-package :spacetraders)

(defun list-ships ()
  (core:hash-table-values (client-ships *client*)))

(defun ship (symbol)
  (declare (type string symbol))
  (or (gethash symbol (client-ships *client*))
      (error 'unknown-ship :symbol symbol)))

(defmacro do-ships ((ship) &body body)
  `(maphash (lambda (symbol ship)
              (declare (ignore symbol))
              (let ((,ship ship))
                ,@body))
            (client-ships *client*)))
