(in-package :spacetraders)

(defun find-system (symbol)
  (declare (type string symbol))
  (with-slots (systems) *client*
    (or (gethash symbol systems)
        (error 'unknown-system :symbol symbol))))
