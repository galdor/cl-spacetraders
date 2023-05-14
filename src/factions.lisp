(in-package :spacetraders)

(defun list-factions ()
  (core:hash-table-values (client-factions *client*)))

(defun faction (symbol)
  (declare (type string symbol))
  (or (gethash symbol (client-factions *client*))
      (error 'unknown-faction :symbol symbol)))
