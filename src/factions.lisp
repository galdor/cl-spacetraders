(in-package :spacetraders)

(defclass faction ()
  ((symbol :type string :accessor faction-symbol)
   (name :type string :accessor faction-name)
   (description :type string :accessor faction-description)
   (headquarters :type string :accessor faction-headquarters)
   (traits :type list :accessor faction-traits)))

(defmethod print-object ((faction faction) stream)
  (print-unreadable-object (faction stream :type t)
    (with-slots (symbol) faction
      (format stream "~A" symbol))))

(defmethod update-from-api-data ((faction faction) data)
  (alist-case (value data faction)
    (symbol
      (setf (faction-symbol faction) value))
    (name
      (setf (faction-name faction) value))
    (description
      (setf (faction-description faction) value))
    (headquarters
      (setf (faction-headquarters faction) value))
    (traits
      (setf (faction-traits faction)
            (map 'list (lambda (data)
                         (alist-getf 'symbol data))
                 value)))))
