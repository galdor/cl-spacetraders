(in-package :spacetraders)

(defstruct (point
            (:constructor nil))
  (x 0 :type integer)
  (y 0 :type integer))

(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream :type t)
    (with-slots (x y) point
      (serialize-point point :stream stream))))

(defun point (x y)
  (declare (type integer x y))
  (let ((point (make-instance 'point)))
    (setf (slot-value point 'x) x)
    (setf (slot-value point 'y) y)
    point))

(defun serialize-point (point &key stream)
  (declare (type point point))
  (with-slots (x y) point
    (format stream "~D,~D" x y)))

(defun data-point (data)
  (point (cdr (assoc 'x data))
         (cdr (assoc 'y data))))
