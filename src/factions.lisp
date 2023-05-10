(in-package :spacetraders)

(defclass faction ()
  ((symbol
    :type string
    :initarg :symbol
    :accessor faction-symbol)
   (name
    :type string
    :initarg :name
    :accessor faction-name)
   (description
    :type string
    :initarg :description
    :accessor faction-description)
   (headquarters
    :type string
    :initarg :headquarters
    :accessor faction-headquarters)
   (traits
    :type list
    :initarg :traits
    :initform nil
    :accessor faction-traits)))

(defmethod print-object ((faction faction) stream)
  (print-unreadable-object (faction stream :type t)
    (with-slots (symbol) faction
      (format stream "~A" symbol))))

(defun build-faction (data)
  (let ((faction (make-instance 'faction)))
    (dolist (entry data faction)
      (case (car entry)
        (symbol
         (setf (faction-symbol faction) (cdr entry)))
        (name
         (setf (faction-name faction) (cdr entry)))
        (description
         (setf (faction-description faction) (cdr entry)))
        (headquarters
         (setf (faction-headquarters faction) (cdr entry)))
        (traits
         (setf (faction-traits faction)
               (map 'list 'build-faction-trait (cdr entry))))))))
