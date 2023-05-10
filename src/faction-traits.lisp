(in-package :spacetraders)

(defclass faction-trait ()
  ((symbol
    :type symbol
    :initarg :symbol
    :accessor faction-trait-symbol)
   (name
    :type string
    :initarg :name
    :accessor faction-trait-name)
   (description
    :type string
    :initarg :description
    :accessor faction-trait-description)))

(defmethod print-object ((trait faction-trait) stream)
  (print-unreadable-object (trait stream :type t)
    (with-slots (symbol) trait
      (format stream "~A" symbol))))

(defun build-faction-trait (data)
  (let ((trait (make-instance 'faction-trait)))
    (dolist (entry data trait)
      (case (car entry)
        (symbol
         (setf (faction-trait-symbol trait) (cdr entry)))
        (name
         (setf (faction-trait-name trait) (cdr entry)))
        (description
         (setf (faction-trait-description trait) (cdr entry)))))))
