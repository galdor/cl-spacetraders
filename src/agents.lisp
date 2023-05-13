(in-package :spacetraders)

(defclass agent ()
  ((id :type string :accessor agent-id)
   (symbol :type string :accessor agent-symbol)
   (headquarters :type string :accessor agent-headquarters)
   (credits :type integer :accessor agent-credits)))

(defmethod print-object ((agent agent) stream)
  (print-unreadable-object (agent stream :type t)
    (with-slots (symbol) agent
      (format stream "~A" symbol))))

(defun build-agent (data)
  (let ((agent (make-instance 'agent)))
    (dolist (entry data agent)
      (case (car entry)
        (account-id
         (setf (agent-id agent) (cdr entry)))
        (symbol
         (setf (agent-symbol agent) (cdr entry)))
        (headquarters
         (setf (agent-headquarters agent) (cdr entry)))
        (credits
         (setf (agent-credits agent) (cdr entry)))))))
