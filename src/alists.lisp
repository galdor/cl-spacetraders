(in-package :spacetraders)

(defun alist-getf (name alist &optional default)
  (let ((pair (assoc name alist)))
    (if pair
        (cdr pair)
        default)))

(defun (setf alist-getf) (value alist name)
  (let ((pair (assoc name alist)))
    (if pair
        (rplacd pair value)
        (push (cons name value) alist))))

(defmacro alist-case ((value alist &optional result) &rest forms)
  (let ((alist-var (gensym "ALIST-"))
        (pair (gensym "PAIR-")))
    `(let ((,alist-var ,alist))
       (dolist (,pair ,alist-var ,result)
         (case (car ,pair)
           ,@(mapcar
              (lambda (form)
                `(,(car form)
                  (let ((,value (cdr ,pair)))
                    (declare (ignorable ,value))
                    ,(cadr form))))
              forms))))))
