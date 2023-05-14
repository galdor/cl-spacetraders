(in-package :spacetraders)

(define-condition unknown-system (error)
  ((symbol
    :type string
    :initarg :symbol
    :reader unknown-system-symbol))
  (:report
   (lambda (condition stream)
     (with-slots (symbol) condition
       (format stream "Unknown system ~S." symbol)))))

(define-condition unknown-faction (error)
  ((symbol
    :type string
    :initarg :symbol
    :reader unknown-faction-symbol))
  (:report
   (lambda (condition stream)
     (with-slots (symbol) condition
       (format stream "Unknown faction ~S." symbol)))))

(define-condition unknown-ship (error)
  ((symbol
    :type string
    :initarg :symbol
    :reader unknown-ship-symbol))
  (:report
   (lambda (condition stream)
     (with-slots (symbol) condition
       (format stream "Unknown ship ~S." symbol)))))

(define-condition unknown-contract (error)
  ((id
    :type string
    :initarg :id
    :reader unknown-contract-symbol))
  (:report
   (lambda (condition stream)
     (with-slots (id) condition
       (format stream "Unknown contract ~S." id)))))
