(in-package :spacetraders)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *systems-uri*
    (uri:parse "https://api.spacetraders.io/v2/systems.json"))

  (defparameter *systems-path*
    (asdf:system-relative-pathname "spacetraders" "data/systems.json"))

  (defun fetch-systems ()
    (let ((response (http:send-request :get *systems-uri*)))
      (with-open-file (stream *systems-path* :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create
                                             :element-type 'core:octet)
        (write-sequence (http:response-body response) stream))
      *systems-path*))

  (defun load-systems ()
    (unless (probe-file *systems-path*)
      (fetch-systems))
    (let ((data (system:read-file *systems-path* :external-format :utf-8)))
      (json:parse data :mapping '(:array :element system))))

  (defvar *systems* (load-systems)))
