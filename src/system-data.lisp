(in-package :spacetraders)

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
  (let* ((string (system:read-file *systems-path* :external-format :utf-8))
         (data (json:parse string :mapping '(:array :element system)))
         (systems (make-hash-table :test #'equal)))
    (do ((i 0 (1+ i)))
        ((>= i (length data))
         systems)
      (let* ((system-data (aref data i))
             (system (create-from-api-data 'system system-data)))
        (setf (gethash (system-symbol system) systems) system)))))
