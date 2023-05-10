(defpackage :spacetraders
  (:use :cl)
  (:export
   #:*default-api-uri*
   #:*mock-api-uri*
   #:*api-uri*
   #:*authentication-token*

   #:register
   #:find-faction))
