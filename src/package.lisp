(defpackage :spacetraders
  (:use :cl)
  (:export
   #:*default-api-uri*
   #:*mock-api-uri*
   #:*api-uri*
   #:*authentication-token*

   #:point
   #:point-x
   #:point-y
   #:serialize-point

   #:waypoint
   #:waypoint-symbol
   #:waypoint-type
   #:waypoint-point

   #:system
   #:system-symbol
   #:system-sector-symbol
   #:system-type
   #:system-point
   #:system-waypoints

   #:load-systems
   #:find-system

   #:faction-trait
   #:faction-trait-symbol
   #:faction-trait-name
   #:faction-trait-description

   #:faction
   #:faction-symbol
   #:faction-name
   #:faction-description
   #:faction-headquarters
   #:faction-traits

   #:register
   #:find-faction))
