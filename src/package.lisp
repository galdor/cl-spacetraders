(defpackage :spacetraders
  (:use :cl)
  (:export
   #:*default-api-uri*
   #:*mock-api-uri*
   #:*api-uri*
   #:*authentication-token*
   #:http-error
   #:http-error-response
   #:api-error
   #:api-error-message
   #:api-error-code
   #:api-error-data
   #:rate-limit-reached
   #:rate-limit-reached-delay

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

   #:agent
   #:agent-id
   #:agent-symbol
   #:agent-headquarters
   #:agent-credits

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

   #:unknown-faction
   #:unknown-faction-symbol
   #:register
   #:fetch-agent
   #:fetch-faction
   #:find-faction))
