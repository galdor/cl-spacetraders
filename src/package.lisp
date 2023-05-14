(defpackage :spacetraders
  (:use :cl)
  (:export
   #:alist-getf
   #:alist-case

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
   #:system-sector
   #:system-type
   #:system-point
   #:system-waypoints

   #:agent
   #:agent-id
   #:agent-symbol
   #:agent-headquarters
   #:agent-credits

   #:faction
   #:faction-symbol
   #:faction-name
   #:faction-description
   #:faction-headquarters
   #:faction-traits

   #:contract-good
   #:contract
   #:contract-id
   #:contract-faction
   #:contract-type
   #:contract-goods
   #:contract-accepted
   #:contract-fulfilled
   #:contract-expiration-time
   #:contract-deadline
   #:contract-initial-payment
   #:contract-final-payment

   #:ship
   #:ship-symbol
   #:ship-faction
   #:ship-role
   #:ship-system
   #:ship-waypoint
   #:ship-route
   #:ship-navigation-status
   #:ship-flight-mode
   #:ship-cargo-units
   #:ship-cargo-capacity
   #:ship-cargo
   #:ship-fuel
   #:ship-fuel-capacity
   #:ship-last-fuel-consumption-amount
   #:ship-last-fuel-consumption-time

   #:unknown-system
   #:unknown-system-symbol
   #:unknown-faction
   #:unknown-faction-symbol
   #:unknown-ship
   #:unknown-ship-symbol
   #:unknown-contract
   #:unknown-contract-id

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
   #:register

   #:fetchs-systems
   #:load-systems

   #:*client*
   #:make-client
   #:initialize-client
   #:find-system
   #:agent
   #:list-factions
   #:faction
   #:list-ships
   #:ship
   #:do-ships
   #:list-contracts
   #:contract
   #:accept-contract
   #:deliver-contract
   #:fulfill-contract))
