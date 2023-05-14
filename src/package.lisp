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

   #:load-systems
   #:find-system

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

   #:unknown-faction
   #:unknown-faction-symbol
   #:unknown-contract
   #:unknown-contract-id
   #:register
   #:fetch-agent
   #:fetch-faction
   #:fetch-factions
   #:fetch-ships
   #:fetch-contracts
   #:accept-contract
   #:fulfill-contract))
