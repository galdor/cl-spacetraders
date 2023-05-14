(defsystem "spacetraders"
  :description "A client for the SpaceTraders online game API."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-system"
   "tungsten-uri"
   "tungsten-netrc"
   "tungsten-openapi"
   "tungsten-http")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "alists")
   (:file "objects")
   (:file "points")
   (:file "waypoints")
   (:file "systems")
   (:file "system-data")
   (:file "agents")
   (:file "factions")
   (:file "routes")
   (:file "contracts")
   (:file "ships")
   (:file "conditions")
   (:file "api")
   (:file "client")))
