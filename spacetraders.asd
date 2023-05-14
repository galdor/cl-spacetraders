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
   (:file "api")
   (:file "system-data")
   (:file "client")
   (:file "systems")
   (:file "agents")
   (:file "factions")
   (:file "contracts")
   (:file "ships")
   (:file "conditions")))
