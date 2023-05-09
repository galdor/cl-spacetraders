(defsystem "spacetraders"
  :description "A client for the SpaceTraders online game API."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-uri"
   "tungsten-netrc"
   "tungsten-http")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "spacetraders")))
