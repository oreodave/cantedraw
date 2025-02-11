(asdf:defsystem :cantedraw
  :depends-on (:deploy)
  :components ((:file "packages")
               (:file "lib.macros")
               (:file "lib.functions")
               (:file "cantedraw"))
  :build-operation "deploy-op"
  :build-pathname "cantedraw"
  :entry-point "cantedraw:start")
