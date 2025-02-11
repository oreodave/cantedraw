(asdf:defsystem :odraw
  :depends-on (:deploy)
  :components ((:file "packages")
               (:file "lib.macros")
               (:file "lib.functions")
               (:file "odraw"))
  :build-operation "deploy-op"
  :build-pathname "odraw"
  :entry-point "odraw:start")
