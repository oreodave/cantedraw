(asdf:defsystem :odraw
  :depends-on (:deploy)
  :components ((:file "packages")
               (:file "lib.macros")
               (:file "main"))
  :build-operation "deploy-op"
  :build-pathname "odraw"
  :entry-point "main:start")
