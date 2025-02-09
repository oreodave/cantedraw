(asdf:defsystem :odraw
  :depends-on (:deploy)
  :components ((:file "packages")
               (:file "main"))
  :build-operation "deploy-op"
  :build-pathname "odraw"
  :entry-point "main:start")
