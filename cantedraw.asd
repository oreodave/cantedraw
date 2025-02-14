(asdf:defsystem :cantedraw
  :depends-on (:alexandria)
  :components ((:file "packages")
               (:module "lib"
                :components
                ((:file "macros")
                 (:file "functions")))
               (:module "src"
                :components
                ((:file "model")
                 (:file "main"))))
  :build-operation "program-op"
  :build-pathname "bin/cantedraw"
  :entry-point "cantedraw.main:start")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression 9))
