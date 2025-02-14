(asdf:defsystem :cantedraw
  :components ((:file "packages")
               (:file "lib.macros")
               (:file "lib.functions")
               (:file "main"))
  :build-operation "program-op"
  :build-pathname "bin/cantedraw"
  :entry-point "main:start")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression 9))
