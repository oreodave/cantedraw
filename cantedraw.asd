(asdf:defsystem "cantedraw"
  :author "Aryadev Chavali <aryadev@aryadevchavali.com>"
  :license "GPL-2"
  :depends-on (:alexandria)
  :components ((:file "packages")
               (:module "lib"
                :components
                ((:file "macros")
                 (:file "functions")))
               (:module "src"
                :components
                ((:file "model")
                 (:file "player")
                 (:file "game")
                 (:file "main"))))
  :build-operation "program-op"
  :build-pathname "bin/cantedraw"
  :entry-point "cantedraw.main:start")

;; Compress image for smaller binary size.
#+nil
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression 9))
