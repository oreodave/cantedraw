(asdf:defsystem "cantedraw"
  :author "Aryadev Chavali <aryadev@aryadevchavali.com>"
  :license "GPL-2"
  :depends-on (:alexandria)
  :in-order-to ((asdf:test-op (asdf:test-op :cantedraw/tests)))
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

(asdf:defsystem "cantedraw/tests"
  :author "Aryadev Chavali <aryadev@aryadevchavali.com>"
  :license "GPL-2"
  :depends-on (:cantedraw
               :parachute)
  :components ((:module "tests"
                :components
                ((:file "macros")
                 (:file "functions")
                 (:file "model")
                 ;; (:file "player")
                 ;; (:file "game")
                 (:file "main"))))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test
                                             :cantedraw/tests/main)))

;; Compress image for smaller binary size.
#+nil
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression 9))
