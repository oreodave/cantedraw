(ql:quickload :asdf)
(ql:quickload :deploy)
(asdf:load-asd (merge-pathnames "odraw.asd" (uiop:getcwd)))
(asdf:load-system :odraw)
