;;;; foo-lisp.asd
(asdf:defsystem #:foo-lisp
  :description "Land of lisp grand theft wumpus visualization with xelf"
  :author "Emil Tomov"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:xelf #:inferior-shell)
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "grid-utils")
                         (:file "draw")  
                         (:file "graph-util")
                         (:file "wumpus")
                         (:file "foo-lisp")))
               (:module "static")
               (:module "generated")))
