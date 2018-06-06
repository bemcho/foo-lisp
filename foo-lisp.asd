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
                         (:file "util")  
                         (:file "config")
                         (:file "grid-utils")
                         (:file "draw")  
                         (:file "graph-util")
                         (:file "wumpus")
                         (:file "foo-lisp")
                         (:module "utilities"
                                  :components
                                  ((:file "utilities")
                                   (:file "test-utilities")
                                   (:file "queue")
                                   (:file "binary-tree")
                                   (:file "index")
                                   ))
                         (:module "agents"
                                  :components
                                  ((:module "agents"
                                            :components
                                            ((:file "agent")
                                             (:file "vacuum")
                                             (:file "wumpus" )))
                                   (:module "algorithms"
                                            :components
                                            ((:file "grid")))
                                   (:module "environments"
                                            :components
                                            ((:file "basic-env")
                                             (:file "grid-env":depends-on ("basic-env"))
                                             (:file "vacuum":depends-on ("basic-env"))
                                             (:file "wumpus":depends-on ("basic-env"))))
                                   (:file "test-agents")))))
               (:module "static")
               (:module "generated")))
