;;;; foo-lisp.asd

(asdf:defsystem #:foo-lisp
  :description "Describe foo-lisp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:drakma #:cl-csv #:yason #:cl-ana #:inferior-shell #:xelf)
  :components ((:file "package")
               (:file "graph-util")
               (:file "wumpus")
               (:file "foo-lisp")))
