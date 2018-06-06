;;;; package.lisp

(defpackage #:foo-lisp
  (:use #:cl #:xelf #:inferior-shell)
  (:export #:say #:ugraph->png #:new-game #:wumpus-game #:agent))

(defpackage   #:aima
  (:use       #:cl )
  (:export #:test)
  (:documentation "doc"))











