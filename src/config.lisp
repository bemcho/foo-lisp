(in-package #:foo-lisp)
(defparameter *unit* 10)
(defun units (n) (* *unit* n))

(defparameter *width* 1800)
(defparameter *height* 900)
(defparameter *city-node-size* (units 15))
(defparameter *space-btw-nodes* (units 4))
(defparameter *padding-inside-node* (units 3))
(defparameter *objects-size* (units 4) "in pixels")

(defparameter *score-font* "sans-mono-bold-12")
(defparameter *big-font* "sans-mono-bold-16")








