(defpackage #:grid-utils
  (:use #:cl)
  (:export
   #:config
   #:node-pos-to-node
   #:set-node-mapping
   #:get-wumpus-hunter-node
   #:move-node-to
   #:get-nth-grid-box-coord
   #:get-next-x
   #:get-next-y))

(in-package #:grid-utils)

(defparameter *width* 1800)
(defparameter *height* 950)
(defparameter *city-node-size* 80 "in pixels")
(defparameter *space-btw-nodes* 40 "in pixels")
(defparameter *padding-inside-node* 15 "in pixels")
(defparameter *objects-size* 20 "in pixels")
(defparameter *visited-nodes* '()  "reference to list of agent's visited nodes as fist is current node")


(defparameter *node-offset* (+ *city-node-size* *space-btw-nodes*) "in pixels")
(defparameter *x-max-objects* (floor *width* *space-btw-nodes*) "optimal nodes count per row")
(defparameter *y-max-objects* (floor *height* *city-node-size*) "optimal nodes count per col")
(defparameter *half-city-node* (floor *city-node-size* 2)  "in pixels")

(defparameter *grid-max-row-cols* 10)
(defparameter *grid-width*  100 "in pixels")

(defparameter *grid-height*  100 "in pixels")


(defun config(width height node-size space-btw-nodes padding-inside-node object-size grid-max-row-cols ref-to-visited-nodes)
  "Call this before using grid if you want to change default config values"
  (setf *width*  width *height* height *city-node-size* node-size *space-btw-nodes* space-btw-nodes  *padding-inside-node*  padding-inside-node *objects-size*
          object-size)
  (setf  *visited-nodes*  ref-to-visited-nodes  *grid-max-row-cols* grid-max-row-cols)

  (setf *grid-width*  (floor *width* *grid-max-row-cols*))
  (setf *grid-height* (floor *height* *grid-max-row-cols*)))

(defparameter *node-to-coordinates-map* (make-hash-table :test 'equal))

(defun  node-pos-to-node(pos)
  "doc"
  (multiple-value-bind (node found)
      (gethash pos *node-to-coordinates-map*)
    node))

(defun set-node-mapping (pos node)
  "doc"
  (setf (gethash pos *node-to-coordinates-map*) node))

(defun get-wumpus-hunter-node ()
  "doc"
  (node-pos-to-node (car *visited-nodes*)))

(defun move-node-to (node node-pos)
  "doc"
  (let ((wumpus-node (node-pos-to-node node-pos)))
    (xelf:move-to node
                  (+ (xelf:x wumpus-node) *half-city-node*)
                  (+ (xelf:y wumpus-node) *half-city-node*))))
;; 2D grid utils


(defun get-nth-grid-box-coord (grid-pos)
  "doc"
  (multiple-value-bind (y-step x-step)
      (floor (- grid-pos 1) *grid-max-row-cols*)
    (values 
     (+ (* *grid-width* x-step) 0 *space-btw-nodes*)
     (+ (* *grid-height* y-step)  *space-btw-nodes*))))

(defun get-next-x (x step)
  "doc"
  (cond  ((or (= step 0) (= step 2)) x)
         ((or (=  step 1) (= step 3)) (+ (- *city-node-size* (* *padding-inside-node* 2) *objects-size*) x))))

(defun get-next-y (y step)
  "doc"
  (cond  ((or (= step 0) (= step 1)) y)
         ((or (=  step 2) (= step 3)) (+ (- *city-node-size* (* *padding-inside-node* 2) *objects-size*) y))))