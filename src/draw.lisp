;;;;;;;;;;;;;;;;;;;;; xelf objects

;; Common Lisp packages

;; Then we must define a Common Lisp "package" for our game's code to
;; inhabi.


;; Configuring your space 

;; Here we define an arbitrary measurement unit used throughout, and
;; set up some variables to hold the height and width of the game
;; world.

(in-package #:foo-lisp)

(defparameter *unit* 10)
(defun units (n) (* *unit* n))

(defparameter *width* 1800)
(defparameter *height* 950)
(defparameter *city-node-size* (units 8))
(defparameter *space-btw-nodes* (units 5))
(defparameter *padding-inside-node* 15)
(defparameter *objects-size* (units 2) "in pixels")



(defclass wumpus-hunter-sprite (node)
  ((height :initform (units 2))
   (width :initform (units 2))
   (max-dx :initform 100)
   (max-dy :initform 100)
   (max-ddy :initform 0.01)
   (image :initform "wumpus_hunter.png")
   (kick-clock :initform 0)
   (color :initform "red")
   (speed :initform 0)
   (count :initform 0)
   (last-collision :initform nil :accessor last-collision)))




;; The generic function [[file:dictionary/UPDATE.html][UPDATE]] is called on each object once during
;; each game loop.


(defmethod update ((wumpus-hunter-sprite wumpus-hunter-sprite))
  (with-slots (heading speed) wumpus-hunter-sprite
    (let ((node-pos (+ (random (length *congestion-city-nodes*)) 1)))
      (grid-utils:move-node-to wumpus-hunter-sprite node-pos))))

;; Now we need walls around the game world in order to contain the
;; ball.
;; cops
(defclass city-node (node)
  ((color :initform "gray")
   (image :initform "city-node.png")
   (node-number
    :reader node-number
    :writer (setf node-number)
    :initform 0)))

(defclass wumpus (node)
  ((color :initform "white")
   (image :initform "wumpus.png")))

(defclass blood (node)
  ((color :initform "red")
   (image :initform "blood.png")))

(defclass cops (node)
  ((color :initform "blue")
   (image :initform "cops.png")))

(defclass sirens (node)
  ((color :initform "cyan")
   (image :initform "sirens.png")))

(defclass glowworm (node)
  ((color :initform "hot pink")
   (image :initform "glow-worm.png")))

(defclass lights (node)
  ((color :initform "pink")
   (image :initform "lights.png")))

(defclass wall (node)
  ((color :initform "gray50")
   (image :initform "wall.png")))

;;
(defun make-object (x y width height object-type)
  (let*((obj (make-instance object-type)))
    (resize obj width height)
    (move-to obj x y)
    obj))

;; We want the ball to bounce off of the walls. The [[file:dictionary/COLLIDE.html][COLLIDE]] method is
;; called for every frame on all pairs of objects whose bounding boxes
;; collide during that frame.


(defmethod collide ((wumpus-hunter-sprite wumpus-hunter-sprite) (wall wall))
  (with-slots (heading speed x y) wumpus-hunter-sprite
    ;; back away from wall
    (move wumpus-hunter-sprite (opposite-heading heading) speed)
    ;; sometimes choose another direction to prevent getting stuck
    (percent-of-time 10 (incf heading (radian-angle 90)))))

;; Making noise

;; The ball should emit a retro beep when colliding with any node. We
;; use [[file:dictionary/DEFRESOURCE.html][DEFRESOURCE]] to let Xelf know about the sound file. 


(defmethod collide :after((wumpus-hunter-sprite wumpus-hunter-sprite) (node node)))

(defun  wumpus-hunter-sprite () (slot-value (current-buffer) 'wumpus-hunter-sprite))
;; Building the game-world out of objects 
;; See also [[file:dictionary/MOVE-TO.html][MOVE-TO]], [[file:dictionary/RESIZE.html][RESIZE]].
(defun get-class-for (wumpus-city-symbol)
  "Return class type eg: LIGHTS! -> lights"
  (cond ((eql wumpus-city-symbol 'LIGHTS!) 'lights)
        ((eql wumpus-city-symbol 'SIRENS!) 'sirens)
        ((eql wumpus-city-symbol 'BLOOD!) 'blood)
        ((eql wumpus-city-symbol 'WUMPUS) 'wumpus)
        ((eql wumpus-city-symbol 'GLOW-WORM) 'glowworm)
        ((eql wumpus-city-symbol 'COPS) 'cops)
        ))

(defun insert-objects (x y objects)
  "Inserts objects starting from x,y"
  (let ((counter 0))
    (dolist (obj objects)
      (let* ((xo (grid-utils:get-next-x (+ x *padding-inside-node*) counter))
             (yo (grid-utils:get-next-y (+ y *padding-inside-node*) counter)))
        (insert (make-object xo yo  *objects-size*  *objects-size* (get-class-for obj)))
        (incf counter)))))

(defun draw-city-background ()
  "doc"
  (insert (make-object 0 0  *width* *height* 'wall)))

(defun populate-city (city-nodes)
  (with-new-buffer
    (dolist (node city-nodes)
      (multiple-value-bind (grid-x grid-y)
          (grid-utils:get-nth-grid-box-coord (car node))
        (let*((node-pos (car node))
              (objects (rest node))
              (x grid-x)
              (y grid-y)
              (current-node (make-object x y *city-node-size* *city-node-size* 'city-node)))
          (setf (node-number current-node) node-pos)
          (grid-utils:set-node-mapping node-pos current-node)
          (insert current-node)
          (and objects (insert-objects x y objects)))))
    (current-buffer)))
