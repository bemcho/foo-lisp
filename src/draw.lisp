;;;;;;;;;;;;;;;;;;;;; xelf objects

;; Common Lisp packages

;; Then we must define a Common Lisp "package" for our game's code to
;; inhabi.


;; Configuring your space 

;; Here we define an arbitrary measurement unit used throughout, and
;; set up some variables to hold the height and width of the game
;; world.

(in-package #:foo-lisp)

(defclass wumpus-hunter-sprite (node)
  ((height :initform (+ *objects-size* (units 2)))
   (width :initform (+ *objects-size* (units 2)))
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
    (let ((current-node-pos (caar (known-city-edges))))
      (and current-node-pos
           (grid-utils:move-node-to wumpus-hunter-sprite current-node-pos )))))


(defclass wumpus-world (buffer)
  ((wumpus-hunter-sprite :initform (make-instance 'wumpus-hunter-sprite))
   (background-color :initform "black")
   (width :initform *width*)
   (height :initform *height*)))
;; Now we need walls around the game world in order to contain the
;; ball.
;; cops
(defclass city-node (node)
  ((color :initform "gray50")
   (image :initform "city-node.png")
   (objects
    :reader objects
    :writer (setf objects)
    :initform nil)
   (node-number
    :reader node-number
    :writer (setf node-number)
    :initform 0)))

(defclass wumpus (node)
  ((height :initform (+ *objects-size* (units 2)))
   (width :initform (+ *objects-size* (units 2)))
   (max-dx :initform 100)
   (max-dy :initform 100)
   (max-ddy :initform 0.01)
   (color :initform "white")
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

(defclass mist (node)
  ((color :initform "gray50")
   (image :initform "mist.png")))

(defclass cloud (node)
  ((color :initform "gray50")
   (image :initform "cloud.png")
   ))

(defclass question-mark (node)
  ((color :initform "gray50")
   (image :initform "question-mark.png")
   ))

;;
(defun make-object (x y width height object-type)
  (let*((obj (make-instance object-type)))
    (resize obj width height)
    (move-to obj x y)
    obj))

(defun show-instructions (x y)
  "Draws instructions starting from x y"
  (with-buffer (current-buffer)
    (draw-string "2 blood nodes away is the wumpus.2 lights nodes away is some Glow Worms Gang.You see siren on the next one you are busted by cops. Kill the wumpus with right click or charge." x y :font *big-font* :color "orange")))

(defmethod draw :after ((wumpus-world  wumpus-world))
  (show-instructions 0 20)
  (draw-nodes (known-city-edges)))

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
(defparameter *keyboard-events* '()
  "doc")

(defmethod handle-event :around ((self buffer) event)
  (pushnew event *keyboard-events*))

(defparameter *node-events* '()
  "doc")

(defmethod handle-event :around ((self node) event)
  (pushnew event *node-events*))

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
         ((eql wumpus-city-symbol '?) 'question-mark)
        ))

(defun insert-objects (x y objects)
  "Inserts objects starting from x,y"
  (let ((counter 0))
    (dolist (obj objects)
      (let* ((xo (grid-utils:get-next-x (+ x *padding-inside-node*) counter))
             (yo (grid-utils:get-next-y (+ y *padding-inside-node*) counter)))
        (insert (make-object xo yo  *objects-size*  *objects-size* (get-class-for obj)))
        (incf counter)))))

(defun draw-cloud-background ()
  "Draws mist per row"
  (let ((x 0)
        (y 0))
      (dotimes (n 10)
        (insert (make-object x y (* *city-node-size*  (random 15)) (* *city-node-size* (random 4)) 'cloud))
        (setf y (* n *city-node-size*)))))



(defun draw-object(node)
  "Draws node and its objects"
  (let* ((node-pos (node-number node))
         (x (x node))
         (y (y node))
         (objs (objects node)))
    (and node
         (progn
           (if (member node-pos *visited-nodes*)
               (progn (insert node) (and objs  (insert-objects x y objs)))
               (insert (make-object x y (width node) (height node) 'question-mark)))
           (draw-string (write-to-string node-pos) x y :font *score-font* :color "white")
           ))))

(defun populate-city (city-nodes)
  (with-new-buffer
    (dolist (node city-nodes)
      (multiple-value-bind(grid-x grid-y)
          (grid-utils:get-nth-grid-box-coord (car node))
        (let*((node-pos (car node))
              (objects (rest node))
              (x grid-x)
              (y grid-y)
              (current-node (make-object x y *city-node-size* *city-node-size* 'city-node)))
          (setf (node-number current-node) node-pos)
          (setf (objects current-node) objects)
          (grid-utils:set-node-mapping node-pos current-node)
          )))
    (current-buffer)))


(defun draw-node (node-pos)
  "doc"
  (let ((current-node (grid-utils:node-pos-to-node node-pos)))
       (draw-object current-node)))

(defun draw-nodes (node-list)
  "doc"
  (dolist (conn-list node-list)
    (draw-connections conn-list)
    (dolist (node conn-list)
      (let* ((node-pos (if (listp node) (car node) node)))
        (draw-node node-pos)
      ))))

(defun plus-half-city-node (x-or-y)
  (+ x-or-y (floor *city-node-size* 2)))

(defun draw-connections (nodes)
  "(car nodes) is the source,(rest nodes) is a list of list eg:  '(() ()) targets"
  (and nodes
       (car nodes)
       (let* ((source-node (grid-utils:node-pos-to-node (car nodes)))
              (src-x (plus-half-city-node (x source-node)))
              (src-y (plus-half-city-node (y source-node)))
              (targets-pos (rest nodes)))
         (dolist (node-pos targets-pos)
           (let* ((target-node (grid-utils:node-pos-to-node (car node-pos)))
                  (x-target (x target-node))
                  (y-target (y target-node))
                  (half-node (plus-half-city-node 0))
                  (cops-size (* *objects-size* 2))
                  ;(x-half-path (plus-half-city-node (floor (max src-x (plus-half-city-node x-target)) 2)))
                  ;(y-half-path (plus-half-city-node (floor (max src-y (plus-half-city-node y-target)) 2)))
                  (cops (cdr node-pos)))
             (draw-line src-x src-y (plus-half-city-node x-target) (plus-half-city-node y-target) :color "orange")
             (and (member 'cops cops)
                  (insert (make-object (- src-x half-node) src-y cops-size cops-size 'cops))
                  (insert (make-object (- target-x half-node) target-y cops-size cops-size 'cops))))
             ))))








