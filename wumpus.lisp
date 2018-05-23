
(in-package #:foo-lisp)

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
    (1+ (random *node-num*)))

(defun edge-pair (a b)
    (unless (eql a b)
        (list (cons a b) (cons b a))))

(defun make-edge-list ()
    (apply #'append (loop repeat *edge-num*
                          collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
    (remove-if-not (lambda (x) (eql (car x) node))
                   edge-list))

(defun get-connected (node edge-list)
    (let ((visited nil))
         (labels ((traverse (node)
                      (unless (member node visited)
                          (push node visited)
                          (mapc (lambda (edge) (traverse (cdr edge)))
                                (direct-edges node edge-list)))))
                 (traverse node))
    visited))

(defun find-islands (nodes edge-list)
    (let ((islands nil))
         (labels ((find-island (nodes)
                      (let* ((connected (get-connected (car nodes) edge-list))
                             (unconnected (set-difference nodes connected)))
                          (push connected islands)
                          (when unconnected (find-island unconnected)))))
                 (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
    (when (cdr islands)
        (append (edge-pair (caar islands) (caadr islands))
                (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))


(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x) (zerop (random *cop-odds*))) edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1 (mapcar (lambda (edge) (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list) :test #'equal))))
    (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))


(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x) (within-one x b edge-alist))
            (neighbors a edge-alist))))


(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num* collect (random-node))))
       (loop for n from 1 to *node-num*
             collect (append (list n)
                             (cond ((eql n wumpus) '(wumpus))
                                   ((within-two n wumpus edge-alist) '(blood!)))
                             (cond ((member n glow-worms) '(glow-worm))
                                   ((some (lambda (worm) (within-one n worm edge-alist)) glow-worms)
                                    '(lights!)))
                             (when (some #'cdr (cdr (assoc n edge-alist))) '(sirens!))))))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun find-empty-node ()
  (let ((x (random-node)))
       (if (cdr (assoc x *congestion-city-nodes*))
           (find-empty-node)
           x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))


(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node *congestion-city-nodes*)))
                     (if (eql node *player-pos*)
                         (append n '(*))
                         n))
                (list node '?)))
          (remove-duplicates (append *visited-nodes*
                                     (mapcan (lambda (node)
                                               (mapcar #'car (cdr (assoc node *congestion-city-edges*))))
                                             *visited-nodes*)))))
; Why not use neighbour function instead of last lambda?

(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))


(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos (cdr (assoc *player-pos* *congestion-city-edges*)))))
       (if edge
           (handle-new-place edge pos charging)
           (princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charging
                                     (princ "You found the Wumpus! You WON!")
                                     (princ "You ran into the Wumpus. He killed you.")))
          (charging (princ "You wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))



;;;;;;;;;;;;;;;;;;;;; xelf objects

;; Common Lisp packages

;; Then we must define a Common Lisp "package" for our game's code to
;; inhabit.


;; Configuring your space 

;; Here we define an arbitrary measurement unit used throughout, and
;; set up some variables to hold the height and width of the game
;; world.


(defparameter *unit* 10)
(defun units (n) (* *unit* n))

(defparameter *width* 1024)
(defparameter *height* 720)
(defparameter *city-node-size* (units 6))
(defparameter *space-btw-nodes* (units 4))
(defparameter *node-offset* (+ *city-node-size* *space-btw-nodes*))
(defparameter *objects-size* (units 1))
(defparameter *x-max-objects* (floor *width* *space-btw-nodes*))
(defparameter *y-max-objects* (floor *height* *city-node-size*))


(defclass wumpus-hunter-sprite (node)
  ((height :initform (units 2))
   (width :initform (units 2))
   (max-dx :initform 100)
   (max-dy :initform 100)
   (max-ddx :initform 0.01)
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
    (move wumpus-hunter-sprite heading speed)))



;; street
(defun make-street (x-start y-start x-end y-end width height)
  "Simply remove wall nodes where street should pass"
  (let ((left x)
        (top y)
        (right (+ x width))
        (bottom (+ y height)))
;;code goes here
    )
  )
;; Now we need walls around the game world in order to contain the
;; ball.
;; cops
(defclass city-node (node)
  ((color :initform "gray")
   (image :initform "city-node.png")))

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
   ((color :initform "gray50")))

;;
(defun make-object (x y width height object-type)
  (let ((obj (make-instance object-type)))
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


(xelf:defresource "bip.wav" :volume 20)

(defmethod collide :after((wumpus-hunter-sprite wumpus-hunter-sprite) (node node)))

(defun  wumpus-hunter-sprite () (slot-value (current-buffer) 'wumpus-hunter-sprite))
;; See also:

;;  - [[file:dictionary/KEYBOARD-DOWN-P.html][KEYBOARD-DOWN-P]]
;;  - [[file:dictionary/NUMBER-OF-JOYSTICKS.html][NUMBER-OF-JOYSTICKS]]
;;  - [[file:dictionary/LEFT-ANALOG-STICK-PRESSED-P.html][LEFT-ANALOG-STICK-PRESSED-P]]
;;  - [[file:dictionary/LEFT-ANALOG-STICK-HEADING.html][LEFT-ANALOG-STICK-HEADING]]

;; In the paddle's UPDATE method, we read the inputs and move the
;; paddle accordingly.


;; Building the game-world out of objects 

;; Now that we have all the pieces of our game world, it's time to put
;; them all together in a buffer. First we have a function to make a
;; wall of a specified height, width, and position.


(defun make-wall (x y width height)
   (let ((wall (make-instance 'wall)))
     (resize wall width height)
     (move-to wall x y)
     wall))

 

;; See also [[file:dictionary/MOVE-TO.html][MOVE-TO]], [[file:dictionary/RESIZE.html][RESIZE]].

;; This function MAKE-BORDER returns a buffer with four walls.


(defun make-border (x y width height)
  (let ((left x)
        (top y)
        (right (+ x width))
        (bottom (+ y height)))
    (with-new-buffer
       ;; top wall
      (insert (make-wall left top (- right left) (units 1)))
       ;; bottom wall
      (insert (make-wall left bottom (- right left (units -1)) (units 1)))
       ;; left wall
      (insert (make-wall left top (units 1) (- bottom top)))
       ;; right wall
      (insert (make-wall right top (units 1) (- bottom top (units -1))))
       ;; send it all back
      (current-buffer))))

(defun make-border (x y width height)
  (let ((left x)
        (top y)
        (right (+ x width))
        (bottom (+ y height)))
    (with-new-buffer
       ;; top wall
      (insert (make-wall left top (- right left) (units 1)))
       ;; bottom wall
      (insert (make-wall left bottom (- right left (units -1)) (units 1)))
       ;; left wall
      (insert (make-wall left top (units 1) (- bottom top)))
       ;; right wall
      (insert (make-wall right top (units 1) (- bottom top (units -1))))
       ;; send it all back
      (current-buffer))))

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
  (let ((counter 10))
      (dolist (obj objects)
        (let* ((xo (+ x  counter))
               (yo (+ y  counter)))
          (insert (make-object xo yo  *objects-size*  *objects-size* (get-class-for obj)))
          (setf counter (+ counter *objects-size*))))))

(defun get-x (node-pos)
  "gets X coordinate within screen size"
  (let ((new-x (* node-pos (random *width*))))
    (if (<= (+ new-x *node-offset*) *width*)
        new-x
        (get-x node-pos))))

(defun get-y (node-pos)
  "gets Y coordinate within screen size"
  (let ((new-y (* node-pos (random *height*))))
    (if (<= (+ new-y *node-offset*) *height*)
        new-y
        (get-y node-pos))))

(defun populate-city (city-nodes)
  (with-new-buffer
    (dolist (node city-nodes)g
      (let ((x (get-x (car node)))
            (y (get-y (car node)))
            (objects (rest node)))
        (insert (make-object x y *city-node-size* *city-node-size* 'city-node))
        (and objects (insert-objects x y objects))))
    (current-buffer)))

;; See also [[file:dictionary/INSERT.html][INSERT]] and [[file:dictionary/CURRENT-BUFFER.html][CURRENT-BUFFER]].

;; Now it's time for pretty rows of colored bricks.


(defparameter *row-colors* 
   '("dark orchid" "medium orchid" "orchid" "dark orange" "orange" "gold"))

 (defun row-color (row)
   (nth (mod row (length *row-colors*))
        *row-colors*))

 ;; See also [[file:dictionary/ADD-NODE.html][ADD-NODE]].

;; You can see that MAKE-PUZZLE also returns a new buffer. We'll put
;; together these component buffers into the final game board below
;; with a function called [[file:dictionary/PASTE.html][PASTE]].

;; But first, we need a Buffer subclass for the game board.


(defclass wumpus-world (buffer)
   ((wumpus-hunter-sprite :initform (make-instance 'wumpus-hunter-sprite))
    (background-color :initform "orange")
    (width :initform *width*)
    (height :initform *height*)))



;; After initializing a new Plong buffer, we set things up so that
;; pressing Control-R causes the game to reset.


(defmethod initialize-instance :after (( wumpus-world  wumpus-world) &key)
  (bind-event  wumpus-world '(:r :control) 'start-game))

;; Putting it all together

;; The START-GAME function builds the game board by inserting the
;; ball and paddle objects, then pasting in the bricks and border.


(defmethod start-game ((wumpus-world  wumpus-world))
  (with-slots ( wumpus-hunter-sprite) wumpus-world
    (with-buffer wumpus-world
      (new-game)
      (insert  wumpus-hunter-sprite)
      (paste-from wumpus-world (make-border 0 0 (- *width* (units 1)) (- *height* (units 1))))
      (paste-from wumpus-world (populate-city *congestion-city-nodes*))
       )))



;; Now we define the main entry point for the game, the function
;; PLONG. We set up our variables and then invoke [[file:dictionary/WITH-SESSION.html][WITH-SESSION]] to start
;; Xelf going.


(defun wumpus-game ()
  ;; Configure the screen dimensions
  (setf *window-title* "Grand Theft Wumpus")
  (setf *font* "sans-11")
  (setf *frame-rate* 60)
  (setf *font-texture-scale* 1)
  (setf *font-texture-filter* :linear)
  (setf *screen-width* *width*)
  (setf *screen-height* *height*)
  ;; Allow resizing of window and scaling
  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (with-session  
    (open-project :foo-lisp)
     ;; this indexes everything defined with DEFRESOURCE
    (index-all-images)
    (index-all-samples)
    (index-pending-resources)
    (preload-resources)
    (let ((wumpus-world (make-instance 'wumpus-world)))
      ;; start the buffer running
      (switch-to-buffer wumpus-world)
      (start-game wumpus-world))))
