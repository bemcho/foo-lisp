
(in-package #:foo-lisp)

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *game-message* "")
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
                             ((some (lambda (worm)
                                      (within-one n worm edge-alist)) glow-worms) '(lights!)))
                       (when (some #'cdr (cdr (assoc n edge-alist))) '(sirens!))))))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (setf *game-message* (format nil "You are on node [~d]" *player-pos*)))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

(defun draw-city ()
  (ugraph->png "../generated/city" *congestion-city-nodes* *congestion-city-edges*))


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
  (ugraph->png "../generated/known-city" (known-city-nodes) (known-city-edges)))


(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos (cdr (assoc *player-pos* *congestion-city-edges*))))
        (connections (cdr (assoc *player-pos* *congestion-city-edges*))))
    (if edge
        (handle-new-place edge pos charging)
        (setf *game-message* (format nil "You can't go there ->  position: [~d] you can go to: ~a" pos connections)))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (connections (mapcar #'car (cdr (assoc *player-pos* *congestion-city-edges*))))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (setf *game-message*  (format nil  "You are on position [~d] with ~a." pos (rest node) connections))
    ;;(draw-known-city)
    (cond ((member 'cops edge) (setf *game-message* "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charging
                                     (setf *game-message*  "You found the Wumpus! You WON!")
                                     (setf *game-message*  "You ran into the Wumpus. He killed you.")))
          (charging  (setf *game-message* "You wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (setf *game-message* "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))



;; After initializing a new Wumpus buffer, we set things up so that
;; pressing Control-R causes the game to reset.
(defmethod initialize-instance :after (( wumpus-world  wumpus-world) &key)
  (xelf:bind-event  wumpus-world '((:r."^R") :control) 'start-game))

;; Putting it all together

;; The START-GAME function builds the game board by inserting the
;; ball and paddle objects, then pasting in the bricks and border.


(defmethod start-game ((wumpus-world  wumpus-world))
  (with-slots (wumpus-hunter-sprite) wumpus-world
    (with-buffer wumpus-world
      (xelf:do-nodes  (node (current-buffer))
        (xelf:remove-node (current-buffer) node))
      (new-game)
      (insert  wumpus-hunter-sprite)
      (grid-utils:config *width* *height*
                         *city-node-size* *space-btw-nodes* *padding-inside-node*
                         *objects-size* (ceiling  (sqrt (length *congestion-city-nodes*))))
      )))


;; Now we define the main entry point for the game, the function
;; We set up our variables and then invoke [[file:dictionary/WITH-SESSION.html][WITH-SESSION]] to start
;; Xelf going.


(defun wumpus-game ()
  ;; Configure the screen dimensions
  (setf *window-title* "Grand Theft Wumpus")
  (setf *font* "sans-11")
  (setf *frame-rate* 5)
  (setf *font-texture-scale* 1)
  (setf *font-texture-filter* :linear)
  (setf *screen-width* *width*)
  (setf *screen-height* *height*)
  (setf *user-projects-directory*  "/static/")
  ;; Allow resizing of window and scaling
  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (with-session  
    (open-project :foo-lisp)
    ;; this indexes everything defined with DEFRESOURCE
    (index-all-images)
    (setf *project-path* (merge-pathnames "static/" *project-path*))
    (index-all-images)
    (index-all-samples)
    (index-pending-resources)
    (preload-resources)
    (let ((wumpus-world (make-instance 'wumpus-world)))
      ;; start the buffer running
      (xelf:bind-any-default-events wumpus-world)
      (switch-to-buffer wumpus-world)
      (start-game wumpus-world))))


;;;;;;;
;;(walk 3)r
;;(charge 14)

;;(known-city-edges)
;;*keyboard-events*
;;*node-events*
;;(draw-connections (car (known-city-edges)))
;;(node-pos-to-node 22)
;;(member 'not-cops '(cops not-cops wumpus))
;;*congestion-city-edges*
;;*congestion-city-nodes*
;;(member 18 *visited-nodes*)
;; (with-session
;;   (with-buffer 
;;       (xelf:clean-buffer (current-buffer))
;;(current-buffer)))
;;(wumpus-game)
;;(start-game (current-buffer))
;;(stop wumpus-world)
;;*player-pos*
;;(at-next-update (destroy self))
;;(open-shell (current-buffer))
;;(show-game-message 100 100 "Hello")
;;(wumpus-game)
;;(remove-node (current-buffer) (node-pos-to-node 16))











