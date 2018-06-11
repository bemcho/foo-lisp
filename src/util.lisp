;;These functions utils are from
;;OnLisp book of Paul Graham
(in-package #:foo-lisp)

;;misc
(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))


;;functions which search lists
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

;;Search functions which compare elements.
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max
                         score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;;mapping functions
(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))
(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))
;;i/o functions
(defun readlist (&rest args)
  (values (read-from-string
           (concatenate ’string "("
                         (apply #'read-line args)
                         ")"))))
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
     (let ((in (apply #'prompt args)))
       (if (funcall quit in)
           (return)
           (format *query-io* "~A~%" (funcall fn in))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
       (symbol-name sym)))

;;function builders
(defun memoize (fn)
  "Memoize returned result from function.Works with functions that return single values"
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

(defun compose (&rest fns)
  "Composes functions, all butlast must be single argument functions"
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
       #'(lambda (&rest args)
            (reduce#'funcall fns
                      :from-end t
                      :initial-value (apply fn1 args))))
     #'identity))

(defun fif (if then &optional else)
  "Function if
   Makes: (mapcar#'(lambda (x)
            (if (slave x)
                (owner x)
                (employer x)))
          people)

   To: (mapcar (fif#'slave#'owner#'employer) people)"
 #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun fint (fn &rest fns)
  "Function intersection.
   Makes: (find-if#'(lambda (x)
            (and (signed x) (sealed x) (delivered x)))
           docs)

   To: (find-if (fint#'signed#'sealed#'delivered) docs)"
  (if (null fns)
      fn
      (let ((chain (apply#'fint fns)))
       #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  "Function union.Same like fint but uses 'or' instead of 'and'"
  (if (null fns)
      fn
      (let ((chain (apply#'fun fns)))
       #'(lambda (x)
           (or (funcall fn x) (funcall chain x))))))

;;Function to define flat list recursers.
(defun lrec (rec &optional base)
  "Function to define flat list recursers.
   The first argument to lrec must be a function of two arguments: the current
   car of the list, and a function which can be called to continue the recursion.

;copy-list
 (lrec#'(lambda (x f) (cons x (funcall f))))

;remove-duplicates                    
 (lrec#'(lambda (x f) (adjoin x (funcall f))))

;find-if, for some function fn         
 (lrec#'(lambda (x f) (if (fn x) x (funcall f))))

;some, for some function fn            
 (lrec#'(lambda (x f) (or (fn x) (funcall f))))"
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                         #'(lambda ()
                              (self (cdr lst)))))))
   #'self))


;;functions for traversing trees
(defun rfind-if (fn tree)
  "Generalized find-if for trees.Our rfind-if takes the former approach, 
   so the caller can assume that the function given as the first argument will only be
   called on atoms"
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))

(defun ttrav (rec &optional (base#'identity))
  "Tree traverse.
   1. In the base case it returns its argument.
   2. In the recursive case, it applies cons to the recursions down the left (car)
   and right (cdr) subtrees."
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))
   #'self))


(defun trec (rec &optional (base#'identity))
  "The second arg to trec should be a function of three arguments: the current object and the two
   recursers. The latter two will be closures representing the recursions down the
   left and right subtrees."
  (labels
      ((self (tree)
         (if (atom tree)
             (if (functionp base)
                 (funcall base tree)
                 base)
             (funcall rec tree
                     #'(lambda ()
                          (self (car tree)))
                       #'(lambda ()
                            (if (cdr tree)
                                (self (cdr tree))))))))
   #'self))
