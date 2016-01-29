;;; IDDFS.CL
;;; Iterative Deepening Depth-First Search, applied to robot planning.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 9 ("Planning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Solves the low-level robot planning problem in the text.
;;; Manages the combinatorial explosion by severely limiting
;;; the size of the world and imposing preconditions on each
;;; operator.

;;; The difference from the book version is that here we
;;; assume the gripper can grasp a block when the gripper
;;; occupies the square directly above the block.
;;; We assume that a block can be moved horizontally
;;; provided only that the space it moves into is vacant;
;;; it does not have to be lifted up an extra square and
;;; then lowered.  A plan is considered to be a solution
;;; even if the gripper is still *holding* block C after
;;; placing it on block B. A 12-step plan can then be
;;; found after examining only 6216 nodes.

;;; x positions of blocks and gripper can be from 0 to 2.
(defparameter *world-width* 3)
;;; y positions can be from 0 to 3 for blocks, 1 to 3 for gripper.
(defparameter *world-height* 4)

(defvar *fingers-open*)
(defvar *holding*)
(defvar *node-visits*)
(defparameter *blocks* '(a b c))

(let ((x-values (make-hash-table :size 10))
      (y-values (make-hash-table :size 10)) )
  (defun set-x-value (obj x)
    (setf (gethash obj x-values) x) )
  (defun set-y-value (obj y)
    (setf (gethash obj y-values) y) )
  (defun inc-x-value (obj)
    (incf (gethash obj x-values)) )
  (defun inc-y-value (obj)
    (incf (gethash obj y-values)) )
  (defun dec-x-value (obj)
    (decf (gethash obj x-values)) )
  (defun dec-y-value (obj)
    (decf (gethash obj y-values)) )
  (defun get-x-value (obj)
    (gethash obj x-values) )
  (defun get-y-value (obj)
    (gethash obj y-values) )
  (defun initialize ()
    "Sets the blocks and the gripper in the initial state."
    (set-x-value 'a 0) (set-y-value 'a 1)
    (set-x-value 'b 0) (set-y-value 'b 0)
    (set-x-value 'c 2) (set-y-value 'c 0)
    (set-x-value 'g 2) (set-y-value 'g 1)
    (setf *fingers-open* t)
    (setf *holding* nil)
    (setf *node-visits* 0)   ; count of nodes considered.
    )
  )

(defstruct operator
  name ; a symbol
  function ; a closure
  precondition) ; a closure

;;; Associate functions and their inverses with each operator, in a form
;;; that can be used to modify the current state.
;;; Note that the operators LEFT, RIGHT, and DOWN have preconditions that
;;; disallow either the gripper or a held object colliding with another object
;;; or with the table.

(let
 ((left (make-operator :name 'left 
   :function
     #'(lambda () (dec-x-value 'g)
                  (if *holding* (dec-x-value *holding*)) )
   :precondition
     #'(lambda () (and (> (get-x-value 'g) 0)
        (clear-near-gripper 'left)
        (if *holding* (clear-near-gripper 'leftu) t) )) ) )

  (right (make-operator :name 'right
  :function
    #'(lambda () (inc-x-value 'g)
                 (if *holding* (inc-x-value *holding*)) )
  :precondition
    #'(lambda () (and (< (get-x-value 'g) (1- *world-width*))
        (clear-near-gripper 'right)
        (if *holding* (clear-near-gripper 'rightu) t) )) ) )
 
  (down (make-operator :name 'down
  :function
    #'(lambda () (dec-y-value 'g)
                 (if *holding* (dec-y-value *holding*)) )
  :precondition
    #'(lambda ()
        (and (> (get-y-value 'g) 1)
             (if *fingers-open* (clear-near-gripper 'down)
               (clear-near-gripper 'downu) ) ) ) ))

  (up (make-operator :name 'up
  :function
    #'(lambda () (inc-y-value 'g)
                 (if *holding* (inc-y-value *holding*)) )
  :precondition
    #'(lambda () (< (get-y-value 'g) (1- *world-height*))) ) )

  (grasp (make-operator :name 'grasp
  :function
    #'(lambda () (setf *fingers-open* nil) 
                 (setf *holding*
                       (whats-at (get-x-value 'g)
                                 (1- (get-y-value 'g)) ) ) )
  :precondition
    #'(lambda () (and *fingers-open*
                      (whats-at (get-x-value 'g)
                                (1- (get-y-value 'g)) ) )) ) )
    ;; Don't grasp unless fingers are open and an object
    ;; is under the gripper.

 (open (make-operator :name 'open
  :function
    #'(lambda () (setf *fingers-open* t) (setf *holding* nil))
  :precondition
    #'(lambda () (and (not *fingers-open*) (not (would-drop)))) ) )
    ;; Don't open unless the fingers are closed and opening
    ;; would not result in dropping an object.
  )

 (defun inverse-op (op)
   "Returns the inverse operator."
   (rest (assoc op `((,left . ,right)(,right . ,left)
                     (,up . ,down)(,down . ,up)
                     (,grasp . ,open)(,open . ,grasp) ))) )

 (defparameter *operators* (list left right up down grasp open))
 ) ; end of LET form.

;;; IDDFS is the top-level searching function.
(defun iddfs (maxdepth)
  "Performs an Iterative-Deepening Depth-First Search."
  (catch 'solved   ; quit as soon as a solution is found.
    (dotimes (current-depth-limit maxdepth)
      (format t "~%Considering plans of length ~d."
              current-depth-limit)
      (dfs nil current-depth-limit) )
    (format t "~%No solution found.") ) )

(defun dfs (current-plan depth-left)
  "Performs a depth-first search from the current node for
   DEPTH-LEFT more levels. It does this by trying the different
   possible operators to extend the plan one step and then
   calling itself recursively."
  (incf *node-visits*) 
  (print-state) ; shows each state as generated.
                ; (comment this out to speed execution).
  (cond ((zerop depth-left)  ; Are we at current depth limit?
         (if (goal-test)     ; Yes, see if we have a solution.
             (progn (print-solution current-plan)
                    (throw 'solved t) )
             nil ) )
        ;; Not at max. depth -- try extending the plan.
        (t (dolist (this-op *operators*)
             ;; First make sure THIS-OP isn't simply
             ;; the inverse of the last operator.
             ;; This helps us avoid generating some dumb plans...
             (if (and
                   (if current-plan
                       (not (equal this-op
                                   (inverse-op (first current-plan)) )) 
                       t) ; don't allow immediate undo of last op.
                   (applicable this-op) ) ; Test precondition
                 (progn
                   (funcall (operator-function this-op)) ; Change state.
                   (dfs (cons this-op current-plan)
                        (1- depth-left) ) ; Recursive search.
                   (funcall
                     (operator-function
                       (inverse-op this-op) )) ) ) )) ) ) ; Backtrack.

(defun print-solution (plan)
  "Shows the plan found."
  (format t "~%Solution plan is:")
    (dolist (op (reverse plan))
      (format t " ~A" (operator-name op)) ) )

(defun applicable (op)
  "Determines whether an operator is applicable to
   the current state."
  (funcall (operator-precondition op)) )

(defun whats-at (x y)
  "If there is a block at location (x y) returns
   its name.  Otherwise returns NIL."
  (dolist (object *blocks*)
    (if (and (= x (get-x-value object))
             (= y (get-y-value object)) )
             (return object) ) ) )

(defun would-drop ()
  "If opening the fingers would cause a block to
   drop, returns true."
  (and *holding*
       (> (get-y-value 'g) 1)
       (clear-near-gripper 'downu) ) )

(defun clear-near-gripper (direction)
  "Returns T if the specified space relative to
   the gripper is vacant. LEFTU refers to the
   space under the space to the left, etc."
  (let ((dxdy
         (rest (assoc direction
                      '((down . (0 -1))(left . (-1 0))
                        (right . (1 0))(downu . (0 -2))
                        (leftu . (-1 -1))(rightu . (1 -1))) )) ))
    (null (whats-at (+ (first dxdy) (get-x-value 'g))
                    (+ (second dxdy) (get-y-value 'g)) )) ) )

(defun goal-test ()
  "Checks to see if block C is on block B, and B is
   on the table."
  (and (= (get-x-value 'c)(get-x-value 'b))
       (zerop (get-y-value 'b))
       (= 1 (get-y-value 'c)) ) )

(defun print-state ()
  "Displays the state of the world with a little picture.
   If the gripper fingers are open, it is displayed
   with g, else G."
  (let ((state-array
        (make-array (list *world-width* *world-height*)
                    :element-type 'character
                    :initial-element #\Space) ))
    (mapc #'(lambda (object) 
              (setf (aref state-array (get-x-value object)
                                      (get-y-value object) )
                    (rest
                      (assoc
                        object
                        '((a . #\A)(b . #\B)(c . #\C)) ) ) ) ) 
          *blocks*)
    (setf (aref state-array (get-x-value 'g) (get-y-value 'g))
          (if *fingers-open* #\g #\G) )
    (dotimes (y *world-height*)
      (terpri)
      (dotimes (x *world-width*)
        (format t "~A " (aref state-array
                        x
                        (- *world-height* y 1) )) ) )
    (format t "~%---------") ) )

(defun test ()
  "Performs a trial run of IDDFS."
  (initialize)
  (format t "~%Initial state:")
  (print-state)
  (iddfs 25) ; Iterative-deepening DFS to a max. depth of 24.
  (print-state)
  (format t "~%Number of node visits: ~D." *node-visits*) )

(test)
