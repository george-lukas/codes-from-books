;;; WSPLAN.CL
;;; World-space planner.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 9 ("Planning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Operators use a propositional form
;;; of the STRIPS representation:
(defstruct operator
  name
  preconditions
  add-list
  delete-list
  )

(defvar *domain-operators*)
(setf *domain-operators* nil)

(defun add-operator (new-operator)
  "Enters the NEW-OPERATOR at the end of the list of domain
   operators."
  (setf *domain-operators*
    (append *domain-operators* (list new-operator)) ) )

;;; Domain representation for the Sussman Anomaly problem:

(add-operator
  (make-operator
    :name 'put-a-on-b
    :preconditions '(a-is-on-table a-is-clear b-is-clear)
    :add-list '(a-is-on-b)
    :delete-list '(a-is-on-table b-is-clear)
    ) )

(add-operator
  (make-operator
    :name 'put-a-on-c
    :preconditions '(a-is-on-table a-is-clear c-is-clear)
    :add-list '(a-is-on-c)
    :delete-list '(a-is-on-table c-is-clear)
    ) )

(add-operator
  (make-operator
    :name 'put-b-on-c
    :preconditions '(b-is-on-table b-is-clear c-is-clear)
    :add-list '(b-is-on-c)
    :delete-list '(b-is-on-table c-is-clear)
    ) )

(add-operator
  (make-operator
    :name 'take-a-off-b
    :preconditions '(a-is-on-b a-is-clear)
    :add-list '(a-is-on-table b-is-clear)
    :delete-list '(a-is-on-b)
    ) )

(add-operator
  (make-operator
    :name 'take-c-off-a
    :preconditions '(c-is-on-a c-is-clear)
    :add-list '(c-is-on-table a-is-clear)
    :delete-list '(c-is-on-a)
    ) )

(add-operator
  (make-operator
    :name 'take-b-off-c
    :preconditions '(b-is-on-c b-is-clear)
    :add-list '(b-is-on-table c-is-clear)
    :delete-list '(b-is-on-c)
    ) )

;;; Here are descriptions of the initial and goal states.

(defparameter *initial-state*
  '(c-is-on-a
    a-is-on-table
    b-is-on-table
    c-is-clear
    b-is-clear) )

(defparameter *goal-state* '(a-is-on-b b-is-on-c))

;;; Plan representation is list of operators in reverse order.

(defparameter *empty-plan* nil)

;;; World-space, forward-chaining planning algorithm
;;; based on iterative-deepening depth-first search.

(defun ws-plan (max-depth)
  "Uses Iterative-deepening DFS to build a plan."
  (let
    ((planning-result
       (catch 'plan
         (dotimes (this-max max-depth)
           (ws-plan1 *initial-state*
                     *empty-plan*
                     this-max) ) ) ))
    (if planning-result
        (format t "~%Plan found: ~S." 
                (mapcar #'operator-name
                        (reverse planning-result) ) )
      (format t "~%Could not find a succesful plan.")
  ) ) )

(defun ws-plan1 (current-state current-plan depth-left)
  "Tests for goal reached.  If not, tries all operators 
   or backtracks."
  (cond ((goal-reached-p current-state)
         (throw 'plan current-plan) )
        ((zerop depth-left) nil)
        (t (dolist (op *domain-operators*)
             (if (subsetp (operator-preconditions op) 
                          current-state)
                 (ws-plan1
                   (set-difference 
                     (union current-state
                            (operator-add-list op))
                     (operator-delete-list op) )
                   (cons op current-plan)
		   (1- depth-left) )
	       nil) )) ) )

(defun goal-reached-p (current-state)
  (subsetp *goal-state* current-state) )

(ws-plan 5) ; Initiate a search to a maximum depth of 5-1=4.
