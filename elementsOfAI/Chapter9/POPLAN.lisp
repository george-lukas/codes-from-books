;;; POPLAN.CL
;;; A partial-order (non-linear) planner.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 9 ("Planning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Uses a logically complete planning algorithm to search in
;;; plan space for a partially ordered set of actions that
;;; achieves a given goal from a given initial state.

;;; Simplifying assumptions: the goal is a conjunction of
;;; atomic propositions.  No variables or explicit 
;;; negations are allowed.  Domain operators are expressed
;;; in a STRIPS-like notation.

;;; The strategy used is an Iterative-deepening depth-first
;;; regression method that builds plans by finding actions whose
;;; postconditions satisfy pending subgoals.

;;; A demonstration is provided using the classical planning
;;; problem (a.k.a. the "Sussman Anomaly" problem) in the blocks world.

;;; Operator definition stuff...
(defstruct operator
  name
  preconditions
  add-list
  delete-list
  )

(defvar *domain-operators*)
(setf *domain-operators* nil)

(defun add-operator (new-operator)
  "Enter the NEW-OPERATOR at the end of the list of domain
   operators."
  (setf *domain-operators*
    (append *domain-operators* (list new-operator)) ) )

;;; An action is a plan step...
(defstruct action
  id    ; a symbol
  op    ; an operator
  )

;;; ---------------------------------------------------
;;; Domain representation for the Sussman Anomaly problem:

;;; The first action is a dummy action that is pointed
;;; to by the propositions in the initial state as their
;;; "creator".
(defparameter *creator-action*
  (make-action
    :id 'creator-action
    :op
      (make-operator
        :name 'make-initial-state
        :preconditions nil
        :add-list
          '(c-is-on-a
            a-is-on-table
            b-is-on-table
            c-is-clear
            b-is-clear)
        :delete-list nil
         ) ) )
    
(defparameter *goal-action*
  (make-action
   :id 'goal-action
   :op
      (make-operator
        :name 'consume-goals
        :preconditions '(a-is-on-b
                         b-is-on-c)
        :add-list nil
        :delete-list '(a-is-on-b
                       b-is-on-c)
         ) ) )
    
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

;;; Plan representation...
(defstruct plan
  action-set  ; unordered collection of actions in the plan.
  partial-order ; set of temporal restrictions on actions
  coordinations ; set of ordered triples (A1, A2, P), with two actions
     ; and a proposition P which is a
     ; postcondition of A1 and a precondition of A2.
  )

;;; Here are descriptions of the initial state
;;; and the goal conjuncts.
;;; Note that all states are represented
;;; as a list of pairs of the form (proposition action-that-posted-it).
;;; The goal conjuncts list contains pairs of the form
;;;  (PROPOSITION  ACTION-THAT-REQUIRES-IT), where the action
;;; is the "dummy" action *GOAL-ACTION*.

(defparameter *empty-plan*
  (make-plan :action-set
               `(,*creator-action* ,*goal-action*)
             :partial-order
               `((< ,*creator-action* ,*goal-action*))
             :coordinations nil) )

(defparameter *goal-conjuncts*
  `((a-is-on-b ,*goal-action*)
    (b-is-on-c ,*goal-action*)
  ; (c-is-on-table ,*goal-action*)
  ) )

(defvar *reporting*)
(setf *reporting* t) ;; Display progress during planning.

;;; Don't print "#:  :" around uninterned symbols... 
(setf *print-gensym* nil) ;; This is a system-defined global.

;;;------------------------------------------------------
;;; Non-linear planning algorithm.

(defun plan (max-number-of-actions)
  "Uses non-linear regression to do planning."
  (gensym 0) ;; zero the GENSYM counter. But doesn't work in Allegro CL 4.1.
  (let (temp-plan)
    (if
      (setf temp-plan
        (catch 'plan
          (dotimes (i (1+ max-number-of-actions))
            (let ((num-actions-allowed-in-this-iteration (1+ i)))
              (if *reporting*
                  (format t "~%~%Constructing plans with up to ~A actions."
                          num-actions-allowed-in-this-iteration) )
              (plan1 *empty-plan*
                     *goal-conjuncts*
                     num-actions-allowed-in-this-iteration) ) ) ) )
        (progn (format t "~%~%Plan found: ")
               (print-plan temp-plan) )
      (format t "~%Could not find a successful plan.")
  ) ) )

;;; The following is based on Dan Weld's POP algorithm
;;; on page 16 of his tutorial manuscript.

(defun plan1 (current-plan subgoals-left num-actions-left)
  "Tests for goal reached.  If not, tries to make progress."
  (if *reporting*
      (progn
        (format t "~%Entering PLAN1 with ~A actions left"
                num-actions-left)
        (format t " and SUBGOALS-LEFT: ")
        (dolist (subgoal subgoals-left)
          (format t "~% ~A, posted by ~A."
                  (first subgoal)
                  (action-id (second subgoal)) ) ) ) )
  (if (null subgoals-left) ;; All goals satisfied?
      (throw 'plan current-plan) ;; Yes, go report plan and quit.
    ;;; Else, choose any subgoal -- let's say the first one.
    (let* ((this-goal (first subgoals-left))
           ;;; Get its proposition and the action that posted it.
           (p (first this-goal)) ;; precondition to be established.
           (a (second this-goal)) ) ;; action requiring P.
           ;;; First try to satisfy it by taking an existing action A1
           ;;; and constraining it to happen before A.
      (if (and (not (try-reusing-existing-action
                     p
                     a
                     current-plan
                     subgoals-left
                     num-actions-left))
               ;;; If that failed, try to establish P by
               ;;; creating a new action and constraining
               ;;; that to happen before A.
               (not (try-using-new-action
                     p
                     a
                     current-plan
                     subgoals-left
                     num-actions-left)) )
          nil) ;;; Otherwise, plan cannot be completed.
      ) ) )

(defun try-reusing-existing-action
    (p a current-plan subgoals-left num-actions-left)
  "If an action already in the plan can be constrained to
   guarantee the proposition P, then do it
   and plan on.  If not, return NIL."
  ;;; Loop through the actions in CURRENT-PLAN,
  ;;; and for each one that has P in its add-list,
  ;;; see if it can be constrained to be done before A.
  (dolist (old-action (plan-action-set current-plan) nil)
    (cond ((member p (operator-add-list (action-op old-action)))
           (cond (*reporting*
                  (format t "~%Trying to use old action ")
                  (print-action old-action) ))
           (if (consistent (list '< old-action a)
                           (plan-partial-order current-plan) )
               (let* ((new-constraints
                       (adjoin (list '< old-action a)
                               (plan-partial-order current-plan)
                               :test #'equal) )
                      (new-coordinations
                       (cons (list old-action a p)
                             (plan-coordinations current-plan) ) )
                      (new-plan
                       (make-plan :action-set
                                    (plan-action-set current-plan)
                                  :partial-order
                                     new-constraints
                                  :coordinations
                                     new-coordinations) ) )
                 (setf new-plan (maintain-coordinations new-plan))
                 ;;; If that worked, plan recursively
                 (cond (new-plan
                        (cond (*reporting*
                               (format t "~%Reusing action ")
                               (print-action old-action)
                               (format t "~% to satisfy ~A." p) ))
                        (plan1 new-plan
                               (remove
                                (list p a)
                                subgoals-left
                                :test #'equal)
                               num-actions-left) )
              ) ) ) ) ) ) )

(defun try-using-new-action
    (p a current-plan subgoals-left num-actions-left)
  "Tries to satisfy P with one of the available operators.
   When a good operator is found, a new action is created
   based on that operator, and an ordering constraint is
   set up between the new action and the action A."
  (if (zerop num-actions-left) 
      (if *reporting*
          (format t "~%Depth-limit reached. Backtracking...") )
    ;; depth-limit reached -- return NIL and backtrack.
    ;; Otherwise, try a new action...
    (dolist (op *domain-operators* nil)
      (if (member p (operator-add-list op))
          (let* ((new-action
                  (make-action :id (gensym "Action")
                               :op op) )
                 (new-coordinations
                  (cons (list new-action a p)
                        (plan-coordinations current-plan) ) )
                 (new-constraints 
                  (append `((< ,new-action ,a)
                            (< ,*creator-action* ,new-action) )
                          (plan-partial-order
                           current-plan) ) )
                 (new-plan
                  (make-plan :action-set
                             (cons new-action
                                   (plan-action-set current-plan) )
                             :partial-order new-constraints
                             :coordinations new-coordinations) )
                 )
            (if *reporting*
                (format t "~%Trying operator ~A."
                        (operator-name op) ) )
            ;;; Now let's regress the subgoal list through this action.
            (let ((new-subgoal-list
                   (regress subgoals-left new-action p) )
                  ;;; Next test for any threats and resolve them...
                  (consistent-plan
                   (maintain-coordinations new-plan) ) )
              (if consistent-plan
                  (plan1 consistent-plan
                         new-subgoal-list
                         (1- num-actions-left) )
                         ;;; Used up one "credit" of quota.
                nil) ) ) ) ) ) )
          
(defun regress (subgoals action p)
  "Returns a new list of subgoals consisting of those
   SUBGOALS other than P, unioned with all
   preconditions of ACTION.  Note that each subgoal
   is actually represented as a pair, whose right part
   is the requiring action."
  (let ((new-goals
         (remove (list p nil)
                 subgoals
                 :test
                 #'(lambda (x y)
                           (eql (first x)
                                (first y) ) ) ) ))
    (dolist (new-goal 
             (operator-preconditions (action-op action))
             new-goals)
      (push (list new-goal action) new-goals) ) ) )
      
(defun maintain-coordinations (plan)
  "Checks all coordinations and actions that might
   threaten them.  Resolves all threats and returns
   a new plan, or if it finds a threat that it
   cannot resolve, returns NIL."
  (let ((new-plan plan))
    (dolist (coordination (plan-coordinations plan))
      (let ((a1 (first coordination))
            (a3 (second coordination))
            (p (third coordination)) )
        (dolist (a2 (plan-action-set plan))
          (let ((constraints (plan-partial-order new-plan)))
            (if (and (member p (operator-delete-list
                                 (action-op a2) ))
                     (can-be-between a1 a2 a3 constraints) )
                (progn
                  (setf new-plan
                    (resolve-threat a1 a2 a3 constraints plan) )
                  (if (null new-plan)
                      (return-from maintain-coordinations nil) ) )
              ) ) ) ) )
    new-plan) )

(defun resolve-threat (a1 a2 a3 constraints plan)
  "Returns a plan that resolves the threat by A2 to the
   coordination between A1 and A3, or returns NIL if
   the threat cannot be resolved."
  (if *reporting*
      (progn
        (format t "~%Considering threat by ")
        (print-action a2)
        (format t "~% against coordination between ")
        (print-action a1)
        (format t "~% and ")
        (print-action a3) ) )
  (cond ((consistent `(< ,a2 ,a1) constraints) ; demotion
         (if *reporting*
             (format t "~%Resolving it by DEMOTION.") )
         (make-plan
          :action-set (plan-action-set plan)
          :partial-order (cons `(< ,a2 ,a1) constraints)
          :coordinations (plan-coordinations plan) ) )
        ((consistent `(< ,a3 ,a2) constraints) ; promotion
         (if *reporting*
             (format t "~%Resolving it by PROMOTION.") )
         (make-plan
          :action-set (plan-action-set plan)
          :partial-order (cons `(< ,a3 ,a2) constraints)
          :coordinations (plan-coordinations plan) ) )
        (t 
         (if *reporting*
             (format t "~%Threat could not be resolved.") )
         nil) ) )


;;; Functions for temporal reasoning....

(defun can-be-between (x y z constraints)
  "Returns T unless Y definitely precedes Z or
   Z definitely precedes Y, according to CONSTRAINTS,
   or Y is equal to either X or Z."
  (cond ((precedes y x constraints) nil)
        ((precedes z y constraints) nil)
        ((equal x y) nil)
        ((equal y z) nil)
        (t t) ) )

(defun precedes (x y constraints)
  "Returns T if it can be deduced from CONSTRAINTS
   that X must precede Y."
  (or (member (list '< x y) constraints :test #'equal)
      (try-transitivity
       (restrict-set constraints
                     #'(lambda (v) (equal (second v) x)) )
       y
       constraints) ) )

(defun restrict-set (set test-predicate)
  "Returns the subset of SET whose elements satisfy
   TEST-PREDICATE."
  (cond ((null set) nil)
        ((apply test-predicate (list (first set)))
         (cons (first set)
               (restrict-set (rest set)
                             test-predicate) ) )
        (t (restrict-set (rest set)
                         test-predicate)) ) )

(defun try-transitivity (x-constraints y constraints)
  "Tries each third component of X-CONSTRAINTS
   as a possible Z, to find out whether (< X Z)
   and Z precedes Y, and therefore X precedes Y."
  (dolist (x-constraint x-constraints nil)
    (let ((z (third x-constraint)))
      (if (precedes z y constraints)
          (return-from try-transitivity t)
        ) ) ) )

(defun consistent (new-constraint old-constraints)
  "Returns T iff the NEW-CONSTRAINT does not conflict
   with any OLD-CONSTRAINTS or their implications."
  (let ((x (second new-constraint))
        (y (third new-constraint)) )
    (cond ((equal x y) nil)
          ((precedes y x old-constraints) nil)
          (t t) ) ) )

;;; Functions to print out plans and their components...

(defun print-action (action)
  "Prints out ACTION without the gory details
   of the action's operator."
  (format t "(~A: ~S) "
          (action-id action)
          (operator-name (action-op action)) ) )

(defun print-plan (plan)
  "Prints the essential information in PLAN."
  (format t "~%Plan action set:~%")
  (dolist (action (set-difference
                   (plan-action-set plan)
                   (list *creator-action* *goal-action*) ))
    (print-action action) )
  (format t "~%Plan ordering constraints:")
  (dolist (constraint (plan-partial-order plan))
    (if (not (or (member *creator-action* constraint)
                 (member *goal-action* constraint) ))
     (print-constraint constraint) ) )
  (format t "~%Plan coordinations:")
  (dolist (coordination (plan-coordinations plan))
    (print-coordination coordination) )
  )

(defun print-constraint (constraint)
  "Prints an ordering constraint."
  (format t "~% Constraint: (< ")
  (print-action (second constraint))
  (print-action (third constraint))
  (format t ")") )

(defun print-coordination (coordination)
  "Prints a coordination triple."
  (format t "~% Coordination: (")
  (print-action (first coordination))
  (print-action (second coordination))
  (format t "~S)" (third coordination)) )

