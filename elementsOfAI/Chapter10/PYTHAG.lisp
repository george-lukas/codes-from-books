;;; PYTHAG.CL
;;; "Pythagoras" -- a program that demonstrates heuristically-guided
;;; concept formation in mathematics.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 10 ("Learning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Structure of the concept exploration system:
;;; There is an "agenda" (prioritized list of tasks)
;;; which is continually updated by the insertion of new
;;; tasks and the deletion of tasks that are completed.
;;; Each task is a specification for some activity.
;;; The function EXPLORE-CONCEPTS is a procedure which repeatedly
;;; takes the task of highest priority off the agenda and performs
;;; it with the help of other functions.

;;; An item on the agenda has the form: 
;;;	 (interest-value (Task-spec))

;;; There are two kinds of tasks, and their specifications
;;;  have the following forms:

;;;	(MAKE-SPECIALIZATION concept)
;;;	(FIND-EXAMPLES-OF concept) 

;;; -----------------------------------------------------------------
;;; The concepts under investigation are classes of geometric figures
;;; that can be derived from the most general class "OBJECT" using
;;; specialization through restrictive predicates.
;;; Since these predicates will be applied to actual geometric figures
;;; during the course of the program's exploration, we must define the
;;; representation scheme for the geometric figures.  For this program,
;;; they are all polygons.

;;; A polygon is represented as a list of points,
;;; and a point is a pair of coordinate values.
;;; The set of polygons used in the explorations
;;; is referred to as the "universe of objects" and
;;; it is defined as follows:

(defparameter *universe*
  '(*box* *square* *isosceles* *right-triangle*
      *trapezoid* *parallelogram* *rhombus* *multi*
      *line* *dot*) )

(defparameter *box* '((0 0) (0 5) (10 5) (10 0)) )
(defparameter *square* '((0 0) (0 10) (10 10) (10 0)) )
(defparameter *isosceles* '((0 0) (5 5) (10 0)) )
(defparameter *right-triangle* '((0 0) (4 3) (4 0)) )
(defparameter *trapezoid* '((0 0) (5 5) (20 5) (25 0)) )
(defparameter *parallelogram* '((0 0) (5 5) (15 5) (10 0)) )
(defparameter *rhombus* '((0 0) (4 3) (9 3) (5 0)) )
(defparameter *multi* '((0 0) (0 10) (4 15) (10 15) (15 10)
                          (15 4) (10 0) ) )
(defparameter *line* '((0 0) (10 0)) )
(defparameter *dot* '((0 0)) )

;;; -----------------------------------------------------------------

;;; A collection of predicates for the purpose of forming specializations
;;; is provided below:

(defparameter *predicates*
  '(equal-sides nonzero-area many-sides) )

;;; MANY-SIDES is true if P has more than 6 sides.
(defun many-sides (p)
  "Returns T if P has more than 6 sides."
  (> (length p) 6) )

;;; NONZERO-AREA is true if the area of P is not equal to zero.
(defun nonzero-area (p)
  "Returns T if P has nonzero area."
  (not (zerop (area p))) )

;;; AREA computes the area enclosed by polygon P.
(defun area (p)
  "Computes the area enclosed by polygon P."
  (/ (area1 p (first p)) 2) )

;;; AREA1 computes twice the sum of the areas under each segment.
;;; FIRST-PT is the first point of the polygon P when called
;;; at its top level.  It's used to make sure the last segment
;;; of the polygon is considered --- the one from the last point
;;; to the first point.
(defun area1 (p first-pt)
  "Returns 2 times the sum of the areas under the segments of P."
  (if (null (rest p))
      (* (dx first-pt (first p))
         (py first-pt (first p)) )
      (+ (area1 (rest p) first-pt)
         (* (dx (second p) (first p))
            (py (second p) (first p)) ) ) ) )

;;; EQUAL-SIDES is true if all sides of P have equal length.
(defun equal-sides (p)
  "Returns T if all sides of P have equal length."
  (apply #'= (side-length-sq-list p (first p))) )

;;; SIDE-LENGTH-SQ-LIST makes a list of squared lengths of
;;; the sides of P.
(defun side-length-sq-list (p first-pt)
  "Returns a list of the squared lengths of the sides of P."
  ( if (null (rest p))
       ;; last side connects to 1st point...
       (list (+ (sq (dx first-pt (first p)))
                (sq (dy first-pt (first p))) ))
       ;; other sides connect successive points...
       (cons (+ (sq (dx (second p) (first p)))
                (sq (dy (second p) (first p))) )
             (side-length-sq-list (rest p) first-pt) ) ) )

;;; SQ computes N squared.
(defun sq (n)
  "Returns N squared."
  (* n n) )

;;; DX, DY, PY, XC and YC are helping functions for
;;; coordinate arithmetic:
(defun dx (pt1 pt2) (- (xc pt1) (xc pt2)))

(defun dy (pt1 pt2) (- (yc pt1) (yc pt2)))

(defun py (pt1 pt2)
  "Returns the sum of the y coords of PT1 and PT2."
  (+ (yc pt1) (yc pt2)) )

(defun xc (pt) (first pt)) ; Get X coordinate of point.

(defun yc (pt) (second pt)); Get Y coordinate of point.

;;; declare global variables:
(defvar *reporting* nil)
(defvar *agenda* nil)
(defvar *concept-counter* nil)

;;; Let us now define the CONCEPT structure with 13 slots.
;;; In the following structure definition, we use several
;;; options, including specifying a special function
;;; for printing concepts, as well as default values for
;;; various slots. 
(defstruct (concept (:print-function briefly-print-concept))
  (id (new-atom))
  defn
  predicate
  interest
  (objects-to-try *universe*)
  (examples-found nil)
  (unused-predicates *predicates*)
  predicates-used-in-spec
  predicates-used-in-desc
  (number-found 0)
  (number-tried 0)
  parent
  subconcepts
  )

;;; The following function is used during debugging.
;;; Without it, tracing a function that manipulates
;;; a concept leads to an infinite print recursion
;;; due to the fact that parent and child concepts
;;; cyclically reference each other.
(defun briefly-print-concept (c stream level)
  "Provides a way to print out concepts without
   infinite recursion."
  (declare (ignore level))
  ;; Note (DECLARE (IGNORE LEVEL)) suppresses compiler warning.
  (format stream "~A" (concept-id c)) )


;;; -----------------------------------------------------------------
;;; To find examples, the procedure FIND-EXAMPLES-OF takes the list of
;;; objects not yet tried and tries a fixed number of them, (3 of them).
;;; It puts any examples found on the list of examples for the concept,
;;; and it updates the list of objects left to try.
;;; This procedure also does the following:
;;; It updates the interest value for the concept in accordance to
;;; the results of looking for examples.
(defun find-examples-of (c)
  "Tests objects for being examples of concept C, and updates
   the agenda according to the findings."
  (let ((objects-left (concept-objects-to-try c))
        x)
    ;; Test 3 objects not yet tried as possible examples of C:
    (dotimes (i 3 nil)
      (if (null objects-left)        
          (return (setf (concept-objects-to-try c) nil)) )
      (setf x (pop objects-left))
      (incf (concept-number-tried c)) ; doesn't work in Allegro 4.1
      (pop (concept-objects-to-try c))
      (cond ((apply (concept-predicate c) (list (eval x)))
             ;; An example has been found...
             (format t "~%~a is an example of concept ~a." 
               x (concept-id c))
             (setf (concept-examples-found c)
                   (adjoin x (concept-examples-found c)) )

             (incf (concept-number-found c)) )
            ;; But if the object is not an example...
            (t (format t
                 "~%~a is not an example of concept ~a."
                 x (concept-id c) )) ) )

    ;; The example-checking part of this task is over.
    ;; Now update the interest value for C:
    (setf (concept-interest c) (compute-concept-interest c))
    ;; If there are still objects not yet tried, enter a new 
    ;; task on the agenda to try 3 more objects.
    (if objects-left
        (put-on-agenda
          `(,(examples-task-interest c) ; Compute interest val.
                (find-examples-of ,c) ) )
        (if *reporting*
            (format t "~%All objects now tested for ~a."
                 (concept-id c) ) ) )
            
    (if *reporting* (display-agenda *agenda*))
    ;; If there is at least one example of the concept and no
    ;; specializations for this concept have yet been created,
    ;; and no tasks for such specialization are already on the
    ;; agenda, create a new task to make a specialization of C:
    (if (and (> (concept-number-found c) 0)
             (null (concept-subconcepts c))
             (no-spec-task *agenda* (concept-id c)) )
        (put-on-agenda
          `(,(spec-task-interest c) ; Compute task interest.
                (make-specialization ,c) ) ) )

    ;; Print out a current description of the concept:
    (display-concept c)
    ;; The FIND-EXAMPLES-OF task has now been completed.
    ) )

;;; PUT-ON-AGENDA inserts an entry of the form
;;; (interest-value (task-spec))  onto the agenda,
;;; in its place, so that items are ordered, highest
;;; interest-value first.
(defun put-on-agenda (task)
  "Inserts TASK in the priority queue *AGENDA*."
  (setf *agenda* (put-on-agenda1 task *agenda*)) )

;;; PUT-ON-AGENDA1 is the recursive slave to put-on-agenda.
(defun put-on-agenda1 (task task-list)
  "Handles the insertion of TASK in the right place."
  (cond ((null task-list) (list task))
        ((< (first task) (caar task-list))
         (cons (first task-list)
               (put-on-agenda1 task (rest task-list)) ) )
        (t (cons task task-list)) ) )

(defun no-spec-task (agenda c)
  "Returns T if no MAKE-SPECIALIZATION task
   with concept id C is on AGENDA."
  (not
    (member c agenda
      :test
      #'(lambda (x y)
          (and (eql (caadr y) 'make-specialization)
               (eql x (concept-id (cadadr y))) ) ) ) ) )

;;; CONCEPT-INTEREST computes the current interest value
;;; for concept C using a formula that involves the hit ratio. 
(defun compute-concept-interest (c)
  "Returns the INTEREST value for C using heuristic formula."
  (if (zerop (concept-number-tried c))
      (concept-interest (concept-parent c))
      (let ((r (/ (concept-number-found c)
                  (concept-number-tried c) )))
        (* 400.0 (- r (* r r))) ) ) )

;;; EXAMPLES-TASK-INTEREST computes the interest of a task to
;;; find examples of C as a weighted sum of the interests of
;;; C and it parent.
(defun examples-task-interest (c)
  "Returns the interest value of a FIND-EXAMPLES-OF task."
  (+ (* 0.8 (concept-interest (concept-parent c)))
     (* 0.2 (concept-interest c)) ) )

;;; SPEC-TASK-INTEREST computes the interest value 
;;; for a specialization task, according to the formula:
;;;  value = 10 times the parent's hit ratio.
;;; We add one to the denominator to avoid the possibility
;;; of division by zero.
(defun spec-task-interest (c)
  "Returns the interest value of a MAKE-SPECIALIZATION task."
  (/ (* 10.0 (concept-number-found c))
     (1+ (concept-number-tried c)) ) )

; -----------------------------------------------------------------
; A task of type MAKE-SPECIALIZATION requires that the system
;  attempt to create a representation for a new concept.
; To create a new concept, the function MAKE-SPECIALIZATION
;  creates a structure containing  the following:
; - A unique id which is a symbol, e.g., C1, C2, etc.
; - A definition of the concept in terms of the parent concept.
;   suitable for an explanatory printout.
; - A predicate that can be applied to any object
;   to determine whether it is an example of this concept.
; - An interest value for the concept computed using a rule
;   which takes into account the interest of the parent concept
;   and the interest of the predicate used to form the restriction.
; - A list of objects that have not yet been tried as possible examples,
;   initially the whole "UNIVERSE".
; - A list of examples found.
; - A list of the predicates NOT used in the definition of this concept -
;   this simplifies the procedure MAKE-SPECIALIZATION.
; - A list of the original (provided) predicates that have been used
;   along the path from OBJECT to this concept (used in DISPLAY-CONCEPTS).
; - A list of the predicates that have been used to create specializations
;   of this concept.
; - The number of examples found (so far).
; - The number of objects tried (so far).
; - A parent concept.
; - A list of subconcepts.
; These items are put on the property list of the atom under the
; types: ID, DEFN, PREDICATE, INTEREST, OBJECTS-TO-TRY, EXAMPLES-FOUND,
;	UNUSED-PREDICATES, PREDICATES-USED-IN-SPEC, 
;	PREDICATES-USED-IN-DESC, NUMBER-FOUND, NUMBER-TRIED,
;       PARENT, SUBCONCEPTS.
(defun make-specialization (c)
  "Performs a MAKE-SPECIALIZATION task."
  (let ((pred 
           ;; Select a predicate not already involved in the parent
           ;;  and not already used for a specialization of C.
           (select-pred (concept-unused-predicates c)
                        (concept-predicates-used-in-spec c) ))
        newc)

    (cond ((null pred)
           (if *reporting*
               (format t "~%Cannot further specialize concept ~a."
                    (concept-id c) ) )
           (return-from make-specialization nil) ) )
    ;; Indicate that the selected predicate is no longer available
    ;;  for other specializations of C:
    (push pred (concept-predicates-used-in-spec c))
		 
    ;; Create a new concept structure and make up a new ID...
    (setf newc (make-concept))
    ;; Set up links in concept hierarchy...
    (make-isa newc c)
    ;; Register the list of unused predicates for the new concept: 
    (setf (concept-unused-predicates newc)
          (remove pred (concept-unused-predicates c)) )

    ;; Formulate the definition of NEWC:
    (setf (concept-defn newc)
      (append '(an instance of)(list (concept-id newc))
              '(is a)(list (concept-id (concept-parent newc)))
              '(having)(list pred)) )
		 
    ;; Create the predicate which tests an object to see if
    ;;  it is an example of the new concept:
    (setf (concept-predicate newc)
          (create-concept-predicate newc c pred) )

    ;; Store the list of predicates that should be used to
    ;;  describe this concept:
    (setf (concept-predicates-used-in-desc newc)
          (cons pred (concept-predicates-used-in-desc c)) )

    ;; Set up a task to find examples of the new concept:
    (put-on-agenda
      ;; interest = interest of parent concept.
       `(,(concept-interest c)
            (find-examples-of ,newc) ) )
    ;; Set up a task to make another specialization of C:
    (put-on-agenda
      `(,(spec-task-interest c)
            (make-specialization ,c) ) )
    ) )

;;; SELECT-PRED returns a member of L1 - L2.
;;; Used to selects a predicate from those available
;;; and not yet used for specializing this concept.
(defun select-pred (l1 l2)
  "Returns a member of L1 not in L2."
  (let ((p (set-difference l1 l2)))
    (if (null p) nil (first p)) ) )


;;; NEW-ATOM returns a new atom each time it is called,
;;; beginning with C1, then C2, etc.
(defun new-atom ()
  "Returns a new symbol: first C1, then C2, etc."
  (incf *concept-counter*)
  (intern
    (concatenate 'string
                 "C"
                 (prin1-to-string *concept-counter*) ) ) )

(defun create-concept-predicate (newc c pred)
  "Creates a predicate which tests an object to see if
   it is an example of the concept NEWC, using the
   existing predicate for the parent concept C."
  (let
    ((new-predicate
       #'(lambda (obj) 
           (and (apply pred (list obj))
                (apply (concept-predicate c) (list obj)) ) ) ))
    ;; If reporting is enabled, show predicate:
    (if *reporting*
        (format t "~%Creating concept ~a with predicate: ~s"
                (concept-id newc) new-predicate) )
    new-predicate) )

;;; EXPLORE-CONCEPTS contains the main control loop.
(defun explore-concepts ()
  "Top-level loop for concept exploration."
  (let (current-task)
    (loop
      (if (null *agenda*) (return))
      ;; Select task at head of agenda...
      (setf current-task (second (first *agenda*)))
      (pop *agenda*)  ; Remove it from the agenda.
      (funcall (first current-task)
               (second current-task) ) ; Perform current task.
        ) ) )

;;; Functions for manipulating the concept hierarchy:

;;; ADD-SUBSET records the fact that X is a subset of Y.
(defun add-subset (x y)
  "Inserts X into the list of subconcepts of Y."
  (setf (concept-subconcepts y)
        (adjoin x (concept-subconcepts y)) ) )

;;; ADD-SUPERSET records the fact that X is a superset of Y.
(defun add-superset (x y)
  "Makes X be the parent of Y."
  (setf (concept-parent y) x) )

;;; MAKE-ISA sets up a bi-directional ISA link between X and Y.
(defun make-isa (x y)
  "Creates 2-way ISA hierarchy link."
  (add-superset y x)
  (add-subset x y) )

;-----------------------------------------------------------

;;; INITIALIZE sets up the concept hierarchy to contain a single
;;; concept, "OBJECT" from which specializations will be made.
(defvar *object*)
(setf *dummy* (make-concept :id 'dummy))
(defvar *dummy*)

(defun initialize ()
  "Sets the concept OBJECT to the unprocessed state,
   and initializes the agenda and counter for concepts."
  (setf *object* (make-concept
    :id 'object
    :predicate
      #'(lambda (x) (declare (ignore x)) t)  ; always true.
     ;; Note (DECLARE (IGNORE X)) suppresses compiler warning.
    :unused-predicates *predicates*
    :predicates-used-in-spec nil
    :interest 50) )

  ;; One of the interest-computing functions wants '*OBJECT* to
  ;;  have a parent concept, so we set up a *DUMMY*:
  (make-isa *object* *dummy*)
  (setf (concept-interest *dummy*) 50)

  ;; Start the concept counter at 0:
  (setf *concept-counter* 0)

  ;; Set up the initial agenda of tasks:
  (setf *agenda*
    `((50 (find-examples-of ,*object*))
      (25 (make-specialization ,*object*)) ) ) )


;;; DISPLAY-CONCEPT prints out the status of a concept.
(defun display-concept (c)
  "Prints parts of the description of concept C,
   without any infinitely recursive printing."
  (format t "~%~%Status of concept: ~a, " (concept-id c))
  (format t "Definition: ~s."
    (concept-defn c) )
  (format t "~%  Specialization of ~a."
    (concept-id (concept-parent c)))
  (format t
    "~%  Interest: ~7,3f;  ~s examples found out of ~s tried."
    (concept-interest c)
    (concept-number-found c)
    (concept-number-tried c) )
  (format t "~%  Examples found: ~s.~%"
    (concept-examples-found c) ) )

(defun display-task (task)
  "Prints out one agenda task, without any infinite recursion."
  (format t "~% (~7,2f (~S ~S))"
    (first task) (caadr task) (concept-id (cadadr task)) ) )

(defun display-agenda (agenda)
  "Prints out the current agenda of tasks."
  (format t "~%Agenda is: ( " )
  (mapc #'display-task agenda)
  (format t " ) ") )

;-----------------------------------------------------------
;;; TEST exercises the program.
(defun test ()
  "Sets up and executes a run of the program."
  (initialize)         ; Set up initial concept OBJECT etc.
  (setf *reporting* t) ; Display execution details.
  (explore-concepts) ) ; Create and evaluate new concepts.

(test)
