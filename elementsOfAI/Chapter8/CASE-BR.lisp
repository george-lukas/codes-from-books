;;; CASE-BR.CL
;;; Basic structures for a small demonstration of
;;; Case-Based Problem Solving.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 8 ("Commonsense Reasoning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; This program tries to solve simple menu-planning problems
;;; using case-based reasoning.
;;; Given a menu "problem" it tries to match this problem to
;;; the problems associated with stored examples in
;;; its database of solved problems (the examples database).
;;; It begins with the most closely matching case and tries
;;; to adapt the solution from that case by eliminating the
;;; discrepancies between the current problem and the retrieved problem
;;; and fixing any constraint violations in the retrieved solution.

;;; 
;;; Part 1:  Representation of Cases
;;;  We use the symbol EXAMPLE instead of CASE, since CASE has
;;;  a built-in meaning in Common Lisp.

;;; A example consists of three parts: (<problem>, <solution>, <result>).
;;; A PROBLEM consists of 4 parts:
;;;   (<cuisine>, <n-diners>, <diet-restrictions>, <cost-per-person>)
;;; A SOLUTION consists of 3 parts:
;;;   (<main dish>, <appetizer>, <dessert>)

;;; Here <cuisine> refers to the region of the world or cuisine type,
;;; and <#diners> refers to the number of people that will be eating.
;;; The third part, <diet-restrictions>, is a list of constraints
;;; that the menu should satisfy.
;;; Finally, <cost-per-person> indicates how much money the host or
;;; hostess intends to spend.

;;;  The examples.
;;; <----problem-------------->  <----------solution-------->   <-result->
;;; CUISINE  N DIET-RESTR $/per. MAIN-DISH APPETIZER   DESSERT     RESULT
;;; Swedish  4 no-meat    10     Gravlax   Mussels     Linzertorte  S
;;; Mexican  6 no-gluten   3     Tacos     Chips&Salsa Fruit        S
;;; Szechuan 2 nil         5     Chicken-w-Garlic-Sauce
;;;                                        Spring-rolls Fort-cookie S

(defstruct example
  problem
  solution
  result)

(defstruct problem
  cuisine
  n-diners
  diet-restrictions
  cost-per-person)

(defstruct solution
  main-course
  appetizer
  dessert)

(defparameter *example1*
  (make-example
   :problem
   (make-problem :cuisine 'swedish
                 :n-diners 4
                 :diet-restrictions '(no-meat)
                 :cost-per-person 10)
   :solution
   (make-solution :main-course 'gravlax
                  :appetizer 'mussels
                  :dessert 'linzertorte)
   :result 's) )


(defparameter *example2*
  (make-example
   :problem
   (make-problem :cuisine 'mexican
                 :n-diners 6
                 :diet-restrictions '(no-gluten)
                 :cost-per-person 3)
   :solution
   (make-solution :main-course 'tacos
                  :appetizer 'chips-n-salsa
                  :dessert 'fruit)
   :result 's) )

(defparameter *example3*
  (make-example
   :problem
   (make-problem :cuisine 'szechuan
                 :n-diners 2
                 :diet-restrictions '()
                 :cost-per-person 5)
   :solution
   (make-solution :main-course 'chicken-w-garlic-sauce
                  :appetizer 'spring-rolls
                  :dessert 'fortune-cookies)
   :result 's) )

(defparameter *example-database* (list *example1* *example2* *example3*))

;;; Representations for dietary knowledge:
(let ((nutrition-misc  (make-hash-table :size 20 :test #'equal)))
  (defun food-fact (food property)
    "Stores the assertion that FOOD has PROPERTY."
    (setf (gethash (list food property) nutrition-misc) t) )
  (defun food-fact-p (food property)
    "Returns true if it was previously asserted that
     FOOD has PROPERTY; otherwise returns NIL."
    (gethash (list food property) nutrition-misc) )
 )

(food-fact 'gravlax 'no-meat)
(food-fact 'gravlax 'no-gluten)
(food-fact 'mussels 'no-meat)
(food-fact 'mussels 'no-gluten)
(food-fact 'mussels 'no-sugar)
(food-fact 'linzertorte 'no-meat)
(food-fact 'fruit 'no-meat)
(food-fact 'fruit 'no-gluten)
(food-fact 'fruit 'no-sugar)
(food-fact 'fortune-cookie 'no-meat)
(food-fact 'tacos 'no-sugar)
(food-fact 'chips-n-salsa 'no-sugar)
(food-fact 'chips-n-salsa 'no-meat)

;;; Part 2: Matching of Problems

;;; The interesting part of matching problems with cases in the
;;; database is the design of the function that measures the
;;; distance between the CURRENT-PROBLEM and the EXAMPLE-PROBLEM.

;;; We use a distance function on problems:
;;;  of the form a1 x1 + a2 x2 + ... + an xn
;;; Each xi is separate distance value computed according to a function
;;; particular to that attribute.  In our case,
;;; Distance(current-problem, example-problem) =
;;;   a1 CUISINE-DIST(Ep, Ec)
;;; + a2 N-DINERS-DIST(NP, NC)
;;; + a3 DIET-REST-DIST(DRP, DRC)
;;; + a4 COST-DIST(CP, CC).
;;;  We'll define these component distance functions later.
;;;  The definitions will be simple, to keep the emphasis on basic CBR.

;;; Here's the overall distance function.
(defun distance (current-problem example-problem)
  "Returns the computed distance between the 2 problems."
  (+ (* 5 (cuisine-dist
            (problem-cuisine current-problem)
            (problem-cuisine example-problem) ))
     (* 1 (n-diners-dist
            (problem-n-diners current-problem)
            (problem-n-diners example-problem) ))
     (* 2 (diet-rest-dist
            (problem-diet-restrictions current-problem)
            (problem-diet-restrictions example-problem) ))
     (* 1 (cost-pp-dist
            (problem-cost-per-person current-problem)
            (problem-cost-per-person example-problem) )) ) )

;;; Here is a tree representing a hierarchy of cuisines.
;;; It's used by the distance function CUISINE-DIST.
(defparameter *cuisines*
  '(world (asian (chinese (szechuan)))
          (european (scandinavian (norwegian) (swedish))
                    (italian (tuscan) (sicilian)) )
          (american (mexican) (canadian))
          ) )

(defconstant *cuisines-depth* 3) ; This tree has maximum depth 3.
;;; This should be changed if the tree is changed to have a different
;;; maximum depth.

;;; The next function is also used in computing the 
;;; distance between two cuisines.
(defun lowest-common-ancestor (tree n1 n2)
  "Returns the node in TREE whichs the LCA of N1 and N2."
  (if (or (not-in n1 tree) (not-in n2 tree)) nil
    (let ((subtrees (rest tree)))
      (loop (if (null subtrees) (return (first tree)))
        (let ((temp
               (lowest-common-ancestor (first subtrees) n1 n2) ))
          (if temp (return temp)) )
        (pop subtrees)
      ) ) ) )

(defun not-in (node tree)
  "Returns T if NODE does not occur in TREE."
  (cond ((eql tree node) nil)
        ((atom tree) t)
        (t (and (not-in node (first tree))
                (not-in node (rest tree)) ) ) ) )

;;; The function CUISINE-DIST takes two cuisines (representing
;;; food styles), CUISINE1 and CUISINE2.  It returns a value
;;; indicating the extent to which style CUISINE2 could be 
;;; expected to satisfy a request for style CUISINE1.
;;; This value is computed as follows:
;;; If style CUISINE1 is equal to CUISINE2 or a generalization
;;; of CUISINE2, then the value is 0.
;;; Otherwise, let A be the lowest common ancestor of CUISINE1
;;; and CUISINE2 in the tree of cuisines.
;;; If A equals CUISINE2 (i.e., CUISINE2 is a generalization
;;; of CUISINE1) then the value is half the "span" of A,
;;; where the spans of the deepest tree nodes
;;; are 1 and the spans of nodes at the next deepest level is 2,
;;; then 4, etc., doubling each level.
;;; Finally if neither of CUISINE1 or CUISINE2 is equal to A,
;;; then the value is the span of A.
(defun cuisine-dist (cuisine1 cuisine2)
  "Returns a heuristic 'distance' value based upon the
   positions of CUISINE1 and CUISINE2 in the tree.  This is not a
   true distance metric, as it is intentionally asymmetric."
  (let ((lca (lowest-common-ancestor *cuisines* cuisine1 cuisine2)))
    (cond ((null lca) (expt 2 *cuisines-depth*))
	  ((eql cuisine1 lca) 0)
          ((eql cuisine2 lca) (/ (span lca) 2))
          (t (span lca)) ) ) )

(defun span (c)
  "Returns a power of 2 depending on the level of C,
   large if near root, etc."
  (let ((depth (tree-depth c *cuisines*)))
    (expt 2 (- *cuisines-depth* depth)) ) )

(defun tree-depth (node tree)
  "Returns the depth of NODE in TREE."
  (cond ((eql node (first tree)) 0)
        (t (let ((subtrees (rest tree)))
             (loop (if (not (not-in node (first subtrees)))
                     (return (1+ (tree-depth node (first subtrees)))) )
                   (pop subtrees) ) )) ) )

(defun n-diners-dist (n1 n2)
  "Returns the postive numerical distance between N1 and N2."
  (abs (- n1 n2)) )

(defun diet-rest-dist (dr1 dr2)
  "Returns the number of elements that occur in
   either set DR1 or set DR2 but not both."
  (+ (length (set-difference dr1 dr2))
     (length (set-difference dr2 dr1)) ) )

(defun cost-pp-dist (cpp1 cpp2)
  "Returns the postive numerical distance between CPP1 and CPP2."
  (abs (- cpp1 cpp2)) )

;;; Matching of problems with examples in the database
;;;  -- performed only on the PROBLEM component of examples...

(defun retrieve-examples (current-problem example-database)
  "Returns a list of pairs of the form (DISTANCE EXAMPLE),
   sorted in order of lowest DISTANCE first."
  (let ((return-list nil))
    (dolist (this-example example-database return-list)
      (setf return-list
        (insert-pair (distance current-problem
                               (example-problem this-example) )
                     this-example
                     return-list) ) ) ) )

;;; The following is a hleping function for RETRIEVE-EXAMPLES.
;;; It effects the main part of an insertion sort.
(defun insert-pair (value example the-list)
  "Returns THE-LIST with the pair (VALUE EXAMPLE) inserted
   in its place, so that VALUEs are increasing."
  (cond ((null the-list) (list (list value example)))
        ((< value (first (first the-list)))
         (cons (list value example) the-list) )
        (t (cons (first the-list)
                 (insert-pair value
                              example
                              (rest the-list) ) )) ) )

;;; Part 3:  Interpretation of Examples, including transferring
;;;  appropriate parts of the solution, and resolving 
;;;  conflicts in components.

(defun adapt-solution (current-problem trial-example)
  "Applies adaptation rules to try to transform
   the solution of the selected trial example into a
   solution for the current problem."
  (let (temp)
    (and
     (setf temp (fix-cuisine current-problem trial-example))
     (setf trial-example temp)
     (setf temp (fix-n-diners current-problem trial-example))
     (setf trial-example temp)
     (setf temp
           (fix-diet-conflicts current-problem trial-example) )
     (setf trial-example temp)
     (setf temp (fix-cost current-problem trial-example) )
     )
    (if (null temp) (report-failure trial-example)
      temp) ) )

(defun report-failure (example)
  "Prints a message that example adaptation is impossible."
  (format t "~%Unable to adapt example to current problem. ")
  (format t "~%The partially adapted example is:~%~S" example)
  example)

(defun fix-cuisine (current-prob example)
  "Although in general we might be able to apply a 
   transformation that changes the cuisine of a 
   meal without corrupting it at the same time, in 
   this program we don't make any changes at all
   when this function is called.  It's provided here 
   for consistency of form."
 example)

(defun deep-copy-example (example)
  "Copies an example, including its problem and 
   solution structures."
  (make-example
    :problem (copy-problem (example-problem example))
    :solution (copy-solution (example-solution example))
    :result (example-result example) ) )

(defun fix-n-diners (current-problem example)
  "Changes the number of diners in retrieved example."
  (let ((copy (deep-copy-example example)))
    (setf (problem-n-diners (example-problem copy))
      (problem-n-diners current-problem) )
    copy) )

(defun fix-diet-conflicts (current-problem example)
  "If main course, appetizer and dessert in solution do not
   contain an offending quality, forgets the restriction.
   Otherwise applies substitution transformations.
   For each diet restriction in the current problem, sees if the
   menu courses in the solution satisfy it.  
   If not, applies a transformation to each offending course.
   Returns the updated example."
  (let ((copy (deep-copy-example example))
        (current-main-course
          (solution-main-course (example-solution example)) )
        (current-appetizer
          (solution-appetizer (example-solution example)) )
        (current-dessert
          (solution-dessert (example-solution example)) ) )
    (dolist (this-diet-restr
              (problem-diet-restrictions current-problem) )
      ;;; See if main course satisfies this diet rest.
      ;;; If so, do nothing,
      (if (not (satisfies-diet current-main-course this-diet-restr))
      ;;; If not, modify current-soln...
          (setf current-main-course
            (apply-diet-subst this-diet-restr current-main-course) ) )
      ;;; See if appetizer satisfies this diet rest.
      ;;; If so, do nothing,
      (if (not (satisfies-diet current-appetizer this-diet-restr))
      ;;; If not, modify current-soln...
          (setf current-appetizer
            (apply-diet-subst this-diet-restr current-appetizer) ) )
      ;;; See if dessert satisfies this diet rest.
      ;;; If so, do nothing,
      (if (not (satisfies-diet current-dessert this-diet-restr))
      ;;; If not, modify current-soln...
          (setf current-dessert
            (apply-diet-subst this-diet-restr current-dessert) ) )
      )

      ;;; now create the new example that includes the modified courses...
    (setf (solution-main-course (example-solution copy))
      current-main-course)
    (setf (solution-appetizer (example-solution copy))
      current-appetizer)
    (setf (solution-dessert (example-solution copy))
      current-dessert)

    (if (and current-main-course current-appetizer current-dessert)
       copy ; if all courses now meet constraints, return the altered copy.
      (report-failure copy) ) ) ); Otherwise, the case fails

(defun satisfies-diet (course diet-restr)
  "Returns T if COURSE satisfies DIET-RESTR."
  (food-fact-p (basic-course course) diet-restr) )

(defun basic-course (course)
  "Returns the original course without substitution info."
  (if (symbolp course) course
    (basic-course (first (last course))) ) )

(defun apply-diet-subst (diet-restr course)
  "Returns an expression representing COURSE with
   modifications."
  (if (replacement diet-restr) ; if a substitution is possible, do it.
      (list 'substitute
            (replacement diet-restr)
            'for
            (offender diet-restr)
            'in
            course)
    nil) ) ; if no substitution is known, the case fails.

;;; *INGREDIENT-FIXES* is a list of pairs of the form
;;; (<restriction> . <substitution>),
;;; where each substitution is of the form 
;;; (<offending-ingredient> . <replacement-ingredient>).
(defparameter *ingredient-fixes*
    '((no-gluten . (wheat-flour . rice-flour))
      (no-sugar  . (sugar . dried-fruit))
      (no-meat   . (meat  . tofu))
      (seafood   . (meat  . shrimp)) ) )

(defun replacement (diet-restr)
  "Returns new ingredient used to satisfy DIET-RESTR."
  (rest (rest (assoc diet-restr *ingredient-fixes*))) )

(defun offender (diet-restr)
  "Returns offending ingredient associated with DIET-RESTR."
  (first (rest (assoc diet-restr *ingredient-fixes*))) )

(defun fix-cost (current-problem example)
  "If solution cost per person exceeds problem cost per person,
   replaces appetizer by CRACKERS."
  (if (> (problem-cost-per-person (example-problem example))
         (problem-cost-per-person current-problem) )
      (let ((copy (deep-copy-example example)))
        (setf
          (solution-appetizer (example-solution copy))
          'CRACKERS)
        (decf  ;; also reduce estimated cost per person by $2.
          (problem-cost-per-person (example-problem copy))
          2) 
        copy)
    example) )
;;; Note that reducing the estimated cost by $2 currently does
;;; not affect the resulting solution.  However, it might be
;;; important if the adapted case were to be added to the
;;; database of examples for use in solving future problems.

;;; SOLVE is almost the top-level function of this program,
;;; but, CBR calls SOLVE, and so it is the top-level function.
(defun solve (problem)
  "Tries to solve the menu-planning PROBLEM, by retrieving
   matching examples, and modifying their solutions."
  (let ((matching-examples
         (retrieve-examples problem *example-database*) )
        (done nil)
        (current-example nil) )
    (loop
      (if (or done (null matching-examples))
          (return current-example) )
      (setf current-example (second (pop matching-examples)))
      (if (setf current-example
                (adapt-solution problem current-example) )
          (setf done t) ) )
    current-example) )

(defun cbr ()
  "Prompts the user for menu problems and applies
   case-based reasoning to try to solve them."
  (format t "~%Welcome to the case-based menu planner!~%")
  (let ((sample-problem (make-problem))
        solution)
    (loop
      (format t "~%What type of cuisine do you want? ")
      (format t "~% E.g., EUROPEAN, AMERICAN, NORWEGIAN, CHINESE, etc. ")
      (setf (problem-cuisine sample-problem) (read))
      (format t "~%How many will be eating? ")
      (format t "~% Enter a number: ")
      (setf (problem-n-diners sample-problem) (read))
      (format t "~%Give a list of diet restrictions, possibly NIL. ")
      (format t "~% E.g., (no-gluten no-sugar no-meat): ")
      (setf (problem-diet-restrictions sample-problem)
        (loop (let ((temp (read)))
                (if (listp temp) (return temp))
                (format t "~% You must enter a list: ") )) )
      (format t "~%What is the target cost per person? ")
      (format t "~% Enter a number: ")
      (setf (problem-cost-per-person sample-problem) (read))
      (format t "~%Searching the database of cases...~%")
      (setf solution (solve sample-problem))
      (print-solution (example-solution solution))
      (format t "~%Another example? Y or N...")
      (if (eql (read) 'n) (return 'good-bye)) ) ) )

(defun print-solution (solution)
  "Nicely formats the SOLUTION."
  (if solution
      (progn
        (format t "~%The proposed menu is as follows: ")
        (format t "~%The proposed main course is: ~S."
                (solution-main-course solution) )
        (format t "~%The proposed appetizer is: ~S."
                (solution-appetizer solution) )
        (format t "~%The proposed dessert is: ~S.~%"
                (solution-dessert solution) )
         )
    (format t "~%No acceptable solution could be found.~%") ) )
    

