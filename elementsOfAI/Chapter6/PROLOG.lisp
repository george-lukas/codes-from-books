;;; PROLOG.CL
;;; A mock PROLOG interpreter

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 6 ("Logical Reasoning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; This program demonstrates goal-driven logical inference
;;; using Horn clauses.

;;; The function PROLOG-UNIFY used here is a modification of
;;; the function UNIFY in UNIFY.CL.  The function PROLOG-ADD-PAIR
;;; given here does not perform the "occurs check" that the
;;; version in UNIFY.CL does.

;;; The local variable GOAL is set by QUERY and used by GOAL-VARIABLE-P.
(let (goal)
;;; The top-level function for this whole program (PROLOG) is QUERY.
  (defun query (new-goal)
    "Tries to prove NEW-GOAL using the facts and rules in *DATABASE*."
    (setf goal new-goal) ; save the goal for use in printing the answers.
    (solve goal nil 1) ) ; perform the backward-chaining search.

  (defun goal-variable-p (v)
    "GOAL-VARIABLE-P returns T if V is a variable in GOAL."
    (and (variablep v) (occurs-in v goal)))
  )

(defvar *database* nil) ; *DATABASE* is a global variable.

;;; SOLVE is the main function for solving a problem.
;;; SUBGOALS is the list of current subgoals.
;;; BINDINGS is a list of all the current bindings.
;;; The LEVEL of recursion is maintained in order
;;; to produce distinct versions of variables so
;;; that several instances of the same rule may
;;; be used simultaneously to solve subgoals at
;;; different levels of recursion.
(defun solve (subgoals bindings level)
  "Attempts to solve all SUBGOALS subject to
   current BINDINGS."
  (if (null subgoals)    ; no remaining subgoals?
      (print-solution bindings); then print solution!
      ;; else keep working...
      (solve1 subgoals bindings *database* level) ) ) 

;;; SOLVE1 is the recursive slave to SOLVE.
;;; CLAUSES is a list of the remaining clauses from the database
;;; to be tried in order to satisfy the current subgoals.
(defun solve1 (subgoals bindings clauses level)
  "Helps SOLVE handle the application of multiple
   database CLAUSES."
  (if (null clauses) nil
      (let* ((this-clause (copy-with-new-vars (first clauses) level))
             (new-bindings
              (prolog-unify
                  (first this-clause) ; trial head
                  (first subgoals) ; current subgoal
                  bindings         ; current bindings
                  level) ))
           (if (not (eql new-bindings 'not-unifiable))
               ;; Head of first clause unified.
               ;; Solve new and old subgoals...
               (solve (append (rest this-clause)
                              (rest subgoals) )
                      new-bindings
                      (1+ level) ) )
           ;; Whether or not head of first clause
           ;; was unified, try other clauses:
           (solve1 subgoals bindings (rest clauses) (1+ level))
            ) ) )

;;; PRINT-SOLUTION prints out selected bindings:
(defun print-solution (bindings)
  "Prints out the bindings of selected variables."
  (format t "solution: ")
  (mapcar  #'print-pair bindings)
  (terpri) )

;;; PRINT-PAIR is a helping function for PRINT-SOLUTION which
;;; prints out a term-variable pair in the form "X=MARY; "
(defun print-pair (p)
  "Prints the TERM/VARIABLE pair if it is of interest."             
  (cond ((variablep (first p)) nil)   ; ignore variable-variable pairs.
        ((goal-variable-p (second p)) ; ignore if not in goal
         (format t "~s=~s; " (get-original (second p)) (first p)) )
        (t nil) ) )


;;; PROLOG-UNIFY is a version of unify that postpones the
;;; substitutions required by SOLVE1 (done by COPY and DO-SUBST)
;;; until after the test for matching predicate symbols, often
;;; avoiding some time-consuming, yet fruitless work.  If the
;;; unification was successful, PROLOG-UNIFY returns the unifier
;;; combined with the previous bindings.
;;; PROLOG-UNIFY calls PROLOG-UNIFY1.
(defun prolog-unify (literal1 literal2 bindings level)
  "Attempts to find a unifier for LITERAL1 and LITERAL2
   subject to BINDINGS."
  ;; make sure predicate symbols match:
  (if (eql (first literal1) (first literal2))
      (catch 'unify
        (prolog-unify1 
                (rest literal1)
                (do-subst (rest literal2) bindings)
                bindings
                level) )
     'not-unifiable) )

;;; PROLOG-ADD-PAIR adds a (term-variable) pair to the front
;;; of the substitution list, after substituting TERM for each
;;; occurrence of VAR in the terms of U.  The new list
;;; of substitutions is returned.  PROLOG-ADD-PAIR is
;;; similar to the function ADD-PAIR in UNIFY.CL, except that,
;;; to improve efficiency, no "occurs check" is made.
(defun prolog-add-pair (term variable u)
  "Adds a new TERM/VARIABLE pair to the list U after
   applying the substitution to U."
  (cons (list term variable) 
        (subst term variable u) ) )

;;; The recursive function PROLOG-UNIFY1 unifies the two lists
;;; of terms, TERMLIST1 and TERMLIST2, and in the course of
;;; doing so adds more pairs onto the unifier U, returning
;;; the resulting list of bindings.
(defun prolog-unify1 (termlist1 termlist2 u level)
  "Handles the real work of PROLOG-UNIFY.  Similar to UNIFY1."
  (cond
    ;; If equal, no substitution necessary:
    ((equal termlist1 termlist2) u)
    ;; Check for list length mismatch (a syntax error):
    ((or (null termlist1) (null termlist2))
     (throw 'unify 'not-unifiable) )
    ;; If TERMLIST1 is a variable, try to add a substitution:
    ((variablep termlist1)
     (prolog-add-pair termlist2 termlist1 u) )
    ;; Handle the case when TERMLIST2 is a variable similarly:
    ((variablep termlist2)
     (prolog-add-pair termlist1 termlist2 u) )
    ;; Now, if either expression is atomic, it is a
    ; constant and there's no match since they're not equal:
    ((or (atom termlist1) (atom termlist2))
     (throw 'unify 'not-unifiable) )
    ;; The expressions must be non-atomic; do recursively.
    ;; Apply current substitutions before unifying the CARs.
    (t (setf u (prolog-unify1
                   (do-subst (first termlist1) u)
                   (do-subst (first termlist2) u) 
                   u
                   level) )
       ; Now unify the CDRs.
       (prolog-unify1 (rest termlist1)
                      (rest termlist2)
                      u
                      level) )
       ) )


;;; The function COPY-WITH-NEW-VARS replaces all the variables in L
;;; by new variables, thus assuring that the same clause may
;;; be used in different ways at different levels of recursion.
;;; COPY-WITH-NEW-VARS is supported by GET-VERSION-OF-VARIABLE.
(defun copy-with-new-vars (l level)
  "Returns a new copy of clause (list) L with fresh
   instances of variables."
  (cond ((null l) nil)
        ((atom l)
         (if (variablep l)
             (get-version-of-variable l level)
             l) )
        (t (cons (copy-with-new-vars (first l) level)
                 (copy-with-new-vars (rest l) level) )) ) )

;;; GET-VERSION-OF-VARIABLE creates a new "version" of variable V
;;; for the current LEVEL, or it retrieves it if already created.
(defun get-version-of-variable (v level)
  "Return a fresh version of variable V using level, unless 
   one already exists; in that case return it."
  (let ((v1 (get-instance v level)))
       (if v1 v1
           (let ((v1 (gensym (symbol-name v))))
                (set-new-version v1 v level)
                v1) ) ) )


;;; Here we declare certain symbols to be variables.
;;; Others are assumed to be functions or constants.
(defvar *known-variables*
  '(u v w x y z wine entree lst lst2 r) )

;;; We use a hash table to store information about all the
;;; original variables and synthesized variables.
(let ((variables-info (make-hash-table :size 20 :test #'equal)))
  (defun set-new-version (newvar oldvar level)
    "Stores the original variable and level for a new variable,
     and stores the new variable for the original and level."
    (setf (gethash newvar variables-info)
          (cons oldvar level) ) ; store old variable and level.
    (setf (gethash (cons oldvar level) variables-info)
          newvar) ) ; store new variable.
  (defun get-original (v)
    "Returns the original variable for V."
    (first (gethash v variables-info)) )
  (defun get-instance (v level)
    "Returns the variable instance given level and V."
    (gethash (cons v level) variables-info) )
 )

;;; For each known variable, register its "original" as itself,
;;; and set its "level" to zero.
(mapcar
  #'(lambda (v) (set-new-version v v 0))
  *known-variables*)

;;; The function VARIABLEP supports both UNIFY1 and COPY-WITH-NEW-VARS.
;;; This version (unlike that given in UNIFY.CL)
;;; supports the two examples given below.
(defun variablep (x)
  "Returns True if X is a known variable or a created
   instance of one, i.e., if it is registered in the hash table."
  (or (member x *known-variables*) (get-original x)) )

;;; ------------------------------------------------------------
;;; The following are from UNIFY.CL:

;;; DO-SUBST performs all substitutions in L on EXP in
;;; reverse order.
(defun do-subst (exp l)
  "Performs all substitutions of L on EXP."
  (cond ((null l) exp)
        (t (subst (first (first l))
                  (second (first l))
                  (do-subst exp (rest l)) )) ) )

;;; OCCURS-IN returns T if ELT occurs in EXP at any level.
(defun occurs-in (elt exp)
  "Returns True if ELT occurs in EXP at any level."
  (cond ((eql elt exp) t)
        ((atom exp) nil)
        (t (or (occurs-in elt (first exp))
               (occurs-in elt (rest exp)) )) ) )


;;; end of definitions from UNIFY.CL
;;; ------------------------------------------------------------

;;;----------------------------------------------
;;; Let us now give two examples:
;;;----------------------------------------------

;;; database of clauses for example 1:
(setf database1 '(
  ((grandson x y) (son x z) (parent y z))
  ((son walter martha))
  ((parent jonathan martha))
 ))

;;; database of clauses for example 2:
(setf database2 '(
  ((redwine beaujolais))
  ((redwine burgundy))
  ((redwine merlot))

  ((whitewine chardonnay))
  ((whitewine riesling))

  ((meat steak))
  ((meat lamb))

  ((fish salmon))

  ((goodwine wine) (maincourse entree) (meat entree) (redwine wine))
  ((goodwine wine) (maincourse entree) (fish entree) (whitewine wine))

  ((maincourse salmon))
 ))

;;; Now demonstrate sample inferences with tracing turned on:
(trace solve prolog-unify print-solution)

(setf *database* database1)  ; Use the database for example 1.
;;; Who is the grandson of jonathan?
(query '((grandson w jonathan)))

(setf *database* database2)  ; Now use the database for example 2.
;;; What is a good wine for dinner tonight?
(query '((goodwine wine)))

;;; Find combinations of a red wine with a meat entree...
(query '((redwine wine) (meat entree)))

