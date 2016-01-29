;;; MATCHL.CL -- A variation of MATCH with enhancements
;;; for use in LEIBNIZ.CL.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 3 ("Productions Systems
;;; and Pattern Matching") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; This version incorporates a means to permit matching at levels
;;; other than top level and still combine the bindings.
;;; Checks consistency of bindings to each variable for the (? X) option.
;;; Also tests for S being atomic and not NIL and fails on that.

;;; Like MATCH, (MATCHL P S) returns an association list of bindings
;;; e.g., ((X . 5) (Y A SEQUENCE OF ELTS) (:YES . :YES)),
;;; that represents the pairings of variables of P with
;;; components of S that put P into correspondence with S.
;;; The substitution list always ends with (:YES . :YES)
;;; which represents an empty substitution. The presence of this
;;; empty substitution indicates that the match was successful.
;;; If matching is unsuccessful, NIL is returned.
;;; The difference from MATCH is in the handling of matching with
;;; restrictive predicates.  See the comments in the code below.

(defun matchl (p s)
  "Attempt to find a correspondence between P and S, utilizing
   any special constructs appearing in P.  Return an association
   list of bindings if successful; NIL otherwise.
   This is an enhanced version of MATCH for LEIBNIZ.CL."
  (cond
    ((handle-both-null p s))
    ((and s (atom s)) nil)
    ((handle-normal-recursion p s))
    ((atom (first p)) nil)
    ((handle-?-with-consistency-check p s))
    ((handle-* p s))
    ((handle-pred-with-bindings p s))
    (t nil) ) )

(defun 1st-pattern-op (p)
  "Return the *, ? or predicate in the first pattern
   construct of P."
  (first (first p)) ) ; same as (CAAR P)

(defun 1st-pattern-variable (p)
  "Return the variable in the first pattern
   construct of P."
  (first (rest (first p))) ) ; same as (CADAR P)

(defun handle-both-null (p s)
  "Test for and handle case when both P and S
   are null."
  (if (and (null p)(null s))
      '((:yes . :yes)) ) )

(defun handle-normal-recursion (p s)
  "Test for and handle case when the first
   elements of P and S are EQL."
  (if (atom (first p))
      (if (eql (first p)(first s))
          (matchl (rest p)(rest s)) ) ) )

(defun handle-?-with-consistency-check (p s)
  "Test for and handle the case when (FIRST P) is of
   the form (? X)."
  (if s ; S must not be null
      (if (eql (1st-pattern-op p) '?)
          (let ((rest-match (matchl (rest p)(rest s)))
                (pattern-variable (1st-pattern-variable p)) )
            ;; Here we check for consistency of bindings:
            ;; Any attempt to bind the same variable to two
            ;; different values causes MATCHL to fail...
            (if rest-match
                (if (new-variable pattern-variable rest-match)
                    (acons pattern-variable
                           (first s)
                           rest-match)
                    (if (consistent pattern-variable
                                    (first s)
                                    rest-match)
                        rest-match) ) ) ) ) ) )

(defun handle-* (p s)
  "Test for and handle the case when (FIRST P) is of
   the form (* X)."
  (if (eql (1st-pattern-op p) '*)
      (let ((pattern-variable
              (1st-pattern-variable p) )
            (rest-match nil) )
        (cond ; subcase 1 --match 1 element of S:
              ((and s
                    (setf rest-match
                          (matchl (rest p)
                                  (rest s) ) ) )
               (acons pattern-variable
                      (list (first s))
                      rest-match) )

              ; subcase 2 --match no elements of S:
              ((setf rest-match (matchl (rest p) s))
               (acons pattern-variable
                      nil
                      rest-match) )

              ; subcase 3 --match more than 1 elt of S:
              ((and s
                    (setf rest-match
                          (matchl p (rest s)) ) )
               (acons pattern-variable
                      (cons (first s)
                            (val pattern-variable
                                 rest-match) )
                      (rest rest-match)) )
              (t nil) ) ) ) )

;;; The main difference between MATCH and MATCHL is in the following
;;; portion for handling matching with restrictive predicates.
;;; If the result of applying the predicate is a list of bindings,
;;; then those bindings are appended in with the answer.
(defun handle-pred-with-bindings (p s)
  "Handle case when (FIRST P) is of the form (PREDICATE X)."
  (if s ; S must not be null
    (if (member (1st-pattern-op p)
                '(? *))            ; Don't apply '? or '*.
        nil
      (let ((apply-result (apply (1st-pattern-op p)
                                 (list (first s)) )))
        (if apply-result
          (let ((rest-match (matchl (rest p) (rest s)))
                (pattern-variable
                  (1st-pattern-variable p) ) )
            (if rest-match
              (acons pattern-variable
                     (first s)
                     (if (match-result-p apply-result)
                         (append-bindings apply-result
                                          rest-match)
                       rest-match) ) ) ) ) ) ) ) )


;;; The following four functions are helping functions for MATCHL:

(defun new-variable (v bindings)
  "Return T if V is not bound in BINDINGS."
  (not (assoc v bindings)) )

(defun consistent (var value bindings)
  "Return T if the value of VAR in BINDINGS is VALUE."
  (equalp value (val var bindings)) )

(defun match-result-p (exp)
  "Return T if EXP is a list of bindings of the form
   created by MATCH and MATCHL.  Otherwise return NIL."
  (and (listp exp)
    (equalp (first (last exp)) '(:yes . :yes)) ) )

(defun append-bindings (binding-list1 binding-list2)
  "Return concatenated bindings.
   Don't repeat the (:YES . :YES) entry.
   Does not check for consistency of bindings."
  (append (butlast binding-list1) binding-list2) )

;;; VAL provides convenient access to something matched by
;;; a variable after matching with MATCH.
(defun val (variable alist)
  "The function VAL returns the value associated
   with VARIABLE on ALIST."
  (rest (assoc variable alist)) )
