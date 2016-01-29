;;; MATCH.CL -- a recursive pattern-matching function
;;; for use in production-systems programming.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 3 ("Productions Systems
;;; and Pattern Matching") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; (MATCH P S) returns an association list of bindings
;;; e.g., ((X . 5) (Y A SEQUENCE OF ELTS) (:YES . :YES)),
;;; that represents the pairings of variables of P with
;;; components of S that put P into correspondence with S.
;;; The substitution list always ends with (:YES . :YES)
;;; which represents an empty substitution. The presence of this
;;; empty substitution indicates that the match was successful.
;;; If matching is unsuccessful, NIL is returned.

(defun match (p s)
  "Attempt to find a correspondence between P and S, utilizing
   any special constructs appearing in P.  Return an association
   list of bindings if successful; NIL otherwise."
  (cond
    ((handle-both-null p s))
    ((handle-normal-recursion p s))
    ((atom (first p)) nil)
    ((handle-? p s))
    ((handle-* p s))
    ((handle-restrict-pred p s))
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
          (match (rest p)(rest s)) ) ) )

(defun handle-? (p s)
  "Test for and handle the case when (FIRST P) is of
   the form (? X)."
  (if s ; S must not be null
      (if (eql (1st-pattern-op p) '?)
          (let ((rest-match
                  (match (rest p)(rest s)) ))
            (if rest-match
                (acons 
                  (1st-pattern-variable p)
                  (first s)
                  rest-match) ) ) ) ) )

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
                          (match (rest p)
                                 (rest s) ) ) )
               (acons pattern-variable
                      (list (first s))
                      rest-match) )

              ; subcase 2 --match no elements of S:
              ((setf rest-match (match (rest p) s))
               (acons pattern-variable
                      nil
                      rest-match) )

              ; subcase 3 --match more than 1 elt of S:
              ((and s
                    (setf rest-match
                          (match p (rest s)) ) )
               (acons pattern-variable
                      (cons (first s)
                            (val pattern-variable
                                 rest-match) )
                      (rest rest-match)) )
              (t nil) ) ) ) )

(defun handle-restrict-pred (p s)
  "Handle case when (FIRST P) is of the form
   (PREDICATE X)."
  (if s ; S must not be null
    (if (member (1st-pattern-op p)
                '(? *) ) ; Don't apply '? or '*.
        nil
      (if (apply (1st-pattern-op p)
                 (list (first s)) )
          (let ((rest-match
                  (match (rest p) (rest s)) )
                (pattern-variable
                  (1st-pattern-variable p) ) )
            (if rest-match
                (acons pattern-variable
                       (first s)
                       rest-match) ) ) ) ) ) )

;;; The function VAL provides convenient access to
;;; something matched by a variable after matching
;;; with MATCH.
(defun val (variable alist)
  "Return the value associated with VARIABLE
   on ALIST."
  (rest (assoc variable alist)) )
