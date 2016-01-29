;;; SHELL.CL 
;;; a mini shell for forward-chaining and
;;; backward-chaining production systems.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 5 ("Search") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; This shell allows the use of certainty factors, and it
;;; supports a popular approach to the design of expert systems.

;;; This file includes contains the matching function MATCHS
;;; and does not use the file MATCH.CL.

(defparameter *threshold-for-assertion* 0.2)

;;; ------RULE REPRESENTATION------
;;; 
;;; The user program describes each production rule
;;; as a 7-tuple of the form,
;;; (rule-name IF condition THEN action WITH-CERTAINTY certainty-formula)
;;; The condition may be a simple tuple representing a fact and
;;; certainty value, or it may be a conjunction of simple conditions
;;; (a simple condition has no embedded conditions).
;;; e.g., (AND cond1 cond2 ... condK) is an acceptable form for a condition,
;;; where cond1, etc., do not contain any ANDs ORs or NOTs.
;;; The SHELL program stores the rules as 4-component structures.

;;; example rule: "If the painting texture is dots
;;;                then the style is pointillism
;;;                with certainty 0.8"
(defstruct rule
  name condition action cf)

;;; For simplicity, we keep certain information in global variables
;;; for this application.  This practice can be dangerous in the
;;; context of large multi-programmer Lisp projects.
(defvar *rules*)  ; the rule base.
(defvar *facts*)  ; the list of facts and their certainty values.

(defun shell-init ()
  "Initializes the rules and facts for the SHELL."
  (setf *rules* nil)
  (setf *facts* nil)
  (new-hash-table) )

(let (hashtable)
  (defun new-hash-table ()
    "Initializes session-dependent, dynamic information."
    (setf hashtable (make-hash-table :size 20 :test #'equal)) )
  (defun mark-rule-as-used (rulename)
    "Remembers so that user will not be asked the
     same questions more than once in a session."
    (setf (gethash rulename hashtable) t) )
  (defun rule-used-p (rulename)
    "Recalls whether a rule has been used."
    (gethash rulename hashtable) )
  (defun mark-fact-from-user (fact rulename)
    "Saves justification for use in proof."
    (setf (gethash fact hashtable) rulename) )
  (defun fact-from-user-p (fact)
    "Recalls whether the fact came from the user."
    (gethash fact hashtable) ) )


;;; DEFINE-RULE does a little bit of syntax checking.
;;; Then it creates a structure to store the rule and
;;; pushes the structure onto the list *RULES*.
(defun define-rule (rule-desc)
  "Sets up a rule in the collection of rules."
  (cond ((< (length rule-desc) 7)
         (format t "~%Too few items in rule: ~A" rule-desc) )
        ((> (length rule-desc) 7)
         (format t "~%Too many items in rule: ~A" rule-desc) )
        ((not (eql (second rule-desc) 'IF))
         (format t "~%2nd element is not IF in rule: ~A" rule-desc) )
        ((not (eql (fourth rule-desc) 'THEN))
         (format t "~%4th element is not THEN in rule: ~A" rule-desc) )
        (t (setf *rules*  ; Place the rule on the global list.
             (append 
               (list
                 (make-rule
                   :name (first rule-desc)
                   :condition (third rule-desc)
                   :action (fifth rule-desc)
                   :cf (seventh rule-desc) ) )
               *rules*) )
         ) ) )

;;; See if FACT is a new fact, and if so, add it to *FACTS*.
;;; If not new, but new certainty factor is larger than old,
;;; register the new certainty factor.
;;; If no changes are actually made to *FACTS*, return NIL.
(defun assert-fact (fact)
  "Adds a new fact or updates certainty on an old fact."
  (format t "~%asserting or reasserting fact:~%  ~A" fact)
  (let ((temp (member fact *facts* :test #'fact-equal)))
    (if temp
        (let* ((old-fact (first temp))
               (new-cf (fact-cf fact))
               (old-cf (fact-cf old-fact)) )
          (if (> new-cf old-cf)
              (rplaca (last old-fact) new-cf)
            nil) )
      (push fact *facts*) ) ) )

(defun fact-cf (fact)
  "Returns the certainty-factor associated with FACT."
  (first (last fact)) )

;;; Test equality of two facts ignoring the certainty factor.
(defun fact-equal (f1 f2)
  "Returns T if F1 equals F2 modulo certainty factors."
  (equal (butlast f1)(butlast f2)) )

;;; Create a fact according to the action part of a rule.
(defun make-fact (rule bindings)
  "Creates a new fact by applying RULE using BINDINGS."
  (let ((the-fact 
        (append
          ; Substitute the binding for each occurrence of
          ; a designated symbol in the action...
          (subst-bindings (rule-action rule) bindings)
          ; Substitute the certainty values in the formula and
          ; then evaluate...
          (list (eval (subst-bindings
                        (rule-cf rule)
                        bindings))) ) ) )
    ; (format t "~%creating fact: ~A" the-fact)
    the-fact) )

(defconstant *end* '((:yes . :yes)))

;;; SUBST-BINDINGS performs all the subtitutions in BINDINGS
;;; at all levels of EXPRESSION.
(defun subst-bindings (expression bindings)
  "Returns result of applying BINDINGS to EXPRESSION."
  (if (null bindings)
      expression
    (subst1 (cdar bindings) ; a value to replace:
            (caar bindings) ; a symbol
            (subst-bindings expression
                            (rest bindings) ) )) )

(defun subst1 (new old exp)
  "Any and all embeddings in EXP of the form (? OLD) are
   replaced by NEW."
  (cond ((atom exp) exp)
        ((and (= (length exp) 2)
              (eql (first exp) '?)
              (equal (second exp) old) )
         new)
        (t (cons (subst1 new old (first exp))
                 (subst1 new old (rest exp)) )) ) )

;;; Functions for manipulating certainty values:
(defun square (x)
  "Returns the square of X."
  (* x x))

(defun compl (x)
  "Returns the complement of x, i.e., 1 - x."
  (- 1.0 x) )

(defun and-indep (&rest cfs)
  "Returns the fuzzy AND, treating args as independent probabilities."
  (apply #'* cfs) )

(defun or-indep (&rest cfs)
  "Takes the fuzzy OR, treating args as independent probabilities."
  (compl (apply #'* (mapcar #'compl cfs))) )

(defun strengthen (cf)
  "Intensifies the certainty."
  (compl (square (compl cf))) )

;;; ------ Support for rules that QUERY the user begins here ------

(defun query-rule-p (rule)
  "Returns T if last character in name of rule is a question mark."
  (if (char= (last-char (rule-name rule)) #\?) t nil) )

(defun last-char (sym)
  "Returns last character of the name of symbol SYM."
  (let ((s (string sym)))
    (char s (1- (length s))) ) )

(defun handle-query (rule)
  "Handles the querying process using info from the action part of RULE."
  (if (rule-used-p (rule-name rule))
      nil ; Do not ask the same questions again in this session.
    (progn
      (mark-rule-as-used (rule-name rule))
      (let ((rule-action (rule-action rule)))
        (query-user (first rule-action)
                    (second rule-action)
                    (third rule-action)
                    (rule-name rule) ) ) ) ) )

(defun query-user (question fact-template1 fact-template2 rulename)
  "Prints the string, reads certainty factors
   from the terminal, and asserts facts."
  (let (cf)
    (format t "~%")
    (format t question) ; Ask the question from the rule.
    (format t "~%Enter a number in the range 0.0 (no) to 1.0 (yes). -> ")
    (loop (if (certainty-value-p (setf cf (read))) (return cf)
      (format t "~%Please enter a number in the range 0.0 to 1.0. -> ") ))
    (assert-fact (append fact-template1 (list cf)))
    (mark-fact-from-user fact-template1 rulename)
    (format t "~%How certain are you that your assessment is correct? -> ")
    (loop (if (certainty-value-p (setf cf (read))) (return cf)
    (format t "~%Please enter a number in the range 0.0 to 1.0. -> ") ))
    (assert-fact (append fact-template2 (list cf)))
    (mark-fact-from-user fact-template2 rulename)
    )
  t )

(defun certainty-value-p (s)
  "Returns T if S is a number in the range 0.0 to 1.0."
  (and (numberp s) (>= s 0.0) (<= s 1.0)) )

;;; ----- Support for FORWARD CHAINING begins here ---------

;;; The control scheme tries rules until one succeeds, then starts again;
;;; when no rules fire, FORWARD-CHAIN returns.
;;; Thus this implements a kind of forward-chaining system.
(defun forward-chain ()
  "Top level control loop for forward chaining rule-based system."
  (loop (cond ((not (try-rules *rules* *facts*))
               (return (format t "~%Inferences now complete.")) ))) )

;;; TRY-RULES tries each rule in RULES-LEFT
;;; on the list of facts until one succeeds,
;;; or the end of list is reached.
;;; If a rule succeeds, a notice is printed;
;;; otherwise, NIL is returned.
(defun try-rules (rules-left facts)
  "Successively tries rules on RULES-LEFT until one succeeds or
   all of them fail to generate anything new."
  (cond ((null rules-left) nil)
        (t (let ((temp (try-rule (first rules-left) facts)))
             (if temp t
               (try-rules (rest rules-left) facts) ) )) ) )

;;; TRY-RULE tries to apply a rule to the current list of facts.
;;; If it cannot apply a rule, NIL is returned.
(defun try-rule (rule facts)
  "Tries to apply RULE to FACTS.
   Returns NIL if nothing new can be generated."
  (let (results) ;; note this line has been corrected. SLT 14 Feb 96.
    (cond ((and (eql (first (rule-condition rule)) 'AND)
                (setf results (match-all-conditions
			        (rest (rule-condition rule))
                                nil
                                facts) ) )
           (fire rule results) )
          ((setf results (match-one-condition
			  (rule-condition rule)
			  nil
			  facts))
           (fire rule results) )
          (t nil) ) ) )

;;; MATCH-ONE-CONDITION
(defun match-one-condition (condition bindings facts)
  "Tries to find a fact that matches the given condition.
   If it matches, returns the list of bindings, otherwise returns NIL."
  (match-one-cond1 condition facts bindings) )

(defun match-one-cond1 (condition fact-list bindings)
  "Tries to find a matching fact from the given FACT-LIST."
  (if (null fact-list) nil   ; if no more facts, matching fails.
    (let ((results (matchs condition (first fact-list) *end*)))
      (if results
          ;;; Append bindings from previous subconditions
          ;;; to bindings for this one:
          (merge-bindings results bindings)
        (match-one-cond1 condition (rest fact-list) bindings) ) ) ) )

(defun match-all-conditions (cond-list bindings facts)
  "Tries to find facts matching all patterns on COND-LIST.
   For each pattern on COND-LIST, finds a matching fact,
   and returns a list of all the bindings.
   If any pattern has no matching fact, returns NIL."
  (if (null cond-list) bindings
    (let ((results (match-one-condition (first cond-list) bindings facts)))
      (if results
          (match-all-conditions (rest cond-list) results facts)
        nil) ) ) )

(defun merge-bindings (bindings1 bindings2)
  "Merges two lists of bindings.
   Does not check for multiple attempts to
   bind the same variable."
  (append (butlast bindings1) bindings2) )

;;; The function FIRE is evaluated when a production rule fires.
;;; FIRE returns T if a new fact was added or one's certainty increased.
(defun fire (rule bindings)
  "Prints a notice that rule fires,
   and takes action according to RULE."
  ;; print message with name of rule:
  (format t "~%~a fires." (rule-name rule))
  (if (query-rule-p rule)
      ;; Handle rules involving queries specially:
      (handle-query rule)
    ;; Otherwise, create the new fact according to the rule,
    ;; and assert it if its certainty value is large enough:
    (let* ((fact (make-fact rule bindings))
           (cf (fact-cf fact)) )
      (if (>= cf *threshold-for-assertion*)
          (assert-fact (make-fact rule bindings)) ; assert the new fact.
        nil) ) ) )

;;; ----- Support for BACKWARD CHAINING begins here ---------

;;; A proof is a list of the form (STRUC-INFO FACT)
;;; where FACT is a list ending in a certainty factor.

;;; Here is the main backward-chaining function.
;;; GOAL should be a fact without the certainty factor.
(defun backward-chain (goal)
  "Performs backward chaining to try to prove GOAL."
  (assert (listp goal) (goal) "Argument is not a valid goal assertion.")
  (let (fact result rule bindings)
    (cond
      ;; If goal is an asserted fact, then success:
      ((setf fact (find-fact goal))
       (list 'asserted (fact-from-user-p fact) fact) )
      ;; If goal is implied by some rule,
      ;; try to prove condition(s) for the rule:
      ((setf result (match-rhs goal))
       (setf rule (first result))
       (setf bindings (second result))
       (prove-all-subgoals rule bindings) )
      ;; Otherwise, no case can be made:
      (t nil) ) ) )

(defun find-fact (subgoal)
  "Returns the fact (with certainty factor)
   associated with a goal."
  (first (member subgoal *facts* :test #'find-fact-test)) )

;;; Helping function for FIND-FACT.
(defun find-fact-test (goal fact)
  "Works like EQUAL but ignores certainty factor
   on 2nd argument."
  (equal goal (butlast fact)) )

(defun match-rhs (goal)
  "Finds a rule whose right-hand-side matches GOAL.
   In case of a query rule, either assertion on rhs can match.
   If successful, returns a list of the form (RULE BINDINGS)."
  (match-rhs1 *rules* goal) )

(defun match-rhs1 (rules goal)
  "Helping function for MATCH-RHS, and takes a specified list of rules."
  (let (temp)
    (cond ((null rules) nil)
          ((setf temp (match-one-rhs (first rules) goal))
           (list (first rules) temp) )
          (t (match-rhs1 (rest rules) goal)) ) ) )

(defun match-one-rhs (rule goal)
  "Determines if the goal matches the rhs of rule or one assertion in
   case of a query rule.  If succesful, then returns bindings."
  (if (query-rule-p rule)
      (let ((bindings1
              (matchs (query-rule-assertion-1 rule) goal *end*) )
            (bindings2
              (matchs (query-rule-assertion-2 rule) goal *end*) ) )
        (cond (bindings1 bindings1)
              (bindings2 bindings2)
              (t nil) ) )
      (matchs (rule-action rule) goal *end*) ) )

(defun query-rule-assertion-1 (query-rule)
  "Returns the first assertion on RHS of query rule."
  (second (rule-action query-rule)) )

(defun query-rule-assertion-2 (query-rule)
  "Returns the second assertion on RHS of query rule."
  (third  (rule-action query-rule)) )

(defun prove-all-subgoals (rule rhs-bindings)
  "Tries to prove all the conditions of this rule.
   Applies RHS-BINDINGS to the conditions before
   trying to prove them. Returns a proof or NIL."
  (let* ((rule-cond (rule-condition rule))
         (unbound-conds (if (eql (first rule-cond) 'and)
                            (rest rule-cond)
                          (list rule-cond) ))
         (conditions
           (mapcar
             #'(lambda (x) (subst-bindings x rhs-bindings))
             unbound-conds) )
         (subgoal-proofs nil)
         (bindings *end*)
         subgoal)
    (dolist (condition conditions)
      ;; For each of the conditions in RULE:
      (setf subgoal (butlast condition))
            ;; Ignore certainty factor here.
      (format t "~%Considering subgoal: ~S." subgoal)
      (let ((subgoal-proof
            (backward-chain subgoal) ))
            ;; Recursively prove SUBGOAL.
        (if subgoal-proof
          (progn
            (push subgoal-proof subgoal-proofs)
            ;; Determine bindings consistent with
            ;; all subgoals so far...
            (if (null bindings)
              (return-from prove-all-subgoals nil) )
            (setf bindings
                  (matchs condition
                          (third subgoal-proof)
                          bindings) ) )
          ;;; If subgoal fails, exit...
          (return-from prove-all-subgoals
                       (setf subgoal-proofs nil) ) ) ) )
    ;; If overall proof was successful, combine partial
    ;; proofs and compute certainty factors.
    (if subgoal-proofs
      ;; If this is a query rule and subgoals have 
      ;; been satisfied, then go ahead and ask the
      ;; user to answer the questions.
      (if (query-rule-p rule)
        (progn (handle-query rule)
               (make-q-proof subgoal-proofs rule) )
      (combine-proofs subgoal-proofs rule bindings) )
    nil) ) )

(defun make-q-proof (subgoal-proofs rule)
  "Returns a proof for the results of a query rule."
  (list subgoal-proofs
        rule
       (find-fact (query-rule-assertion-1 rule)) ) )

(defun combine-proofs (subgoal-proofs rule bindings)
  "Returns a list of the proofs, and the proved statement.
   It must compute the certainty factor."
  (list
    subgoal-proofs
    rule
    (append
      (subst-bindings (rule-action rule) bindings)
      (list (eval (subst-bindings
                    (rule-cf rule)
                    bindings))) ) ) )

(defun explain-proof (proof)
  "Prints out a description of PROOF."
  (explain-proof1 proof 0) )

(defun explain-proof1 (proof level)
  "Helping function for EXPLAIN-PROOF."
  (dotimes (i level)
    (format t "  ") )
  (cond
    ((eql (first proof) 'asserted)
     (if (fact-from-user-p (butlast (third proof)))
         (format t "Fact entered by user: ~S.~%"
                 (butlast (third proof)) )
       (format t "Given fact: ~S.~%"
               (butlast (third proof)) ) ) )
    (t
      (if (zerop level)
        (format t "~%PROOF FOR GOAL:")
        (format t "Proof for subgoal:") )
      (format t " ~S~%"
        (butlast (third proof)) )
      (dolist (subgoal-proof (first proof))
        (explain-proof1 subgoal-proof (1+ level)) )
      (dotimes (i level)
        (format t "  ") )
      (if (query-rule-p (second proof))
          (format t "Using question rule")
          (format t "Using rule") )
      (format t " ~S,~%"
        (rule-name (second proof)) )
      (dotimes (i level)
        (format t "  ") )
      (format t "Goal ~S is now proved with certainty ~5,3f.~%"
        (butlast (third proof))
        (first (last (third proof))) ) ) ) )

;;; ----- End of section for backward chaining ------


;;; Special matcher for this expert system shell:

;;; (MATCHS P1 P2 B) returns an association list of bindings
;;; e.g., ((X . 5) (Y A SEQUENCE OF ELTS) (:YES . :YES)),
;;; that represents the pairings of variables of either P1 or P2 with
;;; components of the other that puts P1 into correspondence with P2.
;;; The substitution list always ends with (:YES . :YES)
;;; which represents an empty substitution. The presence of this
;;; empty substitution indicates that the match was successful.
;;; If matching is unsuccessful, NIL is returned.
;;; One difference from MATCH is in the taking of an extra input
;;; argument, B, which is a list of bindings that the current matching
;;; must be consistent with. Another difference is in the
;;; handling of matching with restrictive predicates.
;;; See the comments in the code below.

(defun matchs (p1 p2 b)
  "Attempts to find a correspondence between P1 and P2, utilizing
   any special constructs appearing in either P1 or P2.  Returns an association
   list of bindings if successful; NIL otherwise."
  (cond
    ((handle-both-null p1 p2 b))
    ((and p1 (atom p1)) nil)
    ((and p2 (atom p2)) nil)
    ((handle-normal-recursion p1 p2 b))
    ((handle-?-with-consistency-check p1 p2 b))
    (t nil) ) )

(defun 1st-pattern-op (p)
  "Return the ?, if any, in the first pattern
   construct of P."
  (if (consp (first p)) (first (first p)) nil) )

(defun 1st-pattern-variable (p)
  "Return the variable in the first pattern
   construct of P."
  (first (rest (first p))) ) ; same as (CADAR P)

(defun handle-both-null (p1 p2 b)
  "Test for and handle case when both P1 and P2
   are null."
  (if (and (null p1)(null p2)) b) )

(defun handle-normal-recursion (p1 p2 b)
  "Test for and handle case when the first
   elements of P1 and p2 are EQL."
  (if (atom (first p1))
      (if (eql (first p1)(first p2))
          (matchs (rest p1)(rest p2) b) ) ) )

(defun handle-?-with-consistency-check (p1 p2 b)
  "Test for and handle the case when either (FIRST P1) or (FIRST P2) is of
   the form (? X)."
  (cond ((null p2) nil) ;; P2 must not be null
      ((null p1) nil)   ;; P1 " "
      ((eql (1st-pattern-op p1) '?)
          (let ((rest-match (matchs (rest p1)(rest p2) b))
                (pattern-variable (1st-pattern-variable p1)) )
            ;; Here we check for consistency of bindings:
            ;; Any attempt to bind the same variable to two
            ;; different values causes MATCHS to fail...
            (if rest-match
                (if (new-variable pattern-variable rest-match)
                    (acons pattern-variable
                           (first p2)
                           rest-match)
                    (if (consistent pattern-variable
                                    (first p2)
                                    rest-match)
                        rest-match) ) ) ) )
      ((eql (1st-pattern-op p2) '?)
          (let ((rest-match (matchs (rest p1)(rest p2) b))
                (pattern-variable (1st-pattern-variable p2)) )
            ;; Here we check for consistency of bindings:
            ;; Any attempt to bind the same variable to two
            ;; different values causes MATCHS to fail...
            (if rest-match
                (if (new-variable pattern-variable rest-match)
                    (acons pattern-variable
                           (first p1)
                           rest-match)
                    (if (consistent pattern-variable
                                    (first p1)
                                    rest-match)
                        rest-match) ) ) ) )
     (t nil)
  ) )


;;; The following four functions are helping functions for MATCHS:

(defun new-variable (v bindings)
  "Return T if V is not bound in BINDINGS."
  (not (assoc v bindings)) )

(defun consistent (var value bindings)
  "Return T if the value of VAR in BINDINGS is VALUE."
  (equalp value (val var bindings)) )

(defun append-bindings (binding-list1 binding-list2)
  "Return concatenated bindings.
   Don't repeat the (:YES . :YES) entry.
   Does not check for consistency of bindings."
  (append (butlast binding-list1) binding-list2) )

;;; VAL provides convenient access to something matched by
;;; a variable after matching with MATCHS.
(defun val (variable alist)
  "The function VAL returns the value associated
   with VARIABLE on ALIST."
  (rest (assoc variable alist)) )
