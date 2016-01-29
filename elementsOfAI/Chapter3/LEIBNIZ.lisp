;;; LEIBNIZ.CL - production system for symbolic differentiation

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 3 ("Productions Systems
;;; and Pattern Matching") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

; sample formula:
;
;  d     2
;  --   x  + 2x
;  dx

; represented by f0 as follows:
(setf  f0 '(d (+ (exp x 2) (* 2 x)) x))

;;; Each production rule is represented as a 4-component structure.
(defstruct rule
  "This structure definition defines a RULE as having four parts."
  goal    ; tells which current goal this rule is applicable for.
  pattern ; the main part of the rule's condition.
  action  ; the right-hand side of the rule.
  name)   ; a name identifying the rule in printouts.


; example rule: "If the current goal is differentiate and there
;  is a subformula of the form (d (sin x) x) then replace
;  the subformula by (cos x); this is called diff-sin-rule."

; another rule:
;  "If the current goal is differentiate and there is a
;  subformula of the form (d (+ e1 e2) v1) then replace
;  the subformula by one of the form
;  (+ (d e1 v1) (d e2 v1)); this is called diff-sum-rule."

;;; The following function makes rule definition more convenient:
(defun defrule (r)
  "Simplifies the defining of new rules."
  (push (make-rule :goal (first r)
                   :pattern (second r)
                   :action (third r)
                   :name (fourth r))
        *rules*) )

;;;; Here are the definitions of the rules:
(defvar *rules* nil
  "This global variable is used to hold the production rules.")

; rule placed after all the rules of differentiation - causes the
; current goal to be changed to simplify:
(defrule '(
  differentiate
  ((* formula))
  (progn (setf *current-goal* 'simplify) 'formula)
  goal-change-rule
  ) )

(defrule '(
  differentiate
  (d ((lambda (f) (matchl '(+ (? e1) (? e2)) f)) e3)
     (? v1) )
  '(+ (d e1 v1) (d e2 v1))
  diff-sum-rule
  ) )
; Here the lambda expression is embedded in the pattern which
; matches the derivative-of-sum forms.  This local function is
; used to restrict what can match in the second position of the
; list.  The local function itself contains a (recursive) call
; to matchl that looks for the sum within the derivative expression.
; When the outer matchl is successful, E3 is assigned a value, but
; this value is ignored.  The values of E1 and E2, on the other hand,
; are used in the action part of the rule.

(defrule '(
  differentiate
  (d (the-function f) (equals-f v))
  1
  diff-x-rule ) )
; The pattern here uses two specially-defined functions.
; The first, THE-FUNCTION causes the second subexpression
; (the function to be differentiated) to be immediately
; assigned as the value of E1.
; The second function EQUALS-F only succeeds if the
; subexpression there (telling which variable differentiation
; is with respect to) is the same as that saved by THE-FUNCTION.

(defrule '(
  differentiate
  (d (the-function f) (does-not-occur-in-f v))
  0
  diff-const-rule ) )

;;; In the following, SAVE-F is a local variable accessible
;;; only to the three functions defined within its scope.
(let (save-f) 
  (defun the-function (f) (setf save-f f))
  (defun does-not-occur-in-f (v)
    (not (occurs-in v save-f)) )
  (defun equals-f (v) (equal save-f v)) )

;;; The following is a helper to DOES-NOT-OCCUR-IN-F.
;;; Returns T if ASYMBOL occurs at any level in EXPR:
(defun occurs-in (asymbol expr)
  (cond ((null expr) nil)
        ((atom expr)(eq asymbol expr))
        (t (or (occurs-in asymbol (first expr))
               (occurs-in asymbol (rest expr)) )) ) )

; rule for differentiating products:
(defrule '(
  differentiate
  (d ((lambda (f) (matchl '(* (? e1) (? e2)) f) ) e3)
     (? v1) )
  '(+
    (* e2 (d e1 v1))
    (* e1 (d e2 v1)) )
  diff-product-rule
  ) )

; rule for differentiating powers:
(defrule '(
  differentiate
  (d
   ((lambda (f) (matchl '(exp (? e1) (numberp e2)) f) ) e3)
   (? v1) )
  `(* e2
    (* (exp e1 ,(1- e2))
      (d e1 v1) ) )
  diff-power-rule
  ) )

; rule for subtracting 1:
(defrule '(
  simplify
  (1- (numberp e1))
  (1- e1)
  sub1-rule
  ) )

; rule for exponentiation by 0:
(defrule '(
  simplify
  (exp (? e1) 0)
  1
  exp0-rule
  ) )

; rule for exponentiation by 1:
(defrule '(
  simplify
  (exp (? e1) 1)
  'e1
  exp1-rule
  ) )

; rule for multiplication by 1:
(defrule '(
  simplify
  (* (? e1) 1)
  'e1
  times1-rule
  ) )

; variation of rule for multiplication by 1:
(defrule '(
  simplify
  (* 1 (? e1))
  'e1
  one-times-rule
  ) )

; rule for adding 0:
(defrule '(
  simplify
  (+ (? e1) 0)
  'e1
  plus0-rule
  ) )

; variation on rule for adding 0:
(defrule '(
  simplify
  (+ 0 (? e1))
  'e1
  zero-plus-rule
  ) )

; rule for multiplication by 0:
(defrule '(
  simplify
  (* (? e1) 0)
  0
  times0-rule
  ) )

; variation on rule for multiplication by 0:
(defrule '(
  simplify
  (* 0 (? e1))
  0
  zero-times-rule
  ) )

; rule to add constants when possible:
(defrule '(
  simplify
  (+ (numberp e1) (numberp e2))
  (+ e1 e2)
  constant-addition-rule
  ) )

; rule to multiply constants when possible:
(defrule '(
  simplify
  (* (numberp e1) (numberp e2))
  (* e1 e2)
  constant-multiplication-rule
  ) )

; global variables:
(defvar *current-formula*)
(defvar *current-goal*)


;;; The state information for the database consists of two parts:
;;;     (a) current formula
;;;     (b) current goal (either DIFFERENTIATE or SIMPLIFY).

;;; Initialize the goal and formula:
(setf *current-goal* 'differentiate)
(setf *current-formula* f0)

;;; function definitions:

;;; The control scheme tries rules until one succeeds, then starts again;
;;; when no rules fire, the current formula is returned.
(defun control ()
  "Top-level loop for the production system."
  (loop
    (format t "~%Current formula is now: ~S." *current-formula*)
    (cond ((not (try-rules *rules*))
           (return *current-formula*) )) ) )

;;; TRY-RULES tries each rule in RULES-LEFT on the
;;; current formula until one succeeds,
;;; or the end of list is reached,
;;; or the current formula is no longer a list.
;;; If a rule succeeds, the *CURRENT-FORMULA* is returned;
;;; otherwise, NIL is returned.
(defun try-rules (rules-left)
  "Successively tries rules on the *CURRENT-FORMULA*."
  (cond ((null rules-left) nil)
        ((atom *current-formula*) nil)
        (t (let ((temp (try-rule (first rules-left) *current-formula*)))
                (if temp (setf *current-formula* temp)
                    (try-rules (rest rules-left)) ) )) ) )

;;; TRY-RULE tries to apply a rule to an expression or
;;; one of its subexpressions.  If the rule is successful, the
;;; transformed expression is returned; otherwise, NIL is returned.
(defun try-rule (rule expression)
  "Tries RULE on given EXPRESSION and its subexpressions."
  (if (not (eq *current-goal* (rule-goal rule))) nil
      (try-rule1 rule expression) ) )

;;; TRY-RULE1 is the recursive slave of TRY-RULE:
(defun try-rule1 (rule expression)
  "Recurse helper to TRY-RULE."
  (let (bindings)
    (cond
      ; make sure expression is a list...
      ((atom expression) nil)
      ; attempt to apply rule to whole expression...
      ((setf bindings (matchl (rule-pattern rule) expression))
       (fire rule bindings) )
      ; try rule on subexpressions...
      (t (try-rule-on-list rule expression)) ) ) )

; The next function tries to apply the rule to each element on
; expression-list.  It returns NIL if the rule cannot be applied.
; Otherwise, it returns the list with one replacement: the first
; expression to which the rule can be applied is replaced
; by the result of applying the rule to it.
(defun try-rule-on-list (rule expression-list)
  "Tries RULE on successive elements of EXPRESSION-LIST."
  (let (temp)
    (cond ((null expression-list) nil)
          ((setf temp (try-rule1 rule (first expression-list)))
           (cons temp (rest expression-list)) )
          ((setf temp (try-rule-on-list rule (rest expression-list)))
           (cons (first expression-list) temp) )
          (t nil) ) ) )

; The function FIRE is evaluated when a production rule fires.
(defun fire (rule bindings)
  "Print message with name of rule and perform action."
  (format t "~%~a fires." (rule-name rule))
  (eval (apply-bindings (rule-action rule) bindings)) ; do action
   )
 
(defun apply-bindings (expr bindings)
  "Return the result of making substitutions specified
   by BINDINGS in EXPR."
  (if (null bindings) expr
      (let ((first-pair (first bindings)))
        (subst (rest first-pair)
               (first first-pair)
               (apply-bindings expr (rest bindings)) ) ) ) )

; test:
(defun settrace () (trace try-rule1 try-rule-on-list try-rules) )

(setf f10 '(d x x))
(setf f11 '(d (* x y) x))
(setf f12 '(d (* y y) x))
(setf f13 '(d (+ (* x 7) (* 8 x)) x))

(setf *current-formula* f0)
(setf *current-goal* 'differentiate)
(control)
(format t "~%Result is ~s.~%" *current-formula*)

(setf *current-formula* f13)
(setf *current-goal* 'differentiate)
(control)
(format t "~%Result is ~s.~%" *current-formula*)


