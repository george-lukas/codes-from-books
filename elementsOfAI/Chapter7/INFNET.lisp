;;; INFNET.CL
;;; A LISP implementation of an INFerence NETwork that
;;;  uses subjective-Bayesian updating of certainty values.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 7 ("Probabilistic Reasoning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Given values of particular evidential variables,
;;;  the system determines the certainty value of several
;;;  intermediate variables and a summary variable.
;;; The problem solved is the prediction of food quality
;;;  in a restaurant:
;;;  The "Traveller's Restaurant Selection Problem"

;;; ODDS takes a probability value and returns the
;;; corresponding odds value.
(defun odds (prob)
  "Returns the Odds value corresponding to PROB."
  (/ prob (- 1.0 prob)) )

;;; PROB is the inverse of ODDS.
(defun prob (odds)
  "Returns the Probability value corresponding to ODDS."
  (/ odds (1+ odds)) )

;;; Each node of the inference network is represented by
;;; a symbol and its value which is a structure.
;;; The structure has 6 components.
(defstruct node
  name
  prior-prob
  prior-odds
  current-prob
  current-odds
  arcs
  )

;;; The macro DEFINE-NODE helps to set up these
;;; representations, allowing a simpler syntax,
;;; and computing the odds entries.
(defmacro define-node (name prior-prob current-prob arcs)
  "Creates a node with the given name and components."
  (setf (symbol-value name)
    (make-node
      :name name
      :prior-prob prior-prob
      :prior-odds (odds prior-prob)
      :current-prob current-prob
      :current-odds (odds current-prob)
      :arcs arcs
  ) ) )

(defun arc-sufficiency (arc)
  "Returns the Sufficiency parameter for ARC."
  (second arc) )

(defun arc-necessity (arc)
  "Returns the Necessity parameter for ARC."
  (third arc) )

;;; Here we set up nodes for the Primary Evidential Variables:
(define-node decor 0.5 0.9 ())
(define-node table-setting 0.5 0.8 ())
(define-node surface-cleanliness 0.8 0.8 ())
(define-node air 0.6 0.6 ())
(define-node sounds 0.5 0.5 ())
(define-node clientele 0.5 0.9 ())
(define-node menu 0.5 0.5 ())
(define-node prices 0.5 0.9 ())
(define-node service 0.3 0.9 ())

;;; Here are declarations for the Lumped Evidential Variables:
(define-node popularity 0.5 0.6  (indep
                                  (arc sounds 3.0 1.0)
                                  (arc clientele 1.0 0.24) ))
(define-node elegance 0.5 0.5    (indep
                                  (arc decor 3.0 0.5)
                                  (arc table-setting 1.0 0.74)
                                  (arc sounds 1.5 0.74)
                                  (arc clientele 1.0 0.5)
                                  (arc menu 1.24 0.74)
                                  (arc prices 1.24 0.74)
                                  (arc service 1.0 0.5) ))
(define-node artistry 0.5 0.9    (indep
                                  (arc decor 1.0 0.5)
                                  (arc table-setting 1.0 0.5)
                                  (arc menu 1.5 0.74)
                                  (arc service 1.0 0.5) ))
(define-node cleanliness 0.7 0.7 (indep
                                  (arc surface-cleanliness
                                        1.5 0.2)
                                  (arc air 1.5 0.5) ))

;;; Here are node definitions for the Predicted Component Variables:
(define-node taste 0.6 0.6       (indep
                                  (arc popularity 3.0 0.7)
                                  (arc elegance 1.5 0.8) ))
(define-node texture 0.6 0.6     (indep
                                  (arc popularity 3.0 0.7)
                                  (arc elegance 1.0 0.5) ))
(define-node appearance 0.5 0.5  (indep
                                  (arc artistry 3.0 0.4)))
(define-node quantity 0.5 0.5    (indep
                                  (arc popularity 1.5 0.5)))
(define-node correctness 0.5 0.5 (indep
                                  (arc elegance 1.0 0.7)))
(define-node nutrition 0.6 0.6   (indep
                                  (arc popularity 1.1 0.7)
                                  (arc elegance 1.8 0.8) ))
(define-node hygiene 0.8 0.8     (indep
                                  (arc cleanliness 1.0 0.1)))

;;; Here is the Predicted Summary Variable node:
(define-node overall-food-quality 0.5 0.5
                                 (indep
                                  (and
                                   (arc taste 3.0 0.3)
                                   (arc texture 1.0 0.5) )
                                  (and
                                   (arc appearance 1.0 0.3)
                                   (arc correctness 1.3 0.8) )
                                  (arc quantity 1.2 0.8)
                                  (arc nutrition 1.0 0.3)
                                  (arc hygiene 1.5 0.2) ))

;;; UPDATE-PROB computes P(H | E') for a single arc.
;;; H is a node structure, and ARC is a single arc coming into it.
(defun update-prob (h arc)
  "Returns the probability of H given the uncertain evidence."
  (let ((predecessor (symbol-value (first arc))) )
    (cond
      ((> (node-current-prob predecessor)
          (node-prior-prob predecessor) )
       (report-progress 'supportive h arc)
       (+ (node-prior-prob h)
          (* (/ (- (prob (* (arc-sufficiency arc)
                            (node-prior-odds h) ))
                   (node-prior-prob h) )
                (- 1.0 (node-prior-prob predecessor)) )
             (- (node-current-prob predecessor)
                (node-prior-prob predecessor) ) ) ) )
      (t (report-progress 'inhibitive h arc)
        (+ (prob (* (arc-necessity arc) (node-prior-odds h)))
            (* (/ (- (node-prior-prob h)
                     (prob (* (arc-necessity arc)
                              (node-prior-odds h) )) )
                  (node-prior-prob predecessor) )
               (node-current-prob predecessor) ) ) ) ) ) )

(defvar *reporting* nil)  ; global variable controlling printouts.

;;; REPORT-PROGRESS describes the progress of the updating
;;; if global variable *REPORTING* is not null.
(defun report-progress (supp-inhib node arc)
  "If *REPORTING* then prints information about progress."
  (if *reporting*
    (progn
      (format t "~%~a Probability updating for node ~a along arc:"
                supp-inhib (node-name node))
      (format t "~%~s with prior odds ~9,4f."
                arc (node-prior-odds node) )
      (format t "~%Prior and current probs of E are ~6,4f and ~6,4f."
                (node-prior-prob (symbol-value (first arc)))
                (node-current-prob (symbol-value (first arc))) )
        ) ) )

;;; COMBINE-INDEP-LAMBDAS determines the updating factors
;;; for all arcs coming into a node and multiplies them
;;; to get an overall odds updating factor.  This scheme
;;; assumes that the arcs are treated as if their influences
;;; were independent.
(defun combine-indep-lambdas (arc-exp)
  "Returns an effective lambda value for indep. evidence."
  (apply #'*
         (mapcar #'eval-arc-exp
                 (rest arc-exp) ) ) )

;;; COMBINE-CONJUNCTIVE-LAMBDAS handles AND relationships.
(defun combine-conjunctive-lambdas (arc-exp)
  "Returns an effective lambda value for a conjunction."
  (apply #'min
         (mapcar #'eval-arc-exp
                 (rest arc-exp) ) ) )

;;; COMBINE-DISJUNCTIVE-LAMBDAS handles OR relationships.
(defun combine-disjunctive-lambdas (arc-exp)
  "Returns an effective lambda value for a disjunction."
  (apply #'max
         (mapcar #'eval-arc-exp
                 (rest arc-exp) ) ) )

;;; UPDATE-NODES takes a list of nodes and calls
;;; UPDATE-NODE on each one.
(defun update-nodes (nodes)
  "Updates the current probabilities and odds for all
   nodes on list NODES."
  (cond ((null nodes) nil)
        (t (update-node (first nodes))
           (update-nodes (rest nodes)) ) ) )

;;; The function EVAL-ARC-EXP evaluates an arc expression, finding
;;; an effective odds updating factor that takes effects of all
;;; the arcs in the expression into account.
(defun eval-arc-exp (arc-exp)
  "Returns an effective lambda value for an
   entire arc expression."
  (cond ((eql (first arc-exp) 'arc)
         (effective-arc-lambda (rest arc-exp)) )
        ((eql (first arc-exp) 'indep)
         (combine-indep-lambdas arc-exp) )
        ((eql (first arc-exp) 'and)
         (combine-conjunctive-lambdas arc-exp) )
        ((eql (first arc-exp) 'or)
         (combine-disjunctive-lambdas arc-exp) )
        (t (print '(illegal arc expression)) (print arc-exp)) ) )

;; We use a local variable NODE to simplify argument passing
;; between UPDATE-NODE and EFFECTIVE-ARC-LAMBDA.

(let (node)
  ;;; UPDATE-NODE computes the new current probability for a node.
  ;;; H is a symbol whose global value is a node structure.
  (defun update-node (h)
    "Updates the values for node H."
    (let ((s (symbol-value h)))
      (setf node h)  ; use dynamic variable *node*.
      (setf (node-current-odds s)
            (* (node-prior-odds s)
               (eval-arc-exp (node-arcs s)) ) )
      (setf (node-current-prob s) (prob (node-current-odds s)))
      (format t "~%Current probability of node ~a is ~6,4f.~%"
                h (node-current-prob s) ) ) )

  ;;; EFFECTIVE-ARC-LAMBDA determines the odds-updating factor
  ;;; along the ARC specified, given the prior and current
  ;;; probabilities and odds for the predecessor node, the priors
  ;;; for the node NODE, and the sufficiency and necessity
  ;;; values along the arc.
  (defun effective-arc-lambda (arc)
    "Returns the effective lambda value for one arc, based on
     the uncertain evidence."
    (/ (odds (update-prob (symbol-value node) arc))
       (node-prior-odds (symbol-value node)) ) )
  ) ; end of lexical scope for NODE.

;;; TEST makes a pass through the non-input nodes,
;;; updating their probabilities:
(defun test ()
  "Updates all non-input node probabilities."
  (update-nodes '(popularity elegance artistry cleanliness
        taste texture appearance quantity
        correctness nutrition hygiene
        overall-food-quality)) )

;;; To interactively set the current probability and
;;; odds for a node, it is convenient use a form such
;;; as (SET-PROB SOUNDS 0.9)...
(defmacro set-prob (node new-prob)
  "Stores a new current probability value for NODE,
   and also computes a new current odds for it."
  `(progn (setf (node-current-prob ,node) ,new-prob)
          (setf (node-current-odds ,node) (odds ,new-prob)) ) )

(setf *reporting* t)
(test)
