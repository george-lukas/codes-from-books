;;; MED-ES.CL
;;; A very simple medical "expert system"
;;; to demonstrate forward and backward chaining.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 5 ("Search") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; The medical rules here are inspired by a flowchart on page 51 of
;;; The American Medical Association Encyclopedia of Medicine.
;;; However, these rules are neither a complete nor accurate
;;; representation of the chart!

;;; This rule base includes "query" rules. When invoked, these
;;; rules ask the user two questions.  One question is fact-oriented,
;;; while the second asks the user to assess her or his confidence in
;;; the answer.

(defun set-up-medical-rules ()
  "Defines the medical rules."
  (shell-init)

  (define-rule 
    '(abdom-pain?
      IF (health-problem (? cf))
      THEN 
      ("Do you have abdominal pain?"
        (abdominal-pain)
        (knowledge-of-abd-pain))
      WITH-CERTAINTY 1.0) )

  (define-rule 
    '(abdom-pain-severe?
      IF (abdominal-pain (? cf))
      THEN 
      ("Is the abdominal pain severe?" 
        (abdominal-pain-severe)
        (knowledge-of-abd-pain-severe) )
      WITH-CERTAINTY 1.0) )

  (define-rule
    '(temp-relevant
      IF (abdominal-pain-severe (? cf))
      THEN (temp-relevant)
      WITH-CERTAINTY (strengthen (? cf)) ) )

  (define-rule
    '(high-temp?
    IF (temp-relevant (? cf))
    THEN 
    ("Is your temperature over 100 degrees Fahrenheit?"
      (temp-over-100)
      (knowledge-of-temp-over-100) )
    WITH-CERTAINTY 1.0) )

  (define-rule
    '(immediate-attention!
      IF (AND (abdominal-pain-severe (? cf1))
              (temp-over-100 (? cf2)) )
      THEN (seek-immediate-attention)
      WITH-CERTAINTY (or-indep (? cf1) (? cf2)) ) )

  (define-rule
    '(diarrhea-relevant
      IF (AND (abdominal-pain (? cf1))
              (abdominal-pain-severe (? cf2)) )
      THEN (diarrhea-relevant)
      WITH-CERTAINTY (and-indep (? cf1) (compl (? cf2))) ) )
  ;;; Note that severe abdominal pain lowers the certainty of
  ;;; DIARRHEA RELEVANT by this formula.

  (define-rule
    '(diarrhea?
      IF (diarrhea-relevant (? cf))
      THEN 
        ("Do you have diarrhea?"
          (diarrhea)
          (knowledge-of-diarrhea) )
      WITH-CERTAINTY 1.0) )

  (define-rule
    '(food-poisoning
      IF (AND (abdominal-pain (? cf1))
              (diarrhea (? cf2)) )
      THEN (food-poisoning)
      WITH-CERTAINTY (* (? cf1) (? cf2)) ) )

  ;;; Let's assume the user has some kind of health problem,
  ;;; to get the session rolling.
  (assert-fact '(health-problem 0.9))
  )

(defun med-fc ()
  "Tests the forward-chaining capability with the medical rules."
  (set-up-medical-rules)
  (forward-chain) )

(defun med-bc ()
  "Tests the backward-chaining capability with the medical rules."
  (set-up-medical-rules)
  (explain-proof (backward-chain '(food-poisoning))) )

;;; Run the forward-chaining inference engine...
;(med-fc)

;;; Run the backward-chaining inference engine...
;(med-bc)
