;;; PARSE.CL
;;; A top-down, backtracking parser for a context-free grammar

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 11
;;; ("Natural-Language Understanding") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Here is the grammar:
;;; The production rules for each nonterminal
;;;   are grouped together.  We store them in a hash table:

(let ((productions (make-hash-table :size 20)))
  (defun declare-productions (symbol right-hand-sides)
    "Stores the productions for SYMBOL in the hash table."
    (setf (gethash symbol productions) right-hand-sides) )
  (defun get-rhss (symbol)
    "Returns right-hand sides of productions for SYMBOL."
    (gethash symbol productions) )
  )
(declare-productions 's  '((np vp)))
(declare-productions 'np '((np pp)(d n)))
(declare-productions 'vp '((v av)(v)))
(declare-productions 'n  '((man)))
(declare-productions 'd  '((the)))
(declare-productions 'v  '((ran)))
(declare-productions 'av '((quickly)))
(declare-productions 'pp '((over there)))

;;; global variables declaration:
(defvar *current-parse*)

;;; TERMINALP is a predicate which distinguishes terminal symbols.
(defun terminalp (w)
  "Returns T if W is a known terminal symbol."
  (member w '(man the there ran quickly over)) )

;;; PARSE is the top level parser function.
(defun parse (input)
  "Initiates parsing of INPUT."
  (setf *current-parse* nil)
  (parse2 's nil 1 input (length input)) )

;;; PARSE2 is the function that actually applies productions
;;;  and does backtracking:
;;; X is leftmost nonterminal of current sentential form.
;;; RIGHT is the portion of the sentential form to the
;;;   right of X.
;;; S-LENGTH is the length of current form portion.
;;; UNMATCHED is the unmatched portion of the input.
;;; U-LENGTH is the length of UNMATCHED.
(defun parse2 (x right s-length unmatched u-length)
  "Tries applying all the productions for symbol X to this occurrence
   of this nonterminal, and calls COMPARE to continue the parsing."
  (let ((x-productions (get-rhss x)))
    (dolist (p x-productions)
      (push (cons x p) *current-parse*) ; Suppose we use this production...
      ;; examine resulting sentential form:
      (compare 
               (append p right)              ; apply production P
               (1- (+ s-length (length p)))  ; update length
               unmatched
               u-length)  

      (pop *current-parse*)  ; backtrack
       )   ; go try next alternative
    ) )

;;; COMPARE matches terminal symbols in the sentential form
;;;  with the input and continues the parsing if on the right track.
;;; BETA is the relevant suffix of the current sentential form.
;;; UNMATCHED is the portion of the input remaining to be matched.
(defun compare (beta s-length unmatched u-length)
  "Performs part of the parsing."
  (cond
    ((null beta)                       ; Nothing left of sent. form.
     (cond ((null unmatched)           ; Nothing unmatched: Success!
            (format t "~%Parse: ~s."   ; Print answer (in reverse order).
                    *current-parse*) )
           (t nil) ) )                 ; Something unmatched-no good.
    ((> s-length u-length) nil)        ; Derivation too long.
    ((terminalp (first beta))          ; Current symbol is a terminal.
     (cond ((null unmatched) nil)      ; If no input left, no good.
           ((eql (first beta)
                 (first unmatched))    ; If symbols match, then
            (compare (rest beta)       ; try to match more.
                     (1- s-length)
                     (rest unmatched)
                     (1- u-length) ) )
           (t nil)  ; case where terminal doesn't match.
           ) )
    (t ; we have reached a nonterminal in BETA;
       ; parse recursively:
       (parse2 (first beta)            ; new leftmost nonterminal,
               (rest beta)             ; new right portion,
               s-length
               unmatched               ; remaining input.
               u-length) ) ) ) 

;;; TEST tries out the parser.
(defun test ()
  (let ((sentence '(the man over there ran quickly)))
    (trace parse2)
    (parse sentence) ) )


(test)
