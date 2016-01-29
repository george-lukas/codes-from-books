;;; SHRINK.CL -- a simple conversational program after ELIZA.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 3 ("Productions Systems
;;; and Pattern Matching") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Requires the functions MATCH and VAL defined in MATCH.CL.

(defun shrink ()
  "SHRINK is the top-level function."
  (let ((wword-count 0) ; counter for question words.
        (punt-count 0)  ; counter for punt responses.
        b  ; holds bindings after matching.
        s  ; holds the mapped input sentence.
        )
  (format t "WELCOME TO MY SOFA!~%")
  (format t "PLEASE ENCLOSE YOUR INPUT IN PARENTHESES.~%")
  (loop (setq s (you-me-map (read)))
        (terpri)
        (cond ((match '(bye) s)
               (return 'goodbye) )
              ((setf b (match '(you are (* x)) s))
               (incf wword-count)
               (print-s (append '(please tell me)
                                (list (wword wword-count))
                                '(you are)
                                (val 'x b) )) )
              ((setf b (match '(you have (* x)) s))
               (print-q (append '(how long have you had) (val 'x b) )) )
              ((match '(you feel (* x)) s)
               (format t "I SOMETIMES FEEL THE SAME WAY.~%") )
              ((match '(because (* x)) s)
               (format t "IS THAT REALLY THE REASON.~%") )
              ((match nil s)
               (format t "PLEASE SAY SOMETHING!~%") )
              ((setf b (match '(yes (* x)) s))
               (print-q (append '(how can you be so sure) (val 'x b) )) )
              ((setf b (match '(me are (* x)) s))
               (print-s (append '(oh yeah i am) (val 'x b) )) )
              ((setf b (match '((verbp v) (* x)) s))
               (print-q (append '(why do you want me to)
                               (list (val 'v b)) (val 'x b) ) ) )
              ((setf b (match '((wpred w)(* x)) s))
               (print-s (append '(you tell me)(list (val 'w b))) ) )
              ((match '(do me think (* x)) s)
               (format t "I THINK YOU SHOULD ANSWER THAT YOURSELF.~%") )
              ((setf b (match '((dpred w) me (* x)) s))
               (print-s (append '(perhaps i)(list (val 'w b)) (val 'x b) ) ) )
              ((member 'dream s)
               (format t "FOR DREAM ANALYSIS SEE FREUD.~%") )
              ((member 'love s)
               (format t "ALL IS FAIR IN LOVE AND WAR.~%") )
              ((member 'no s)
               (format t "DONT BE SO NEGATIVE.~%") )
              ((member 'maybe s)
               (format t "BE MORE DECISIVE!~%") )
              ((member 'you s)(print-s s))
              (t (incf punt-count)
                 (print-s (punt punt-count)) ) ) ) ) )

(defun print-s (message)
  "Print message list as a sentence."
  (printl (butlast message))
  ; Print the last element followed by a period:
  (format t "~a." (first (last message)))
  (terpri) )

(defun print-q (message)
  "Print message list as a question."
  (printl (butlast message))
  ; Print the last element followed by a question mark:
  (format t "~a?" (first (last message)))
  (terpri) )

(defun printl (message)
  "Prints a list without the surrounding parens."
  ; Print each element except the last followed by a space:
  (mapcar #'(lambda (txt) (format t "~a " txt))
          message) )

;;; WWORD returns one of four question words,
;;; WWORD-COUNT must be an integer.
(defun wword (wword-count)
  "Returns the ith question word where i = WWORD-COUNT mod 4."
  (nth (mod wword-count 4)
          '(when why where how) ) )

(defun wpred (w)
  "Returns T if W is one of the question words."
  (member w '(why where when what which how)) )

(defun dpred (w)
  "Returns T is W is an auxiliary verb."
  (member w '(do can should would)) )

(defun punt (punt-count)
  "Returns one from a list of default responses."
  (nth (mod punt-count 6)
            '((please go on)
              (tell me more)
              (i see)
              (what does that indicate)
              (but why be concerned about it)
              (just tell me how you feel) ) ) )

(defun you-me (w)
  "Changes a word from 1st to 2nd person or vice-versa."
  (cond
    ((eql w 'i) 'you) ((eql w 'me) 'you) ((eql w 'you) 'me)
    ((eql w 'my) 'your) ((eql w 'your) 'my) 
    ((eql w 'yours) 'mine) ((eql w 'mine) 'yours)
    ((eql w 'am) 'are) (t w) ) )

(defun you-me-map (lst)
  "Applies YOU-ME to a whole sentence or phrase."
  (mapcar (function you-me) lst) )

(defun verbp (w)
  "Return T if W is one of these known verbs."
  (member w '(go have be try eat take help make get jump
              write type fill put turn compute
              think drink blink crash crunch add) ) )

