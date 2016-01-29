;;; ROMAN1.CL - unordered production system to
;;; convert to Roman numerals.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 3 ("Productions Systems
;;; and Pattern Matching") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

(defun roman1 ()
  "Roman numeral conversion with an unordered P.S."
  (let ((x nil))
    (loop
      (cond
        ((null x) (format t "Enter number:") (setf x (read)))
        ((and (not (null x)) (> x 39))
         (format t "too big~%") (setf x nil))
        ((and (not (null x)) (< x 40) (> x 9))
         (prin1 'x) (setf x (- x 10)) )
        ((and (not (null x)) (= x 9))
         (prin1 'ix) (setf x 0) )
        ((and (not (null x)) (< x 9) (> x 4))
         (prin1 'v) (setf x (- x 5)) )
        ((and (not (null x)) (= x 4))
         (prin1 'iv) (setf x 0) )
        ((and (not (null x)) (< x 4) (> x 0))
         (prin1 'i) (setf x (1- x)) )
        ((zerop x) (setf x nil) (terpri))
         ) ) ) )
