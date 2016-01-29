;;; ROMAN3.CL - discrimination net implementation

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 3 ("Productions Systems
;;; and Pattern Matching") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

(defun roman3 ()
  "Roman numeral conversion with a discrimination net."
  (let ((x nil))
    (loop
      (cond
        ((null x) (format t "Enter number:")
                  (setf x (read)) )
        (t (cond
            ((> x 39) (format t "too big~%")
                      (setf x nil) )
            (t (cond
                 ((> x 4)
                  (cond ((> x 9)
                         (prin1 'x)(decf x 10))
                        (t (cond ((= x 9)
                                  (prin1 'ix)
                                  (setf x 0) )
                                 (t
                                  (prin1 'v)
                                  (decf x 5) ) )) ))

                 (t (cond ((= x 4) (prin1 'iv)
                                        (setf x 0) )
                          (t (cond ((> x 0)
                                    (prin1 'i)
                                    (decf x) )
                                   (t (terpri)
                                      (setf x nil) )
                              )) )) )) )) )
      ) ) )
