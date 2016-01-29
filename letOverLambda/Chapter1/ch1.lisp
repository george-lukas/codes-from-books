;;;; ch1.lisp - Chapter 1
;;;; George Lukas < last update: Jan/09/2016 - 07:44PM >

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  (if (zerop n) (error "zero 1ength"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec 
		       (first x)
		       (rec (rest x) acc))))))
    (rec x nil)))

(defun fat (n &optional (aux 1))
  "Tail-recursive Factorial."
  (if (<= n 1) aux
      (fat (- n 1) (* n aux))))

(defun choose (n r)
  "Binomial coefficient function."
  (/ (fat n)
     (fat (- n r))
     (fat r)))
