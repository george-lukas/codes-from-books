;;; EDGES.CL
;;; A demonstration of edge detection and related image processing
;;; in Lisp.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 12 ("Vision") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

(defparameter *nrows* 8)
(defparameter *ncols* 8)

(defun make-image ()
  "Creates and returns a sample binary image."
  (make-array (list *nrows* *ncols*)
              :initial-contents '(
    (1 1 1 1 1 1 0 0)
    (0 1 0 0 0 0 1 0)
    (0 1 0 0 0 0 0 1)
    (0 1 0 0 0 0 0 1)
    (0 1 0 0 0 0 0 1)
    (0 1 0 0 0 0 0 1)
    (0 1 0 0 0 0 1 0)
    (1 1 1 1 1 1 0 0) )) )

(defun print-row (image row)
  "Prints one row of an image."
  (dotimes (i *nrows*)
    (format t "~5D" (aref image row i)) ) )

(defun print-image (image)
  "Print out IMAGE nicely formatted."
  (dotimes (i *nrows*)
    (print-row image i)
    (format t "~%") ) )

(defun amplify-image (image1 factor)
  "Returns result of multiplying each element of
   IMAGE1 by FACTOR."
  (let ((image2 (make-array (list *nrows* *ncols*))))
    (dotimes (row *nrows*)
      (dotimes (col *ncols*)
        (setf (aref image2 row col) 
              (* (aref image1 row col) factor) ) ) )
    image2) )

(defun threshold-image (image1 threshold)
  "Returns result of thresholding each element of
   IMAGE1 with THRESHOLD."
  (let ((image2 (make-array (list *nrows* *ncols*))))
    (dotimes (row *nrows*)
      (dotimes (col *ncols*)
        (setf (aref image2 row col) 
          (if (< (aref image1 row col)
                 threshold)
            0
            1) ) ) )
    image2) )

(defun add-images (image1 image2)
  "Returns the element-by-element sum of IMAGE1
   and IMAGE2."
  (let ((image3 (make-array (list *nrows* *ncols*))))
    (dotimes (row *nrows*)
      (dotimes (col *ncols*)
        (setf (aref image3 row col) 
              (+ (aref image1 row col)
                 (aref image2 row col) ) ) ) )
    image3) )

(defun make-random-image (pmax)
  "Returns an array with elements in range 0 to PMAX."
  (let ((new-image (make-array (list *nrows* *ncols*))))
    (dotimes (row *nrows*)
      (dotimes (col *ncols*)
        (setf (aref new-image row col) 
              (random pmax) ) ) )
    new-image) )

(defun horizontal-differences (image1)
  "Returns result of computing all horizontal
   differences."
  (let ((image2 (make-array (list *nrows* *ncols*))))
    (dotimes (row *nrows*)
      (setf (aref image2 row (1- *ncols*)) 0)
      (dotimes (col (1- *nrows*))
        (setf (aref image2 row col) 
              (- (aref image1 row col)
                 (aref image1 row (1+ col)) ) ) ) )
    image2) )

(defun roberts-cross (image1)
  "Returns result of applying Roberts' cross operator."
  (let ((image2 (make-array (list *nrows* *ncols*))))
    (dotimes (row (1- *nrows*))
      (setf (aref image2 row (1- *ncols*)) 0)
      (dotimes (col (1- *ncols*))
        (setf (aref image2 row col) 
          (floor
            (sqrt
              (+ (sqr
                   (- (aref image1 row col)
                      (aref image1 (1+ row) (1+ col)) ))
                 (sqr
                   (- (aref image1 (1+ row) col)
                      (aref image1 row (1+ col))
                     ) ) ) ) ) ) ) )
    (dotimes (col *ncols*)
      (setf (aref image2 (1- *nrows*) col) 0) )
    image2) )

(defun sqr (x)
  "Returns X squared."
  (* x x) )

(defun test ()
  (let* ((my-initial (amplify-image (make-image) 25))
         (noisy-initial
           (add-images 
             my-initial
             (make-random-image 50) ) ) )
    (format t "~% Image MY-INITIAL:        ~%")
    (print-image my-initial)
    (format t "~% -------------------------~%")
    (format t "~% Horizontal differences:  ~%")
    (print-image (horizontal-differences my-initial))
    (format t "~% -------------------------~%")
    (format t "~% Roberts Cross:           ~%")
    (print-image (roberts-cross my-initial))
    (format t "~% -------------------------~%")
    (format t "~% Image NOISY-INITIAL:     ~%")
    (print-image noisy-initial)
    (format t "~% -------------------------~%")
    (format t "~% Horiz. diffs. (noisy):   ~%")
    (print-image (horizontal-differences noisy-initial))
    (format t "~% -------------------------~%")
    (format t "~% Roberts Cross (noisy):   ~%")
    (print-image (roberts-cross noisy-initial))
    (format t "~% -------------------------~%")
    (format t "~% Thresholded Rob.X(noisy):~%")
    (print-image 
      (threshold-image
        (roberts-cross noisy-initial) 30) ) ) )

(test)




