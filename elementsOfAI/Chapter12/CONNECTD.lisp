;;; CONNECTD.CL
;;; A program for connected components labelling.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 12 ("Vision") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Set up the input array:
(defparameter *nrows* 8)
(defparameter *ncols* 8)

(defparameter *test-image*
  (make-array (list *nrows* *ncols*) :initial-contents
  '((0 1 0 0 0 0 0 0)
    (0 1 1 0 0 0 0 0)
    (0 0 0 1 0 0 0 0)
    (0 0 0 0 1 1 1 0)
    (0 0 0 0 1 0 1 0)
    (1 1 1 0 1 1 1 0)
    (0 1 1 0 0 0 0 0)
    (0 0 0 0 0 0 0 0) ) ) )

(defun negate (a)
  "Creates and returns a new 2-D image array in which
   each element is the negation of its corresponding
   element in the array A."
  (let ((b (make-array (list *nrows* *ncols*))))
    (dotimes (i *nrows*)
      (dotimes (j *ncols*)
        (setf (aref b i j) (- 0 (aref a i j)))
         ) )
    b) )

;;; CONNECTED-COMPONENTS is the top-level function for
;;; the finding connected components of the input IMAGE.
(defun connected-components (image)
  "Calls SCAN on the negation of IMAGE."
  (scan (negate image)) )

(defun scan (image)
  "Performs a raster-scan of the image looking for
   successive connected components."
  (let ((count 0))
    (dotimes (i *nrows*)
      (dotimes (j *ncols*)
        (format t "Scanning ~s, ~s.~%" i j) ; Show progress.
        (cond ((= (aref image i j) -1)      ; unmarked figure cell?
               (incf count)                 ; Yes, up the count, and
               (dfs image count i j) ))     ; label the component.
                ) ) 
    image) )

(defparameter *directions*   ; 8-adjacency definition.
      '((-1 -1)(-1 0)(-1 1)(0 -1)(0 1)(1 -1)(1 0)(1 1)) )

(defun dfs (image count i j)
  "Conducts a depth-first search from position I, J for
   more cells in the current component."
  (cond ((= (aref image i j) -1)        ; Be sure cell is figure.
         (setf (aref image i j) count)  ; then mark the pixel and
         (dolist (which-way *directions*) ; search in all directions.
                 (branch which-way image count i j) ) )
        (t nil) ) )    ; Don't continue if cell is
                       ; already labelled or background.

(defun branch (which-way image count i j)
  "Attempts to continue the search in
   direction WHICH-WAY."
  (let ((ii (+ i (first which-way)))    ; Determine row and col
        (jj (+ j (second which-way))) ) ; of new cell.
    (and (< -1 ii)                      ; Check array bounds...
         (< -1 jj)
         (< ii *nrows*)
         (< jj *ncols*)
         (dfs image count ii jj) ) ) )  ; OK, continue search.

(defun print-row (image row)
  "Prints one row of an image."
  (dotimes (i *nrows*)
    (format t "~5D" (aref image row i)) ) )

(defun print-image (image)
  "Prints out IMAGE nicely formatted."
  (dotimes (i *nrows*)
    (print-row image i)
    (format t "~%") ) )

(defun test ()
  "Performs a demonstration."
  (print-image
    (connected-components
      *test-image*) ) )

(test)
