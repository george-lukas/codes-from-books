;;; GENETIC.CL
;;; A "genetic" algorithm for the Traveling Salesman Problem.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 5 ("Search") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Note that due to the use of random numbers, the results
;;; vary from run to run.

;;; Our problem uses a graph whose nodes present cities and
;;; whose arcs carry distances, much as for the UNIFORM-COST
;;; search and A* programs.

;;; Here is a hash table to store the distance information:
(let ((distance-info (make-hash-table :size 20)) )
  (defun set-distances (x y)
    (setf (gethash x distance-info) y) )
  (defun get-distances (x)
    (gethash x distance-info) )
  )

;;; Here is the set of cities for this problem:
(defparameter *cities*
              '(seattle portland spokane wenatchee bellingham) )
(defparameter *ncities* (length *cities*))

;;; Here are the distances.
;;; (These were estimated and not from a map)

(set-distances 'seattle
               '((portland . 150)(spokane . 350)
                 (wenatchee . 100)(bellingham . 90) ) )
(set-distances 'portland
               '((seattle . 150)(spokane . 400)
                 (wenatchee . 200)(bellingham . 235) ) )
(set-distances 'spokane
               '((portland . 400)(seattle . 350)
                 (wenatchee . 275)(bellingham . 385) ) )
(set-distances 'wenatchee
               '((portland . 200)(spokane . 275)
                 (seattle . 100)(bellingham . 130) ) )
(set-distances 'bellingham
               '((portland . 235)(seattle . 90)
                 (spokane . 385)(wenatchee . 130) ) )

;;; We represent an individual as a dotted pair whose left part
;;; is a list of cities and whose right part is a strength value.
(defun get-path (individual)
  "Returns the list of cities associated with INDIVIDUAL."
  (first individual) )

(defun get-strength (individual)
  "Returns the strength value associated with INDIVIDUAL."
  (rest individual) )

(defparameter *initial-population*
  '( ((seattle seattle seattle seattle seattle) . 0) ) )

(defvar *population*)
(defvar *current-min-strength*)
(defvar *current-pop-size*)

(defparameter *population-limit* 15)

(defun distance (a b)
  "Returns the distance between cities A and B."
  (if (eql a b) 0
      (rest (assoc b (get-distances a))) ) )

(defun cycle-cost (path)
  "Returns length of the PATH, including a closing
   arc from the last to the first element of PATH."
   (+ (distance (first path) (first (last path)))
      (path-cost path) ) )

(defun path-cost (path)
  "Returns the length of the PATH."
  (if (<= (length path) 1) 0
      (+ (distance (first path) (second path))
         (path-cost (rest path)) ) ) )

(defun non-tour-penalty (path)
  "Computes how far PATH is from being a tour.
   If PATH is a tour, then the penalty returned is 0."
  (* 100
    (+ (length (set-difference path *cities*))
       (length (set-difference *cities* path)) ) ) )

(defun chromosome-strength (individual)
  "Returns a value that is highest when INDIVIDUAL is a
   mimimum cost tour."
  (/ 10000.0
     (+ (* 2 (cycle-cost (get-path individual)))
        (* 50 (non-tour-penalty (get-path individual))) ) ) )

(defun mutate (individual)
  "Performs either MUTATE1 or MUTATE2, choosing randomly."
  (if (oddp (random 2))
      (mutate1 individual)
      (mutate2 individual) ) )

(defun mutate1 (individual)
  "Returns a slightly altered INDIVIDUAL, with the
   alteration generated randomly. One city is randomly changed."
  (let* ((path (get-path individual))
         (where (random (length path)))
         (new-city (nth (random *ncities*) *cities*)) )
    (cons (replace-nth path where new-city)
          0) ) )

(defun mutate2 (individual)
  "Returns a slightly altered INDIVIDUAL, with the
   alteration generated randomly. Two cities are transposed."
  (let* ((path (get-path individual))
         (where1 (random (length path)))
         (where2 (random (length path)))
         (city1 (nth where1 path))
         (city2 (nth where2 path)) )
    (cons (replace-nth
            (replace-nth path where1 city2)
            where2
            city1)
          0) ) )

(defun replace-nth (lst n elt)
  "Returns result of replacing the N-th element of LST by ELT."
  (cond ((null lst) nil)
        ((zerop n) (cons elt (rest lst)))
        (t (cons (first lst)
                 (replace-nth (rest lst) (1- n) elt) )) ) )

;;; In CROSSOVER we assume that PATH1 and PATH2 are of the same length.
(defun crossover (individual1 individual2)
  "Returns a new path resulting from
   genetic crossover of PATH1 and PATH2."
  (let* ((path1 (get-path individual1))
         (path2 (get-path individual2))
         (where (random (length path1))) )
    (cons (append (left-part path1 where)
                  (right-part path2 where) )
          0) ) )

(defun left-part (path k)
  "Returns the prefix of PATH having length K."
  (subseq path 0 k) )

(defun right-part (path k)
  "Returns the suffix of PATH starting at position K."
  (subseq path k) )

(defun random-individual ()
  "Returns a randomly selected member of the population."
  (nth (random (length *population*)) *population*) )

(defun another-individual (previous-rand-indiv)
  "Returns a randomly selected member of the population
   but makes an effort to find one that is different
   from PREVIOUS-RAND-INDIV."
  (let ((current-population-size (length *population*))
        (previous-path (get-path previous-rand-indiv))
        candidate)
    (dotimes (i 5 candidate) ; try at most 5 times.
      (setf candidate
            (nth (random current-population-size)
                 *population*) )
      (if (not (equal (get-path candidate) previous-path))
          (return candidate) ) ) ) )

(defun evolve (ngenerations nmutations ncrossovers)
  "Runs the genetic algorithm for NGENERATIONS times."
  (setf *population* *initial-population*)
  (dotimes (i ngenerations)
    (dotimes (j nmutations)
      (let ((mutated-one (mutate (random-individual))))
        (add-individual mutated-one) ) )
    (dotimes (j ncrossovers)
      (let* ((individual1 (random-individual))
             (individual2
               (another-individual individual1) )
             (crossing-result
              (crossover individual1
                         individual2) ) )
        (add-individual crossing-result) ) )
    (format t "~%In generation ~D, population is: ~S.~%"
            (1+ i) *population*)
   ) )

(defun add-individual (individual)
  "Computes and stores the chromosome-strength of INDIVIDUAL.
   Then adds the INDIVIDUAL to the population
   if the population limit has not yet been reached.
   Otherwise, if its strength exceeds that of the weakest
   member it replaces the weakest member."
  (let ((strength (chromosome-strength individual)))
    (setf (rest individual) strength) ; Here SETF works like RPLACD
    (if (= *current-pop-size* *population-limit*)
      (progn
        (if (> strength *current-min-strength*)
          ;;; Remove weakest current member:
          (progn
            (let ((k (where-strength-occurs
                       *current-min-strength*
                       *population*)))
              (setf *population* (remove-kth k *population*)) )
            ;;; Insert INDIVIDUAL into the population:
            (push individual *population*)
            (update-min-strength) ) ) )

      ;;; Still room in population...
      (progn
        (push individual *population*)
        (setf *current-min-strength*
              (min strength *current-min-strength*) )
        (incf *current-pop-size*) ) ) ) )

(defun update-min-strength ()
  "Computes and saves the minimum of all strengths of
   current members of the population."
  (setf *current-min-strength*
    (apply #'min (mapcar #'get-strength *population*)) ) )

(defun where-strength-occurs (val population-list)
  "Returns the first position in POPULATION-LIST where VAL occurs
   as the strength of that individual."
  (cond ((null population-list) nil)
        ((= val (get-strength (first population-list))) 0)
        (t (1+ (where-strength-occurs val (rest population-list)))) ) )

(defun remove-kth (k lst)
  "Returns a list consisting of LST with the Kth element deleted."
  (cond ((null lst) nil)
        ((zerop k) (rest lst))
        (t (cons (first lst)
                 (remove-kth (1- k) (rest lst)) )) ) )

(defun test ()
  "Does a trial run of EVOLVE."
  (setf *population* *initial-population*)
  (setf *current-min-strength* 0)
  (setf *current-pop-size* 1)
  (evolve 10 10 10) ; these values often lead to convergence at strength 4.78.
 )

(test)
