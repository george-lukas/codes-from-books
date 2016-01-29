;;; BESTFS.CL

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 5 ("Search") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Best-First Search (Forward-Looking version).
;;; It uses an evaluation function based on the ordering of cities
;;; with respect to longitude.  The method does not take into
;;; consideration the length of each partial path found so far.
;;; Thus it considers only the "remaining distance" to the goal
;;; in deciding how to order nodes on OPEN.  Hence it is
;;; "forward-looking".

;;; Here is the adjacency data (identical to that used
;;; in DEPTHFS.CL and BREADTH.CL):

;;; Create the hash table ADJACENCY-INFO to store the French
;;; roads data, and define the functions for entering and
;;; retrieving this info.
;;; The second hash table, PATH-PREDECESSOR-INFO, is used
;;; during the search to allow the eventual tracing of
;;; the path back from the goal to the starting node.
(let ((adjacency-info (make-hash-table :size 20))
      (path-predecessor-info (make-hash-table :size 20)) )
  (defun set-adj (x y)
    (setf (gethash x adjacency-info) y) )
  (defun get-adj (x)
    (gethash x adjacency-info) )
  (defun set-predecessor (x y)
    (setf (gethash x path-predecessor-info) y) )
  (defun get-predecessor (x)
    (gethash x path-predecessor-info) )
 )

;;; Establish BREST's list of neighbors as (RENNES), etc.
(set-adj 'brest '(rennes))
(set-adj 'rennes '(caen paris brest nantes))
(set-adj 'caen '(calais paris rennes))
(set-adj 'calais '(nancy paris caen))
(set-adj 'nancy '(strasbourg dijon paris calais))
(set-adj 'strasbourg '(dijon nancy))
(set-adj 'dijon '(strasbourg lyon paris nancy))
(set-adj 'lyon '(grenoble avignon limoges dijon))
(set-adj 'grenoble '(avignon lyon))
(set-adj 'avignon '(grenoble marseille montpellier lyon))
(set-adj 'marseille '(nice avignon))
(set-adj 'nice '(marseille))
(set-adj 'montpellier '(avignon toulouse))
(set-adj 'toulouse '(montpellier bordeaux limoges))
(set-adj 'bordeaux '(limoges toulouse nantes))
(set-adj 'limoges '(lyon toulouse bordeaux nantes paris))
(set-adj 'nantes '(limoges bordeaux rennes))
(set-adj 'paris '(calais nancy dijon limoges rennes caen))

;;; Now we create a new hash table LONGITUDE-INFO to
;;; support the heuristic ordering mechanism.
(let ((longitude-info (make-hash-table :size 20)))
  (defun set-longitude (x y)
    (setf (gethash x longitude-info) y) )
  (defun get-longitude (x)
    (gethash x longitude-info) )
 )

;;; The longitude of each city is stored in tenths of a degree.
;;; We use a local function with a LAMBDA form, since
;;; SET-LONGITUDE takes two arguments but we want a
;;; function that takes one argument for this use with MAPCAR.
(mapcar #'(lambda (pair) (apply #'set-longitude pair))
	'((avignon 48)(bordeaux -6)(brest -45)(caen -4)
	  (calais 18)(dijon 51)(grenoble 57)(limoges 12)
	  (lyon 48)(marseille 53)(montpellier 36)
	  (nantes -16)(nancy 62)(nice 73)(paris 23)
	  (rennes -17)(strasbourg 77)(toulouse 14) ) )

;;; We need one more hash table F-VALUE to
;;; remember the heuristic value at each node visited.
(let ((f-values (make-hash-table :size 20)))
  (defun set-f-value (x y)
    (setf (gethash x f-values) y) )
  (defun get-f-value (x)
    (gethash x f-values) )
 )

;;; BEST-FIRST-SEARCH is the main searching procedure.
(defun best-first-search (start-node goal-node)
  "Performs a best-first search from START-NODE for GOAL-NODE."
  (set-goal goal-node)
  (let ((open (list start-node))                ;step1
        (closed nil)
        n l val)
    (set-predecessor start-node nil)
    (set-f-value start-node (f start-node))
    (loop
      (if (null open)(return 'failure))         ;step2
      (setf n (select-best open))               ;step3
      (setf open (remove n open))               ;step4
      (push n closed)
      (if (eql n (get-goal))                    ;step5
          (return (extract-path n)) )
      (setf l (successors n))                   ;step6
      (setf l (list-difference l closed))
      (dolist (j (intersection l open))         ;step7
              (if (< (setf val (f j))
                     (get-f-value j) )
                  (progn
                    (set-f-value j val)
                    (setf open
                          (insert j
                                  (remove j open)
                                  open
                                  val) ) ) ) )
      (dolist (j (list-difference l (append open closed)))
              ;; open the node J:
              (increment-count)
              (set-f-value j (setf val (f j)))
              (setf open (insert j open val))
              (set-predecessor j n) )
      ; end of loop -------- this is implicitly  step8
       ) ) )

;; The supporting functions:

;;; Use local variable to keep track of the goal.
(let (goal)
  (defun set-goal (the-goal) (setf goal the-goal))
  (defun get-goal () goal) )

;;; EXTRACT-PATH returns the sequence of cities found.
(defun extract-path (n)
  "Returns the path to N."
  (cond ((null n) nil)
        (t (append (extract-path (get-predecessor n))
                   (list n) )) ) )

;;; SUCCESSORS retrieves the list of cities adjacent
;;; to N from N's property list.
(defun successors (n)
  "Returns the list of nodes adjacent to N."
  (get-adj n) ) 

;;; LIST-DIFFERENCE is like the built-in Lisp function
;;; named SET-DIFFERENCE but it preserves the ordering in LST1"
(defun list-difference (lst1 lst2)
  "Returns a list of those elements of LST1 that do not
   occur on LST2."
  (dolist (elt lst2 lst1)
    (setf lst1 (remove elt lst1)) ) )

;;; LONGITUDE-DIFF returns the absolute value of the
;;; difference in longitudes between nodes N1 and N2
;;; in tenths of a degree.
(defun longitude-diff (n1 n2)
  "Computes difference in longitudes."
  (abs (- (get-longitude n1) (get-longitude n2))) )

;;; F evaluates the difference in longitude between
;;; the current node N and the goal node.
(defun f (n)
  "Return diff. in longitude from goal node."
  (longitude-diff n (get-goal)) )

;;; SELECT-BEST chooses a node in step 3...
(defun select-best (lst)
  "Determines the best node to open next."
  (cond ((eql (first lst) (get-goal))(first lst))
        (t (better (first lst)(rest lst))) ) )

;;; The helping function BETTER for select-best checks
;;; to see if there is a goal node on LST with FVALUE
;;; as low as that of ELT.
(defun better (elt lst)
  "Helping function for SELECT-BEST."
  (cond ((null lst) elt)
        ((< (get-f-value elt)(get-f-value (first lst)))
         elt)
        ((eql (first lst) (get-goal))
         (first lst) )
        (t (better elt (rest lst))) ) )

;;; INSERT puts NODE onto LST, which is ordered
;;; by FVALUE property, where VAL is the FVALUE
;;; of NODE.
(defun insert (node lst val)
  "Puts NODE into its proper place on LST."
  (cond ((null lst)(list node))
        ((< val (get-f-value (first lst)))
         (cons node lst))
        (t (cons (first lst)(insert node (rest lst) val))) ) )

;;; Use a local variable EXPANSION-COUNT for counting the
;;; number of nodes expanded by the algorithm.
(let (expansion-count)
  (defun initialize-count () (setf expansion-count 1))
  (defun increment-count () (incf expansion-count))
  (defun get-count () expansion-count) )

;;; TEST sets EXPANSION-COUNT to 0 and
;;; begins a search from RENNES to AVIGNON.
(defun test ()
  "Tests the function BEST-FIRST-SEARCH."
  (initialize-count)
  (format t "Best-first-search solution: ~s.~%"
    (best-first-search 'rennes 'avignon) )
  (format t "~s nodes expanded.~%"
    (get-count) )
  )

(test)

