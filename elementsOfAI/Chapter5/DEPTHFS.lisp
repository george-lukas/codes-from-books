;;; DEPTHFS.CL
;;; Depth-First Search.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 5 ("Search") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; the graph to be searched represents roads in France.

;;; Create a hash table ADJACENCY-INFO to store the French
;;; roads data, and define the functions for entering and
;;; retrieving this info.
;;; A second hash table, PATH-PREDECESSOR-INFO, is used
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

;;; DEPTH-FIRST-SEARCH is the main searching procedure.
;;; Note that we use Common Lisp macros PUSH and POP to simplify
;;; adding and removing elements at the front of lists.
(defun depth-first-graph-search (start-node goal-node)
  "Performs a depth-first search from START-NODE for GOAL-NODE."
  (let ((open (list start-node))                ;step1
        (closed nil)
        n l)
    (loop
      (if (null open)(return 'failure))         ;step2
      (setf n (pop open))                       ;step3
      (push n closed)
      (increment-count)
      (if (eql n goal-node)
          (return (extract-path n)) )
      (setf l (successors n))                   ;step4
      (setf l (list-difference l closed))
      (setf open
            (append l (list-difference open l)));step5
      (dolist (x l)
              (set-predecessor x n) )
      ; end of loop -------- this is implicitly  step6
       ) ) )

;; The supporting functions:

;;; EXTRACT-PATH returns the sequence of cities found.
(defun extract-path (n)
  "Returns the path leading to N."
  (cond ((null n) nil)
        (t (append (extract-path (get-predecessor n))
                   (list n) )) ) )

;;; SUCCESSORS retrieves the list of cities adjacent
;;; to N from N's property list.
(defun successors (n)
  "Returns a list of nodes adjacent to N."
  (get-adj n) )

;;; LIST-DIFFERENCE is like the built-in Lisp function
;;; named SET-DIFFERENCE but it preserves the ordering in LST1"
(defun list-difference (lst1 lst2)
  "Returns a list of those elements of LST1 that do not
   occur on LST2."
  (dolist (elt lst2 lst1)
    (setf lst1 (remove elt lst1)) ) )

;;; Use a local variable EXPANSION-COUNT for counting
;;; the number of nodes expanded by the algorithm.
(let (expansion-count)
  (defun initialize-count () (setf expansion-count 0))
  (defun increment-count () (incf expansion-count))
  (defun get-count () expansion-count) )

;;; TEST sets the count of nodes expanded to 0 and
;;; begins a search from RENNES to AVIGNON.
(defun test ()
  "Tests the function DEPTH-FIRST-SEARCH."
  (initialize-count)
  (format t "Depth-first-search solution: ~s.~%"
    (depth-first-graph-search 'rennes 'avignon) )
  (format t "~s nodes expanded.~%"
    (get-count))
  )

(test)


