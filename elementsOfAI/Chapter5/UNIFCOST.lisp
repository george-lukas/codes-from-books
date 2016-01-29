;;; UNIFCOST.CL
;;; Uniform-Cost search.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 5 ("Search") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; This is a variation on breadth-first search for a shortest path.
;;; No evaluation function is used, but the actual route
;;; distances are used to order the search.  Thus the search
;;; is analogous to "breadth-first search" where new nodes are
;;; opened in order of increasing distance from the start node.

;;; Here we combine the adjacency data and the distance data
;;; and represent it in one hash table DISTANCE-INFO.
(let ((distance-info (make-hash-table :size 20)) )
  (defun set-distances (x y)
    (setf (gethash x distance-info) y) )
  (defun get-distances (x)
    (gethash x distance-info) )
  )

;;; The second hash table, PATH-PREDECESSOR-INFO, is used
;;; as in DEPTHFS.CL, etc.
(let ((path-predecessor-info (make-hash-table :size 20)) )
  (defun set-predecessor (x y)
    (setf (gethash x path-predecessor-info) y) )
  (defun get-predecessor (x)
    (gethash x path-predecessor-info) )
  )

;;; Here are actual inter-city distances from the Michelin map:
(set-distances 'brest '((rennes . 244)))
(set-distances 'rennes '((caen . 176)(paris . 348)
		(brest . 244)(nantes . 107)))
(set-distances 'caen '((calais . 120)(paris . 241)(rennes . 176)))
(set-distances 'calais '((nancy . 534)(paris . 297)(caen . 120)))
(set-distances 'nancy '((strasbourg . 145)(dijon . 201)
		(paris . 372)(calais . 534)))
(set-distances 'strasbourg '((dijon . 335)(nancy . 145)))
(set-distances 'dijon '((strasbourg . 335)(lyon . 192)
		(paris . 313)(nancy . 201)))
(set-distances 'lyon '((grenoble . 104)(avignon . 216)
		(limoges . 389)(dijon . 192)))
(set-distances 'grenoble '((avignon . 227)(lyon . 104)))
(set-distances 'avignon '((grenoble . 227)(marseille . 99)
		(montpellier . 121)(lyon . 216)))
(set-distances 'marseille '((nice . 188)(avignon . 99)))
(set-distances 'nice '((marseille . 188)))
(set-distances 'montpellier '((avignon . 121)(toulouse . 240)))
(set-distances 'toulouse '((montpellier . 240)(bordeaux . 253)
		(limoges . 313)))
(set-distances 'bordeaux '((limoges . 220)(toulouse . 253)
		(nantes . 329)))
(set-distances 'limoges '((lyon . 389)(toulouse . 313)
		(bordeaux . 220)(nantes . 329)(paris . 396)))
(set-distances 'nantes '((limoges . 329)(bordeaux . 329)
		(rennes . 107)))
(set-distances 'paris '((calais . 297)(nancy . 372)(dijon . 313)
		(limoges . 396)(rennes . 348)(caen . 241)))

;;; And here is the hash table F-VALUE to
;;; remember the heuristic value at each node visited.
(let ((f-values (make-hash-table :size 20)))
  (defun set-f-value (x y)
    (setf (gethash x f-values) y) )
  (defun get-f-value (x)
    (gethash x f-values) )
  )

;;; UNIFORM-COST is the main searching procedure.
(defun uniform-cost (start-node goal-node)
  "Performs a uniform-cost search from START-NODE
   to GOAL-NODE."
  (set-goal goal-node)
  (let ((open (list start-node))                ;step1
        (closed nil)
        n l)
    (set-predecessor start-node nil)
    (set-f-value start-node 0)
    (loop
      (if (null open)(return 'failure))         ;step2
      (setf n (select-best open))               ;step3
      (setf open (remove n open))               ;step4
      (push n closed)
      (if (eql n goal-node)                     ;step5
          (return (extract-path n)) )
      (setf l (get-successors n))               ;step6
      (dolist (m (list-difference l closed))
        (let ((temp (f m n)))
          (if (member m open)
            (if (< temp (get-f-value m))
              (progn
                (set-f-value m temp)
                (set-predecessor m n)
                (setf open (insert m (remove m open)))
                (increment-count) ) )
            (progn
              (set-f-value m temp)
              (set-predecessor m n)
              (setf open (insert m open))
              (increment-count) ) ) ) )
      ; end of loop -------- this is implicitly  step7
       ) ) )

;; The supporting functions:

;;; Use local variable to keep track of the goal.
(let (goal)
  (defun set-goal (the-goal) (setf goal the-goal))
  (defun get-goal () goal) )

;;; EXTRACT-PATH returns the sequence of cities found.
(defun extract-path (n)
  "Returns the list of nodes from the Start node to N."
  (if (null n) nil
    (append (extract-path (get-predecessor n))
            (list n) ) ) )

;;; GET-SUCCESSORS retrieves the list of cities adjacent
;;; to N from the hash table.
(defun get-successors (n)
  "Returns a list of cities adjacent to N."
  (mapcar #'first (get-distances n)) )

;;; LIST-DIFFERENCE is like the built-in Lisp function
;;; named SET-DIFFERENCE but it preserves the ordering in LST1"
(defun list-difference (lst1 lst2)
  "Returns a list of those elements of LST1 that do not
   occur on LST2."
  (dolist (elt lst2 lst1)
    (setf lst1 (remove elt lst1)) ) )

;;; ARC-DIST retrieves the distance between N1 and N2.
(defun arc-dist (n1 n2) 
  "Returns the distance along arc N1 N2. If no such arc
   exists, returns 9999."
  (or (rest (assoc n1 (get-distances n2))) 9999) )

;;; F computes the distance from the start node to NODE
;;; by adding the distance from N to NODE to N's distance.
(defun f (node n)
  "Returns the F value for NODE using N."
  (+ (get-f-value n) (arc-dist n node)) )

;;; SELECT-BEST chooses a node in step 3...
(defun select-best (lst)
  "Returns the best node on LST for expansion."
  (if (eql (first lst) (get-goal))
      (first lst)
    (better (first lst)(rest lst)) ) )

;;; The helping function BETTER for SELECT-BEST checks
;;; to see if there is a goal node on LST with FVALUE
;;; as low as that of ELT.  If so, it returns the goal node.
;;; If not, it returns ELT.
(defun better (elt lst)
  "Returns a goal-node on LST if it has an equal value,
   otherwise returns ELT."
  (cond ((null lst) elt) ; no more possibilities, return ELT.
        ((< (get-f-value elt)
            (get-f-value (first lst)) )
         elt) ; ELT is better.
        ((eql (first lst) (get-goal))
         (first lst) ) ;return a goal node.
        (t (better elt (rest lst))) ; same value and not
                                    ; goal -try next on LST.
    ) )

;;; INSERT puts NODE onto LST, which is ordered
;;; by FVALUE property.
(defun insert (node lst)
  "Inserts NODE onto LST, according to FVALUE ordering."
  (cond ((null lst)(list node))
        ((< (get-f-value node) (get-f-value (first lst)))
         (cons node lst) )
        (t (cons (first lst)(insert node (rest lst)))) ) )

;;; Use a local variable EXPANSION-COUNT for counting the
;;; number of nodes expanded by the algorithm.
(let (expansion-count)
  (defun initialize-count () (setf expansion-count 0))
  (defun increment-count () (incf expansion-count))
  (defun get-count () expansion-count) )

;;; TEST sets EXPANSION-COUNT to 0 and
;;; begins a search from RENNES to AVIGNON.
(defun test ()
  "Runs a test of UNIFORM-COST."
  (initialize-count)
  (format t "Uniform-cost-search solution: ~s.~%"
    (uniform-cost 'rennes 'avignon) )
  (format t "Path-length: ~s.~%" 
    (get-f-value 'avignon) )
  (format t "~s nodes expanded.~%"
    (get-count) )
  )

(test)

