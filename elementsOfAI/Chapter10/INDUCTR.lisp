;;; INDUCTR.CL
;;; Induces decision trees using the basic method of CLS and ID3.
;;; Demonstrates machine learning of classification rules from examples.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 10 ("Learning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Part I: The representation of examples.

;;; concept: RED-CIRCLE ...
(defvar *colored-figures*)

(let
  ((ex1 '(+ red-circle (shape circle) (color red) (size large)))
   (ex2 '(- red-circle (shape triangle) (color red) (size small)))
   (ex3 '(- red-circle (shape square) (color green) (size large)))
   (ex4 '(- red-circle (shape triangle) (color blue) (size large)))
   (ex5 '(- red-circle (shape circle) (color blue) (size large)))
   (ex6 '(+ red-circle (shape circle) (color red) (size large)))
   (ex7 '(+ red-circle (shape circle) (color red) (size small))) )

  (setf *colored-figures* (list ex1 ex2 ex3 ex4 ex5 ex6 ex7)) )


;;; Part II: The algorithm, based on the AI Handbook, Vol. III, p407.

;;; Build a decision tree that handles EXAMPLES and
;;; employs attributes in ATTRIBUTES-LEFT...
(defun build-tree (examples attributes-left)
  "Returns a decision tree that handles EXAMPLES and asks about
   attributes in ATTRIBUTES-LEFT."
  (cond
    ((all-positive examples) (new-node 'leaf '+))
    ((all-negative examples) (new-node 'leaf '-))
    ((null attributes-left) (new-node 'leaf 'cannot-tell))
    (t (let* ((attrib  ; choose best attribute.
                (best-attribute examples
                                attributes-left) )
              (node    ; create an interior node.
                (new-node 'interior attrib) ) )
         ;; Now create a subtree for each value of ATTRIB
         ;; that occurs in EXAMPLES.
         (loop
           (if (null examples) (return))
           (let*
             ((this-value
                ;; get an actual value for this attribute
                ;; using 1st of remaining examples.
                (value (first examples) attrib) )
              (this-subset
                ;; find all examples with this property.
                (the-subset examples attrib this-value) )
              (child
                ;; build the subtree for this value.
                (build-tree
                  this-subset
                  (remove attrib attributes-left) ) ) )
             ;; now link the subtree to the new node...
             (add-child node child)
             ;; place the value (the arc label) at the child...
             (setf (get child 'incoming-arc-value) this-value)
             ;; remove the subset of examples from EXAMPLES
             (setf examples
               (set-difference examples this-subset
                 :test #'equal) ) ) )
         ;; Return the new node as the root of the new subtree...
         node) ) ) )

;;; ALL-POSITIVE returns T if all its examples are positive.
(defun all-positive (examples)
  "Checks whether all EXAMPLES are positive."
  (cond ((null examples) t)
        ((null (eql (first (first examples)) '+)) nil)
        (t (all-positive (rest examples))) ) )

;;; ALL-NEGATIVE returns T if all its examples are negative.
(defun all-negative (examples)
  "Checks whether all EXAMPLES are negative."
  (cond ((null examples) t)
        ((null (eql (first (first examples)) '-)) nil)
        (t (all-negative (rest examples))) ) )

;;; NEW-NODE creates the representation of a new node.
(defun new-node (node-type label)
  "Returns a new symbol after having NODE-TYPE and LABEL
   put on its property list."
  (let ((node (gensym "Node-")) )     ; generate a new symbol.
    (setf (get node 'type) node-type) ; save node type on plist.
    (setf (get node 'label) label)    ; save label on plist.
    node) )                           ; return the new symbol.

;;; Choose the best attribute among ATTRIBUTES-LEFT,
;;; based on the EXAMPLES subset.
(defun best-attribute (examples attributes-left)
  "Returns that member of ATTRIBUTES-LEFT that it would be
   most advantageous to ask about in order to classify
   EXAMPLES."
  (let ((ratings (rate-attributes examples attributes-left)))
    ;; show the ratings...
    (format t "~%Attribute ratings are:~%")
    (mapc #'print-rating ratings)
    ;; Select that with lowest expected entropy...
    (rest (select-best ratings)) ) )

(defun print-rating (rating)
  "Prints one (entropy . attribute) pair."
  (format t "(~7,4f ~A) " (first rating) (rest rating)) )

(defun range-of (attribute examples)
  "Returns the set of values that ATTRIBUTE takes on in
   the given set of examples."
  (elim-dups (mapcar #'(lambda (ex) (value ex attribute))
                     examples)) )

(defun rate-one-attribute (attribute examples)
  (let*
    ((range (range-of attribute examples))
     (num-examples (length examples))
     (expected-entropy 0.0) ) ; to hold the expected-entropy value.
    (cons
      ;; Sum the components of the expected entropy...
      ;; For each value of the attribute go through the loop,
      ;;  and at the end of the loop return the EXPECTED-ENTROPY...
      (dolist (val range expected-entropy)
        ;; Compute PROB as the ratio of number of positives to all
        ;; examples within the subset in the possible new subtree.
        (let*
          ((num-in-subset
             (length (the-subset examples attribute val)) )
           (prob
             (/ (count-pos-examples examples attribute val)
                num-in-subset) )
           (branch-weight (/ num-in-subset num-examples)) )
          (incf expected-entropy
                (* branch-weight (entropy prob)) )
          (format t "~%Attribute is ~S, value is ~7,4f,"
                  attribute val)
          (format t " entropy is ~7,4f. Weight is ~7,4f."
                  (entropy prob) branch-weight)
             ) )
      attribute)
    ) )

;;; RATE-ATTRIBUTES returns a list of dotted pairs, each of the
;;; form (EXPECTED-ENTROPY . ATTRIBUTE).  Construct one...
(defun rate-attributes (examples attributes-left)
  "Determine the expected entropy for each of ATTRIBUTES-LEFT in
   classifying EXAMPLES."
  (let ((ratings nil))
    (dolist (this-attribute attributes-left ratings)
      (push (rate-one-attribute this-attribute examples)
            ratings) ) ) )

;;; Eliminate any duplicates in list S.
(defun elim-dups (s)
  "Turns a multiset into a set."
  (cond ((null s) nil)
        ((member (first s) (rest s)) (elim-dups (cdr s)))
        (t (cons (first s) (elim-dups (rest s)))) ) )

;;; Count the number of positive examples in which the attribute
;;; has the given value. This is used to establish the probability
;;; of a positive instance within the subset.
(defun count-pos-examples (examples attribute value)
  "Returns a count of the number of EXAMPLES that are both
   positive and have their ATTRIBUTE's value match VALUE."
  (if examples
    (let ((n-others
            (count-pos-examples (rest examples) attribute value) ))
      (if (and ;; Make sure the example is a positive one:
            (eql (first (first examples)) '+)
            ;; whose value matches VALUE:
            (equal (value (first examples) attribute) value) )
         ;; OK, count it:
         (1+ n-others)
         n-others) )
    0) )


;;; ENTROPY takes a probability and returns the entropy
;;; associated with the probability and its complement.
;;; Entropy = - p log2 p - (1-p)log2 (1-p).
(defun entropy (p)
  "Returns the entropy associated with P and NOT P."
  (let ((entropy 0.0))
    (if (not (= p 0))
        (decf entropy (* p (log p 2))) )
    (if (not (= (- 1 p) 0))
        (decf entropy (* (- 1 p) (log (- 1 p) 2))) )
    entropy) )

;;; SELECT-BEST is like MIN except that each rating is in a dotted
;;; pair (RATING . ATTRIB).
(defun select-best (ratings)
  "Returns the best (expected-entropy, attribute) rating
   of those within RATINGS."
  (let ((best (first ratings)))
    (if (rest ratings)
        (let ((temp (select-best (rest ratings))))
          (if (< (first temp)(first best))
              (return-from select-best temp) ) ) )
    best) )

;;; VALUE takes an EXAMPLE and an ATTRIBUTE and returns the
;;; corresponding value.
(defun value (example attribute)
  "Accesses the value of ATTRIBUTE for a given EXAMPLE."
  (second (assoc attribute (rest (rest example)))) )

;;; Return the subset of EXAMPLES that have the value
;;; THIS-VALUE for this ATTRIB.
(defun the-subset (examples attrib this-value)
  "Selects those EXAMPLES having THIS-VALUE for ATTRIBUTE."
  (cond ((null examples) nil)
        ((eql this-value (value (first examples) attrib))
         (cons (first examples)
               (the-subset (rest examples) attrib this-value) ) )
        (t (the-subset (rest examples) attrib this-value)) ) )

;;; ADD-CHILD connects the CHILD to the NODE
(defun add-child (node child)
  "Creates a link from parent (NODE) to CHILD."
  (push child (get node 'children)) )

;;; APPLY-TREE applies a decision tree (TREE) to test whether
;;; EXAMPLE belongs to the positive category.
(defun apply-tree (tree example)
  "Returns T if the classification of EXAMPLE
   according to TREE is positive."
  (cond ((eql (get tree 'label) '+) t)
        ((eql (get tree 'label) '-) nil)
        (t (apply-tree1 (get tree 'children)
                     (get tree 'label)
                     example)) ) )

;;; APPLY-TREE1 tries each of the CHILDREN and if the EXAMPLE has the
;;; right value for the ATTRIBUTE it calls APPLY-TREE recursively.
(defun apply-tree1 (children attribute example)
  "Helps APPLY-TREE with recursive calls."
  (dolist (child children)
    (if (eql (get child 'incoming-arc-value)
             (value example attribute) )
        (return-from apply-tree1
                     (apply-tree child example) ) ) ) )

;;; PRINT-TREE prints out a description of the decision tree (TREE).
;;; The root is printed first, with zero indentation. The label NIL on
;;; it simply indicates that no arc comes into the root.
;;; Each subtree is indented according to its depth from the root.
(defun print-tree (root)
  "Outputs a sideways diagram of the tree rooted at ROOT."
  (format t "~%Rooted at ~A is the decision tree:" root)
  (print-tree1 root 0) )

;;; helping function...
;;; Each node is printed in the form INCOMING-ARC-VALUE, LABEL.
;;;  For example,  CIRCLE  COLOR.
;;; This means that this is the subtree for circles and the
;;;  attribute to test at this node is color.
;;; Another example of a node description is RED, +
;;;  which indicates this is a subtree for red objects
;;;  and all the examples in this subtree are positive. 
(defun print-tree1 (root level)
  "Prints out a subtree at level LEVEL."
  (terpri)
  (dotimes (i level)(format t "  ")) ; indent according to LEVEL.
  (format t " ~A  " (get root 'incoming-arc-value))
  (format t " ~A  " (get root 'label))
  (dolist (child (get root 'children))
    (print-tree1 child (1+ level)) ) ) ; recursive call for each subtree.

(defvar *tree*)
(setf *tree* (build-tree *colored-figures* '(shape color size)))

(print-tree *tree*)  ; show the resulting tree.

(apply-tree *tree* (first *colored-figures*))

;------------------------------------------------
;;; Part III:  Another set of examples.
;;; Concept: FAVORITE restaurants.

;;; Examples of restaurants:
(defvar *restaurants*)
(let ((ivars '(+ favorite (service table) (price high)
                (location wallingford) (cuisine seafood) ))
      (skippers '(- favorite (service fast-food) (price low)
                (location udist) (cuisine seafood) ))
      (macdonalds '(- favorite (service fast-food) (price low)
                (location udist)(cuisine burgers) ))
      (kidd-valley '(+ favorite (service fast-food) (price low)
                (location greenlake)(cuisine burgers) ))
      (beeliner '(- favorite (service diner) (price medium)
                (location wallingford)(cuisine misc) ))
      (rays '(+ favorite (service table) (price high)
                (location shilshole)(cuisine seafood) ))
      (anthonys '(+ favorite (service table) (price high)
                (location shilshole)(cuisine seafood) ))
      (bangkok '(+ favorite (service table) (price low)
                (location udist)(cuisine thai) ))
      (china-first '(- favorite (service table) (price low)
                (location udist)(cuisine chinese) )) )

  (setf *restaurants* 
        (list ivars skippers macdonalds kidd-valley beeliner
           rays anthonys bangkok china-first) ) )

(defvar *favorite-d-t*)
(setf *favorite-d-t*
      (build-tree *restaurants* '(service price location cuisine)) )

(print-tree *favorite-d-t*)
