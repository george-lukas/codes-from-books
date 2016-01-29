;;;; AI algorithms, data structures, and idioms in Prolog, Lisp, and Java
;;;; id3.lisp - Chapter19 - Machine Learning in Lisp
;;;; Objectives: ID3 algorithm and inducing decision
;;;; trees from lists of examples.
;;;; This is a basic Lisp implementation of ID3
;;;; Demonstration on a simple credit assessment example
;;;; Edited by: George Lukas < last update: Jan/18/2016 - 06:35PM >

;;; Defining Data-Types
;;; A property is a function on objects; we represent these functions as
;;; a slot in a structure that includes other useful information:
(defstruct property
  name
  test
  values)

;;; We now define decision-tree using the following structures:
(defstruct decision-tree
  test-name
  test
  branches)

(defstruct leaf
  value)

;;; Although a set of training examples is, conceptually,
;;; just a collection of objects, we will make it part of a structure
;;; that includes slots for other information used by the algorithm.
;;; We define example-frame as:
(defstruct example-frame
  instances
  properties
  classifier
  size
  information)

;;; Our final structure definition is partition, a division of an example set
;;; into subproblems using a particular property. We define the type partition:
(defstruct partition
  test-name
  test
  components
  info-gain)

;;; Core of ID3
(defun build-tree (training-frame)
  (cond 
					; Case 1: Empty example set. Create leaf with no classification
    ((zerop (example-frame-size training-frame))
     (make-leaf :value "unable to classify: no examples"))
					; Case 2: All properties used. Create leaf with all remaining classes (may be ambiguous)
    ((null (example-frame-properties training-frame))
     (make-leaf :value (list-classes training-frame)))
					; Case 3: All instances of same class.  Create a leaf    
    ((zerop (example-frame-information training-frame))
     (make-leaf :value (funcall
			(property-test (example-frame-classifier training-frame))
			(first (example-frame-instances training-frame)))))
					; Case 4: Choose test for root of tree & recursively build subtrees
    (t (let ((part (choose-partition (gen-partitions training-frame))))
	 (make-decision-tree
	  :test-name (partition-test-name part)
	  :test (partition-test part)
	  :branches (mapcar #'(lambda (x) 
				(cons (first x) (build-tree (rest x)))) 
			    (partition-components part)))))))

;;; gen-partitions takes one argument, training-frame, an object of
;;; type example-frame-properties, and generates all partitions of its
;;; instances. Each partition is created using a different property from the
;;; properties slot. gen-partitions employs a function, partition,
;;; that takes an instance of an example frame and an instance of a property; it
;;; partitions the examples using that property
(defun gen-partitions (training-frame)
  "Generate all different partitions of an example frame."
  (mapcar #'(lambda (x)
	      (partition training-frame x))
	  (example-frame-properties training-frame)))

(defun choose-partition (candidates)
  "choose-partition searches a list of candidate partitions and chooses the
one with the highest information gain."
  (cond ((null candidates) nil)
	((= (length candidates) 1)
	 (first candidates))
	(t (let ((best (choose-partition (rest candidates))))
	     (if (> (partition-info-gain (first candidates))
		    (partition-info-gain best)) (first candidates) best)))))

;;; Partition takes an example frame and a property;
;;; It partitions the example frame on that property
;;; and returns an instance of a partition structure,
;;; where partition-components is an a-list of (property-value . example-frame) pairs
;;; It also computes the information gain and other statistics
;;; for each component of the partition
(defun partition (root-frame property)
					; Initialize parts to to an a-list of empty example frames
					; indexed by the values of property
  (let ((parts (mapcar #'(lambda (x) (cons x (make-example-frame))) 
		       (property-values property))))
    
					; partition examples on property, placing each example in the appropriate
					; example frame in parts
    (dolist (instance (example-frame-instances root-frame))
      (push instance (example-frame-instances 
                      (rest (assoc (funcall (property-test property) instance) 
				   parts)))))
					; complete information in each component of the partition
    (mapcar #'(lambda (x) 
                (let ((frame (rest x)))
                  (setf (example-frame-properties frame)
                        (remove property (example-frame-properties root-frame)))
                  (setf (example-frame-*classifier* frame)
                        (example-frame-*classifier* root-frame))
                  (setf (example-frame-size frame)
                        (list-length (example-frame-instances frame)))
                  (setf (example-frame-information frame)
                        (compute-information 
                         (example-frame-instances frame)
                         (example-frame-*classifier* root-frame)))))
            parts)
					; return an instance of a partition
    (make-partition 
     :test-name (property-name property)
     :test (property-test property)
     :components parts
     :info-gain (compute-info-gain root-frame parts))))

(defun list-classes (training-frame)
  "Lists all the classes in the instances of a training frame"
					; Eliminate those potential classifications not present
					; in the instances of training frame
  (do ((classes (property-values (example-frame-classifier training-frame))
		(rest classes))
       (classifier (property-test (example-frame-classifier training-frame)))
       classes-present)
      ((null classes) classes-present)
    (if (member (first classes) (example-frame-instances training-frame) 
                :test #'(lambda (x y) (eql x (funcall classifier y))))
	(setf classes-present (cons (first classes) classes-present)))))

(defun compute-information (examples classifier)
  "Computes the information content of a list of examples using a classifier."
  (let ((class-count 
         (mapcar #'(lambda (x) (cons x 0)) (property-values classifier))) 
        (size 0))          
					; count number of instances in each class
    (dolist (instance examples) 
      (incf size)
      (incf (rest (assoc (funcall (property-test classifier) instance) 
			 class-count))))
					; compute information content of examples
    (sum #'(lambda (x) (if (zerop (rest x)) 0
                           (* -1 (/ (rest x) size) 
                              (log (/ (rest x) size) 2)))) 
         class-count)))

(defun sum (f list-of-numbers)
  "Sum takes the sum of applying f to all numbers in list-of-numbers."
  (apply '+ (mapcar f list-of-numbers)))

;;; compute the information gain of a partition
;;; by subtracting the weighted average of the information 
;;; in the children from the information in 
;;; the original set of instances.
(defun compute-info-gain (root parts)
  (- (example-frame-information root)
     (sum #'(lambda (x) (* (example-frame-information (rest x))
                           (/ (example-frame-size (rest x))
                              (example-frame-size root))))
          parts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Classifies an instance using a decision tree
(defun classify (instance tree)
  (if (leaf-p tree) 
      (leaf-value tree)
      (classify instance
		(rest (assoc (funcall (decision-tree-test tree) instance) 
			     (decision-tree-branches tree))))))


