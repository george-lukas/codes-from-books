;;; TEMPORAL.CL
;;; A demonstration of temporal reasoning using constraint propagation
;;; with James Allen's interval algebra.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 8 ("Commonsense Reasoning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; The 13 possible relationships between two intervals are encoded as follows:
;;; "before" <
;;; "after" >
;;; "during" d
;;; "contains" di
;;; "overlaps" o
;;; "overlapped-by" oi
;;; "meets" m
;;; "met-by" mi
;;; "starts" s
;;; "started-by" si
;;; "finishes" f
;;; "finished-by" fi
;;; "equals" =

;;; ------- Data structures ------

;;; The following global variable will contain a list of the
;;; intervals of time involved in the problem.
(defvar *intervals*)

;;; We represent several structures using hash tables.
;;; First, we represent the network of temporal relationships using
;;; a one table.  Then we store the transitivity table as a hash table.
;;; We also establish a queue here for use by the main inference
;;; procedure, via the functions INIT-QUEUE, etc.
(let ((table-of-current-relns
        (make-hash-table :size 20 :test #'equal) )
      (transitivity-table
        (make-hash-table :size 100 :test #'equal) )
      (queue nil) )
  (defun get-temporal-reln (i j)
    "Returns the list of possible temporal relationships
     that might hold between intervals I and J."
    (gethash (list i j) table-of-current-relns) )
  (defun set-temporal-reln (i j relationships)
    "Stores the list of possible temporal relationships
     that might hold between intervals I and J."
    (setf (gethash (list i j) table-of-current-relns)
          relationships) )
  (defun combine-relation (r1 r2)
    "Looks up the transitivity entry in the table."
    (gethash (list r1 r2) transitivity-table) )
  (defun store-combine-relation (r1 r2 relationships)
    "Stores the transitivity entry in the table."
    (setf (gethash (list r1 r2) transitivity-table)
          relationships) )
  (defun init-queue ()
    "Initializes the QUEUE."
    (setf queue nil) )
  (defun insert-into-queue (elt)
    "Puts ELT at end of QUEUE."
    (setf queue (append queue (list elt))) )
  (defun get-from-queue ()
    (pop queue) )
  (defun queue-empty-p ()
    (null queue) )
  )

;;; The following constant represents a list of the 13
;;; primitive temporal relations.
(defconstant primitive-relations
  '(< > d di o oi m mi s si f fi =) )

;;; Next is a variant of this list that omits the EQUALS relation;
;;; it is used by the function that initializes the transitivity table.
(defconstant primitive-relations12
  '(< > d di o oi m mi s si f fi) )

;;; ------- Initialization ------

;;; The following data is used to set up the "Transitivity Table" 
;;; used to propagate constraints using Allen's method.
;;; By putting this information into a hash table, the process of
;;; constraint propagation is made to run faster than it would
;;; if the list structures had to be continually traversed.
;;; One could alternatively implement the table as an array,
;;; in which case each interval would need to be assigned an
;;; integer index.
(let ((entries '(
("before" ( < ) (< > d di o oi m mi s si f fi =)
  ( < o m d s ) ( < ) ( < )
  ( < o m d s ) ( < ) ( < o m d s )
  ( < ) ( < ) ( < o m d s ) ( < ))
("after"  (< > d di o oi m mi s si f fi =) ( > )
  ( > oi mi d f ) ( > ) ( > oi mi d f )
  (>) ( > oi mi d f ) ( > )
  ( > oi mi d f ) ( > ) ( > ) ( > ))
("during" ( < ) ( > )
  ( d ) (< > d di o oi m mi s si f fi =) ( < o m d s )
  ( > oi mi d f ) ( < ) ( > )
  ( d ) ( > oi mi d f ) ( d ) ( < o m d s ))
("contains" ( < o m di fi ) ( > oi di mi si )
  ( o oi d s f di si fi = ) ( di ) ( o di fi )
  ( oi di si ) ( o di fi ) ( o di si )
  ( di fi o ) ( di ) ( di si oi ) ( di ))
("overlaps" ( < ) ( > oi di mi si )
  ( o d s ) ( < o m di fi ) ( < o m )
  ( o oi d s f di si fi = ) ( < ) ( oi di si )
  ( o ) ( di fi o ) ( d s o ) ( < o m ))
("overlapped-by" ( < o m di fi ) ( > )
  ( oi d f ) ( > oi mi di si ) ( o oi d s f di si fi = )
  ( > oi mi ) ( o di fi ) ( > )
  ( oi d f ) ( oi > mi ) ( oi ) ( oi di si))
("meets" ( < ) ( > oi mi di si )
  ( o d s ) ( < ) ( < )
  ( o d s ) ( < ) ( f fi = )
  ( m ) ( m ) ( d s o ) ( < ))
("met-by" ( < o m di fi ) ( > )
  ( oi d f ) ( > ) ( oi d f )
  ( > ) ( s si = ) ( > )
  ( d f oi ) ( > ) ( mi ) ( mi ))
("starts" ( < ) ( > )
  ( d ) ( < o m di fi ) ( < o m )
  ( oi d f ) ( < ) ( mi )
  ( s ) ( s si = ) ( d ) ( < m o ))
("started-by" ( < o m di fi ) ( > )
  ( oi d f ) ( di ) ( o di fi )
  ( oi ) ( o di fi ) ( mi )
  ( s si = ) ( si ) ( oi ) ( di ))
("finishes" ( < ) ( > )
  ( d ) ( > oi mi di si ) ( o d s )
  ( > oi mi ) ( m ) ( > )
  ( d ) ( > oi mi ) ( f ) ( f fi = ))
("finished-by" ( < ) ( > oi mi di si )
  ( o d s ) ( di ) ( o )
  ( oi di si ) ( m ) ( si oi di )
  ( o ) ( di ) ( f fi = ) ( fi ))
) ))
  (defun init-combining-table ()
    "Stores James Allen's transitivity table entries in
     a hash table."
    (let ((pr-left primitive-relations12)
          ps-left r)
      (dolist (row entries)
        (pop row)
        (setf r (pop pr-left))
        (setf ps-left primitive-relations12)
        (dolist (entry row)
          (store-combine-relation r (pop ps-left) entry)
          )
        (store-combine-relation r '= (list r))
        )
     (dolist (c primitive-relations)
        (store-combine-relation '= c (list c)) )
      ) ) )

;;; The next function simply prints out the entries of
;;; the combining table, and it is used in debugging.      
(defun print-combining-table ()
  "Prints the table (for debugging)."
  (format t "~%The Transitivity Table:")
    (dolist (row primitive-relations)
      (format t "~%")
      (dolist (col primitive-relations)
        (prin1 (combine-relation row col))
        (format t " ")
      ) ) )

;;; The function below gives initial values to all of the
;;; hashtable entries for temporal relationships on the intervals.
;;; For each pair of intervals I and J, the entry is either
;;; (=) if I = J, or it is the value of PRIMITIVE-RELATIONS,
;;; which means "nothing is known yet" or "anything is possible."
(defun init-current-relationships ()
  "Sets up all the relationships as unconstrained."
  (dolist (i *intervals*)
    (dolist (j *intervals*)
      (if (eql i j)
          (set-temporal-reln i j '(=))
          (set-temporal-reln i j primitive-relations) )
    ) ) )

;;; ------- Updating functions ------

(defun constraints (relationships1 relationships2)
  "Returns the set of constraints on A and C implied by
   this pair of relationships, where A can be related to B
   by RELATIONSHIPS1 and B can be related to C by RELATIONSHIPS2."
  (let ((c nil))
    (dolist (r1 relationships1)
      (dolist (r2 relationships2)
        (setf c (union c (combine-relation r1 r2))) ) )
    c) )

(defun add (i j relationships)
  "Takes the new list of possible RELATIONSHIPS between
   interval I and interval J and propagates this constraint
   to the other stored relationships."
  (let* ((old (get-temporal-reln i j))
         (new (intersection relationships old)) )
    (if (not (set-equal new old))
      (progn (set-temporal-reln i j new)
             (insert-into-queue (list i j)) ) ) )
  (loop
    (if (queue-empty-p) (return))
    (let* ((pair (get-from-queue))
           (i (first pair))
           (j (second pair)) )
      (dolist (k *intervals*)
        (try-to-update i j k)
        (try-to-update k i j)
       ) ) ) )

(defun try-to-update (x y z)
  "Handles updating tests and action for ADD."
  (let ((temp
        (intersection (get-temporal-reln x z)
                      (constraints (get-temporal-reln x y)
                                   (get-temporal-reln y z) ) )))
    (if (null temp) (signal-inconsistent x y z))
    (if (not (set-equal temp (get-temporal-reln x z)))
        (progn (insert-into-queue (list x z))
               (set-temporal-reln x z temp) ) ) ) )
 
(defun add-both (i j relationships)
  "Registers the constraints on I and J given by
   RELATIONSHIPS, including the implied inverse
   relationships."
  (add i j relationships)
  (add j i (invert-relationships relationships)) )

(defun set-equal (set1 set2)
  "Returns T if the sets contain the same elements."
  (null (set-exclusive-or set1 set2)) )

(defun invert-primitive (p)
  "Returns the inverse relation for P."
  (second (assoc p
    '((< >)(> <)(d di)(di d)(o oi)(oi o)(m mi)(mi m)
      (s si)(si s)(f fi)(fi f)(= =) ) )) )

(defun invert-relationships (r)
  "Returns the set of inverse relationships of elements of R."
  (mapcar #'invert-primitive r) )

(defun signal-inconsistent (x y z)
  "Prints a message about a temporal inconsistency."
  (format t "~%NOTE: A CONTRADICTION HAS BEEN FOUND ")
  (format t "~%among intervals ~S, ~S, and ~S.~%"
          x y z) )

(defun print-temporal-relationships ()
  "Prints out for every pair of intervals what is currently
   explicity represented about their temporal relationship."
  (format t "~%The current interval relationships are:")
  (dolist (i *intervals*)
    (dolist (j *intervals*)
    (format t "~%")
      (format t "Interval ~S and interval ~S: ~S"
         i j (get-temporal-reln i j)) ) ) )

;;; ------- Here is a test example ------


(setf *intervals* '(owner-armed-security-system 
                    alarm-armed
                    robber-in-store))

(defun test ()
  "Demonstrates simple temporal reasoning."
  (init-combining-table)
  (init-current-relationships)
  (init-queue)
  (add-both 'owner-armed-security-system
            'alarm-armed
            '(m o) )
  (print-temporal-relationships)
  (add-both 'owner-armed-security-system
            'robber-in-store
            '(< m mi >) )
  (print-temporal-relationships)
  (add-both 'alarm-armed
            'robber-in-store
            '(o s d) )
  (print-temporal-relationships)
  )


 