;;;; (c) 1991 Peter Norvig - Paradigms of Artificial Intelligence Programming
;;;; prlgi.lisp - The Prolog interpreter
;;;; Edited by: George Lukas < last update: jan/26/2016 01:17PM >
;;;; Usage: Type ; seek moore solutions or . to stop
;;; > (?- (likes Sandy ?who)) => ?WHO = KIM. No.

;;; This requires (load ~/unify)

;;; Clauses are represented as (head . body) cons cells
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

;;; Clauses are stored on the predicates's plist
(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation) (first relation))
(defun args (x) "The numbers arguments of a relation" (rest x))

(defvar *db-predicates* nil
  "A list of all predicates stored in the database.")

;;; Now we need a way of adding a new clause. The work is split up into the macro <- ,
;;; which provides the user interface, and a function, add-clause, that does the work.
;;; It is worth defining a macro to add clauses because in effect we are defining a new
;;; language: Prolog-In-Lisp. This language has only two syntactic constructs: the <-
;;; macro to add clauses, and the ?- macro to make queries.
(defun add-clause (clause)
  "add a clause to the data base, indexed by head's predicate."
  ;; the predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses) (nconc (get-clauses pred) (list clause)))
    pred))
    
;;; Functions to update Clauses
(defun clear-predicate (predicate)
  "Remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))

(defun clear-db ()
  "Remove all clauses (for all predicades) from the database."
  (mapc #'clear-predicate *db-predicates*))

;;; ====
(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree)
				found-so-far))))
(defun non-anon-variable-p (x)
  (and (variable-p x) (not (eql x '?))))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'non-anon-variable-p exp))

(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
		  (variables-in x)) x))

(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eql bindings +fail+) +fail+)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))
  
(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
        (some
	 #'(lambda (clause)
	     (let ((new-clause (rename-variables clause)))
	       (prove-all
		(append (clause-body new-clause) other-goals)
		(unify goal (clause-head new-clause) bindings))))
	 clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (funcall clauses (rest goal) bindings
                 other-goals))))

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise 
     (format t "Type ; to see more or . to stop")
     (continue-p))))

(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null vars) (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var (subst-bindings bindings var))))
  (if (continue-p) +fail+
      (prove-all other-goals bindings)))

;;; top-level-prove adds the primitive goal show-prolog-vars
;;; to the end of the list of goals
(defun top-level-prove (goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
             +no-bindings+)
  (format t "~&No.")
  (values))

;;;; Since primitives are represented as entries on the clauses property of predicate
;;; symbols, we have to register show-prolog-vars as a primitive like this:
(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)

(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eql exp '?) (gensym "?"))
	((atom exp) exp)
	(t (reuse-cons (replace-?-vars (first exp))
		       (replace-?-vars (rest exp))
		       exp))))

(defmacro <- (&rest clause)
  "Add a clause to the database."
  `(add-clause ',(replace-?-vars clause)))

(defmacro ?- (&rest goals)
  "Make a query and print answers."
  `(top-level-prove ',(replace-?-vars goals)))
