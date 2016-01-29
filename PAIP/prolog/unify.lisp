;;;; (c) 1991 Peter Norvig - Paradigms of Artificial Intelligence Programming
;;;; unify.lisp - Unification is a extension of the idea of pattern matching
;;;; The difference between simple pattern matching and unification is that
;;;; unification allows two variables to be matched against each other.
;;;; The two variables remain unbound, but they become equivalent.
;;;; Edited by: George Lukas < last update: jan/26/2016 12:23PM >

;;; helper function
(defun reuse-cons (x y x-y &key (test #'eql))
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (funcall test x (first x-y))
	   (funcall test y (rest x-y)))
      x-y
      (cons x y)))

;;; Defining Match
(defconstant +fail+ nil "Indicates MATCH failure.")

(defconstant +no-bindings+ '((t . t))
  "Indicates MATCH success, with no variables.")

(defun variable-p (x)
  "Is X a variable (a symbol beginning with '?')?"
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (first binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (rest binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
	(if (and (eql bindings +no-bindings+)) nil
	    bindings)))

(defun match-variables (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((eql input (binding-val binding)) bindings)
	  (t +fail+))))

;;; Unify dedinictions
(defparameter *occurs-check* t "Should we do the occurs check?")

(defun occurs-check (var x bindings)
  "Does VAR occur anywhere inside x?"
  (cond ((eql var x) t)
	((and (variable-p x) (get-binding x bindings))
	 (occurs-check var (lookup x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))

(defun unify (x y &optional (bindings +no-bindings+))
  "See if X and Y match with given bindings."
  (cond ((eql bindings +fail+) +fail+)
	((eql x y) bindings)
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((and (consp x) (consp y))
	 (unify (rest x) (rest y) (unify (first x) (first y) bindings)))
	(t +fail+)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings)) +fail+)
        (t (extend-bindings var x bindings))))
	
;;; sublis won't work any more, because variables can be bound to other variables,
;;; which are in turn bound to expressions. The function subst-bindings acts like sublis
;;; except that it substitutes recursive bindings.
(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into X,
taking recursively bound variables into account."
  (cond ((eql bindings +fail+) +fail+)
	((eql bindings +no-bindings+) x)
	((and (variable-p x) (get-binding x bindings))
	 (subst-bindings bindings (lookup x bindings)))
	((atom x) x)
	(t (reuse-cons (subst-bindings bindings (first x))
		       (subst-bindings bindings (rest x)) x))))

;;; The function unifier calls unify and substitutes the resulting binding list
;;; into one of the arguments
;;; (UNIFIER '(?x ?y a) '(?y ?x ?x)) => (A A A)
;;; (UNIFIER '((?a * ?x ^ 2) + (?b * ?x) + ?c) '( ?z + (4 * 5) + 3)) => ((?A * 5 ^ 2) + (4 * 5) + 3)
(defun unifier (x y)
  "Return something that unifies with both X and Y (or fail)."
  (subst-bindings (unify x y) x))

;;;; chamando unifier tudo aparenta estar normal, chamando apenas UNIFY tudo retorna nil
