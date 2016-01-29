;;;; Chapter 4: Utility functions
;;;; George Lukas < last update: Jan/29/2016 - 05:20PM >

;;; Operations on lists

;;; Small functions which operate on lists
(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (first (last lst)))

(defun single (lst)
  (and (consp lst) (not (rest lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;;; Larger functions that operate on lists
(defun longer (x y)
  "LONGER is a recursive function to compare the lengths of
two lists."
  (labels ((compare (x y)
	     (and (consp x)
		  (or (null y)
		      (compare (rest x) (rest y))))))
    (if (and (listp x) (listp y))
	(compare x y)
	(> (length x) (length y)))))

;;; (FILTER #'(lambda (x) (if (numberp x) (1+ x))) '(a 1 2 b 3 c d 4)) => (2 3 4 5)
(defun filter (fn lst)
  "You give FILTER a function and a list, and get back a list of whatever non-nil
values are returned by the function as it is applied to the elements of the list"
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

;;; (GROUP '(a b c d e f g) 2) => ((A B) (C D) (E F) (G))
(defun group (lst n)
  "GROUP is for grouping lists into sublists. You give GROUP 
a list l and a number n, and it will return a new list in which the elements
of l are grouped into sublists of length n"
  (if (zerop n) (error "zero length."))
  (labels ((rec (lst acc)
	     (let ((rest (nthcdr n lst)))
	       (if (consp rest)
		   (rec rest (cons (subseq lst 0 n) acc))
		   (nreverse (cons lst acc))))))
    (if lst (rec lst nil) nil)))

;;; Doubly-recursive list utilities

;;; (flatten '(a (b c) ((d e) f))) => (A B C D E F)
(defun flatten (x)
  "It returns a list of all theatoms that are elements of a list,
or elements of its elements."
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (first x) (rec (rest x) acc))))))
    (rec x nil)))

;;; (prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9))) => (1 (3 (5)) 7 (9))
(defun prune (test tree)
  "prune, is to remove-if as copy-tree is to
copy-list. That is, it recurses down into sublists."
  (labels ((rec (tree acc)
	     (cond ((null tree) (nreverse acc))
		   ((consp (first tree))
		    (rec (rest tree)
			 (cons (rec (first tree) nil) acc)))
		   (t (rec (rest tree)
			   (if (funcall test (first tree)) acc
			       (cons (first tree) acc)))))))
    (rec tree nil)))

;;; Functions wich search lists

;;; (find2 #'evenp '(1 2 3)) => 2 T
(defun find2 (fn lst)
  "find2, is the one we defined in response to it."
  (if (null lst) nil
      (let ((val (funcall fn (first lst))))
	(if val (values (first lst) val)
	    (find2 fn (rest lst))))))

;;; (before 'b 'd '(a b c d)) => (B C D)
(defun before (x y lst &key (test #'eql))
  "It tells you if one object is found before another in a list."
  (and lst
       (let ((first (first lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (rest lst) :test test))))))

;;; (AFTER 'a 'b '(b a d)) => (A D)
(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

;;; (duplicate 'a '(a b c a d)) => (A D)
(defun duplicate (obj lst &key (test #'eql))
  "If (member o l) finds o in the list l, it also returns the cdr of l beginning
with o. This return value can be used, for example, to test for duplication. If o is
duplicated in l, then it will also be found in the cdr of the list returned by member.
This idiom is embodied in the next utility, DUPLICATE"
  (member obj (rest (member obj lst :test test))
	  :test test))

;;; (SPLIT-IF #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7 8 9 10)) => (1 2 3 4) (5 6 7 8 9 10)
(defun split-if (fn lst)
  "SPLIT-IF is also a kind of generalization of MEMBER.
While MEMBER returns the cdr of the list beginning with the element it finds,
split-if returns both halves"
  (let ((acc nil))
    (do ((src lst (rest src)))
	((or (null src) (funcall fn (first src)))
	 (values (nreverse acc) src))
      (push (first src) acc))))

;;;  Search functions wich compare elements

;;; (most #'length '((a b) (a b c) (a) (e f g))) => (A B C) 3
(defun most (fn lst)
  "MOST, looks at one element at a time. It
takes a list and a scoring function, and returns the element with the highest score.
In case of ties, the element occurring first wins."
  (if (null lst) (values nil nil)
      (let* ((wins (first lst))
	     (max (funcall fn wins)))
	(dolist (obj (rest lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setq wins obj
		    max score))))
	(values wins max))))

;;; (BEST #'> '(1 2 3 4 5)) => 5
(defun best (fn lst)
  "We can think of BEST as being equivalent to FIRST of SORT.
It is up to the caller to provide a predicate which defines a total order on the
elements of the list. Otherwise the order of the elements will influence the result;
as before, in case of ties, the first element wins."
  (if (null lst) nil
      (let ((wins (first lst)))
	(dolist (obj (rest lst))
	  (if (funcall fn obj wins)
	      (setq wins obj)))
	wins)))

;;; (mostn #'length '((a b) (a b c) (a) (e f g))) => ((A B C) (E F G)) 3
(defun mostn (fn lst)
  "mostn takes a function and a list and returns a list of all the elements
for which the function yields the highest score (along with the score itself)"
  (if (null lst) (values nil nil)
      (let ((result (list (first lst)))
	    (max (funcall fn (first lst))))
	(dolist (obj (rest lst))
	  (let ((score (funcall fn obj)))
	    (cond ((> score max)
		   (setq max score
			 result (list obj)))
		  ((= score max)
		   (push obj result)))))
	(values (nreverse result) max))))

;;; Mapping Functions
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
	(push (funcall fn obj) result)))
    (nreverse result)))

;;; (rmapcar #'princ '(1 2 (3 4 (5) 6) 7 (8 9))) => 123456789
(defun rmapcar (fn &rest args)
  (if (some #'atom args) (apply fn args)
      (apply #'mapcar
	     #'(lambda (&rest args)
		 (apply #'rmapcar fn args))
	     args)))

;;; I/O functions

;;; (readlist) => Call me "Ed" (CALL ME "Ed")
(defun read-list (&rest args)
  "READ-LIST  is for the case where you want users to be able to type in
expressions without parentheses; it reads a line of input and returns it as a list."
  (values (read-from-string
	   (concatenate 'string "("(apply #'read-line args)")"))))

;;; (prompt "Enter a number between ~A and ~A.~%>> " 1 10) => Enter a number between 1 and 10.
;;; >> 3
;;; 3
(defun prompt (&rest args)
  "The function prompt combines printing a question and reading the answer. It
takes the arguments of format, except the initial stream argument"
  (apply #'format *query-io* args)
  (read *query-io*))


;;; (break-loop #'eval #'(lambda (x) (eql x :q)) ">> ") => Entering break-loop;
;;; >> (+ 2 3)
;;; 5
;;; >> :q
;;; NIL
(defun break-loop (fn quit &rest args)
  "break-loop is for situations where you want to imitate the Lisp toplevel.
It takes two functions and an &rest argument, which is repeatedly given to
prompt"
  (format *query-io* "Entering break-loop; ~%")
  (loop
     (let ((in (apply #'prompt args)))
       (if (funcall quit in)
	   (return)
	   (format *query-io* "~a~%" (funcall fn in))))))

;;; Functions which operate on symbols and strings.

;;; (mkstr pi " pieces of " 'pi) => "3.141592653589793d0 pieces of PI"
(defun mkstr (&rest args)
  " mkstr,takes any number of arguments and concatenates
 their printed representations into a string."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;;; (symb 'ar "Madi" #\L #\L 0) => |ARMadiLL0|
(defun symb (&rest args)
  "symb, which is mostly used for building symbols. It takes one or
more arguments and returns the symbol (creating one if necessary) 
whose print-name is their concatenation."
  (values (intern (apply #'mkstr args))))

(defun re-read (&rest args)
  "re-read, is a generalization of symb. It takes a series of
objects, and prints and rereads them. It can return symbols like symb, but it can
also return anything else that read can."
  (values (read-from-string (apply #'mkstr args))))

;;; (explode 'bomb) => (B O M B)
(defun explode (sym)
  "EXPLODE takes a symbol and returns a list of symbols made from the characters
in its name."
  (map 'list #'(lambda (c)
		 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))
