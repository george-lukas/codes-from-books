;;;; match.lisp - Chapter6: Building Software Tools
;;;; George Lukas < Jan/13/2016 - 06:12PM >

;;; Constansts and low-level functions
(defconstant +fail+ nil
  "Indicates MATCH failure.")

(defconstant +no-bindings+ '((t . t))
  "Indicates MATCH success, with no variables.")

(defun variable-p (x)
  "Is X a variable (a symbol beginning with '?')?"
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the variable part of a single binding."
  (rest binding))

(defun make-binding (var val) (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
	;; once we add a "real" binding;
	;; we can get rid of the dummy no-bindings
	(if (eql bindings +no-bindings+)
	    nil
	    bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input? User (or updates) and returns bindings."
  (let ((binding (get-biding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((eql input (binding-val binding)) bindings)
	  (t +fail+))))

;;; Defining the predicate that recognize generalized segments and
;;; single-element patterns, and the matching functions...

;;; Tables..
(setf (get '?is  'single-match) 'match-is)
(setf (get '?or  'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?*  'segment-match) 'segment-match)
(setf (get '?+  'segment-match) 'segment-match+)
(setf (get '??  'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

;;; Defining the "glue" that holds the table together..
(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern)) 
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

;;; Defining the individual matching functions.
;;; The single-pattern matching functions...
(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
	 (pred (second var-and-pred))
	 (new-bindings (match var input bindings)))
    (if (or (eql new-bindings +fail+)
	    (not (funcall pred input)))
	+fail+
	new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eql bindings +fail+) +fail+)
	((null patterns) bindings)
	(t (match-and (rest patterns) input
		      (match (first patterns) input
			     bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any of the patterns match the input."
  (if (null patterns) +fail+
      (let ((new-bindings (match (first patterns)
			    input bindings)))
	(if (eql new-bindings +fail+)
	    (match-or (rest patterns) input bindings)
	    new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
This will never bind any variable."
  (if (match-or patterns input bindings) +fail+
      bindings))

;;; Defining the segment-match functions..
(defun first-match-pos (pat1 input start)
  "Find the first position that PAT1 could possibly match input,
starting at position start. If PAT1 is non-constant, then just
return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
	 (position pat1 input :start start :test #'equal))
	((<= start (length input)) start)
	(t nil)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	(let ((pos (first-match-pos (first pat) input start)))
	  (if (null pos) +fail+
	      (let ((b2 (match
			    pat (subseq input pos)
			    (match-variable var (subseq input 0 pos)
					    bindings))))
		;; If this match +fail+, try another longer one
		(if (eql b2 +fail+)
		    (segment-match pattern input bindings (+ pos 1))
		    b2)))))))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (or (match (cons var pat) input bindings)
	(match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
The pattern looks like ((?if code) . rest)."
  (and (progv (mapcar #'first bindings)
	      (mapcar #'rest bindings)
	 (eval (second (first pattern))))
       (match (rest pattern) input bindings)))

;;; Finally... Match itself..
(defun match (pattern input &optional (bindings +no-bindings+))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings +fail+) +fail+)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)                
         (segment-matcher pattern input bindings))  
        ((single-pattern-p pattern)                 ; ***
         (single-matcher pattern input bindings))   ; ***
        ((and (consp pattern) (consp input)) 
         (match (rest pattern) (rest input)
                    (match (first pattern) (first input) 
                               bindings)))
        (t +fail+)))

;;; 
(defun rule-based-translator (input rules &key
					    (matcher 'match) (rule-if #'first)
					    (rule-then #'rest) (action #'sublis))
  "Find the first rule in rules that matches input,
  and apply the action to that rule."
  (some 
   #'(lambda (rule)
       (let ((result (funcall matcher (funcall rule-if rule) 
			      input)))
	 (if (not (eql result +fail+))
	     (funcall action result (funcall rule-then rule)))))
   rules))

;;; Tests
(defun expand-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-match-abbrev (first pat))
                 (expand-match-abbrev (rest pat))))))

(defun match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a match pattern."
  (setf (get symbol 'expand-match-abbrev) 
	(expand-match-abbrev expansion)))


