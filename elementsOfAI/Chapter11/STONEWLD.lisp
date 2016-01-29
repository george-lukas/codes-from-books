;;; STONEWLD.CL
;;; -- A "Stone World" and a natural language interface to it.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 11
;;; ("Natural-Language Understanding") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; This program demonstrates how a natural-language understanding
;;;  program can be set up around a "microworld".

;;; All the definitions and functions for manipulating the microworld
;;; are presented first.  Then the natural-language understanding
;;; functions are defined.

;;; 3 Apr 1996 Minor fix made to the error action in the ATN at
;;; LAST-NODE.  Thanks to Peter Spoerri for pointing out the bug.-SLT
;-----------------------------------------------------------------------------
;;; First we let the global value of V be a symbol
;;; whose print-name is a dot.  We use this symbol to
;;; represent a "vacant" cell of Stone Land.
(defconstant *v* '|.|)

;;; The "space" of Stone World is STONELAND, represented
;;;  as a 10 by 20 array, initially all vacant.

(defconstant *nrows* 10)
(defconstant *ncols* 20)

(defvar *stoneland*)
(defvar *carrying*)
(defvar *last-direction*)

(setf *stoneland*
      (make-array (list *nrows* *ncols*) :initial-element *v*) )

;;; SHOW-STONELAND displays Stone Land on the screen.
(defun show-stoneland ()
  "Displays Stone Land on the screen."
  (dotimes (i *nrows*)
    (terpri)
    (dotimes (j *ncols*)
      (princ (aref *stoneland* i j)) ; don't print "|" chars.
      (format t " ") ) )
  (format t "~%Mace is carrying: ~a.~%" *carrying*) )

;;; SET-PLACE puts an object at a place in Stone Land.
(let ((positions-table (make-hash-table :size 20)))
  (defun set-place (row col objtype obj)
    "Stores an object of type OBJTYPE in Stone Land at
     position ROW, COL.  Also associate the coords with OBJ."
    (setf (aref *stoneland* row col) objtype)
    (setf (gethash obj positions-table) (list row col))   )
  (defun get-position (obj)
    "Gets the pair (ROW COL) associated with an object."
    (gethash obj positions-table) )
  )

;;; Place landmarks in Stoneland:
(set-place  4  4 'W 'W)  ;well
(set-place  6 14 'Q 'Q)  ;quarry
(set-place  4 12 'M 'M)  ;Mace, the mason
(set-place  6  1 'T 'T)  ;tree
(set-place  9 17 'P 'T)  ;pillar
(set-place  1  5 'G 'G1) ;gem

(setf *carrying* nil) ; Mace starts out empty-handed.

;;; Stones and gems are portable; landmarks are not:
(mapcar #'(lambda (obj) (setf (get obj 'portable) t))   '(S G))
(mapcar #'(lambda (obj) (setf (get obj 'portable) nil)) '(Q W T P))

;;; The functions that follow implement operations on
;;; Stone Land and tests for their legality
;;; (moving, picking things up, putting them down).

(defconstant *directions* '(north east south west))

;;; MOVE makes Mace move in DIRECTION.
(defun move (direction)
  "Moves Mace one step (in the) DIRECTION."
  (let ((pos (neighbor-pos direction)))
    (set-place (mace-row) (mace-col) *v* *v*)
    (set-place (first pos)(second pos) 'M 'M)
    ) )

(defun mace-row () (first (get-position 'M)))
(defun mace-col () (second (get-position 'M)))

;;; MOVE-LEGAL returns T if it is ok to move in DIRECTION.
(defun move-legal (direction)
  "Returns T if the neighboring position (according to
   DIRECTION) is vacant."
  (eql (neighbor-contents direction) *v*) )

;;; NEIGHBOR-CONTENTS returns contents of neighbor specified
;;; or OFF-LIMITS if outside Stone Land.
(defun neighbor-contents (dir)
  "Returns value at neighboring position (specified by DIR)
   of the Stone Land array."
  (let* ((nei-pos (neighbor-pos dir))
         (i (first nei-pos))
         (j (second nei-pos)) )
    (if (or (minusp i)(minusp j)(= i *nrows*)(= j *ncols*))
        'off-limits
        (aref *stoneland* i j) ) ) )

;;; NEIGHBOR-POS returns the coordinates of the neighbor specified.
(defun neighbor-pos (dir)
  "Translates a direction DIR into a pair of coordinates."
  (vector+ (get-position 'M)
    (cond ((eql dir 'north) '(-1 0))
          ((eql dir 'south) '(1 0))
          ((eql dir 'west) '(0 -1))
          ((eql dir 'east) '(0 1)) ) ) )

(defun vector+ (x y)
  "Returns the vector sum of X and Y."
  (if (null x) nil
      (cons (+ (first x) (first y))
            (vector+ (rest x) (rest y)) ) ) )

;;; TAKE makes Mace pick up object in given DIRECTION.
(defun take (direction)
  "Causes Mace to be carrying the object located in the
   neighboring cell specified by DIRECTION, and removes
   the object from that cell."
  (let ((pos (neighbor-pos direction)))
    (setf *carrying*
          (stone-or-gem (aref *stoneland* (first pos) (second pos))) )
    (if (not (equal pos (get-position 'Q)))
        (set-place (first pos) (second pos) *v* *v*) ) ) )

;;; STONE-OR-GEM if given a quarry object returns S (a stone).
;;; Other inputs are returned unchanged.
(defun stone-or-gem (obj)
  "Converts any Q to an S, so that picking up at the quarry
   produces a stone."
  (if (eql obj 'Q) 'S obj) )

;;; TAKE-LEGAL returns T if it is ok to take from DIRECTION.
(defun take-legal (direction)
  "Returns T if the DIRECTION is a legal one, there is
   a possibility to take something from the cell in that
   direction, and Mace is currently empty-handed."
  (and (member direction *directions*)
       (member (neighbor-contents direction) '(Q S G))
       (null *carrying*) ) )

;;; PUT makes Mace drop the object in DIRECTION.
(defun put-down (direction)
  "Causes Mace to put down the object being carried at
   the neighboring cell specified by DIRECTION."
  (let ((pos (neighbor-pos direction)))
    (if (not (equal pos (get-position 'Q)))
        (set-place (first pos)(second pos) *carrying* *carrying*) )
    (setf *carrying* nil) ) )

;;; PUT-LEGAL returns T if it is ok to put to DIRECTION.
(defun put-legal (direction)
  "Returns T if DIRECTION is a valid direction, the
   neighboring cell specified by DiRECTION is either vacant
   or contains a quarry, and Mace is currently holding something."
  (and (member direction *directions*)
       (member (neighbor-contents direction) (list 'Q *v*))
       *carrying*) )

(defvar *direction-slot*)
(defvar *destination-slot*)
(defvar *object-slot*)

(defvar *success*)
(defvar *interpretation*)

;;; START is the top-level function.
(defun start ()
  "Initiates a session with the Stone World program."
  (show-stoneland)
  (format t "~%Hey you up there! what should i do?")
  (format t "~%Please end your sentences with ! . or ?~%")
  (loop	
    (let* ((input (input-sentence)))
      (setf *s* input) )  ; Note that *S* is global.
      (parse 'g1) ;analyze the English input in *S*.
      ; Test for the end of the dialog:
      (if (eql (first *interpretation*) 'bye-command)
          (return '(Good Bye)) )
      (act-upon *interpretation*)     ; Try to obey command or answer query.
      (produce-reply)
      (if *success* (show-stoneland)) ; Update the display.
;;;Code to move the T-rex goes here.
       ) )

;;; INPUT-SENTENCE gets a sentence from the user.
;;; It keeps reading atoms until it reads an atom with
;;; either a period, exclamation mark, or question mark
;;; immediately following, e.g., "GO."
(defun input-sentence ()
  "Reads and returns a new sentence from the user."
  (let ((word (separate-punc (read))))
    (cond ((null word)
           (format t "~%Input should be a series of symbols -- not a list") )
          ((symbolp word) (cons word (input-sentence)))
          (t word) ) ) )

;;; SEPARATE-PUNC takes an expression, which should 
;;; be a symbol, and checks whether it ends in a
;;; punctuation character.  If so, the punctuation is
;;; separated from the rest of the print-name string, and
;;; a list of two separate symbols is returned: one for the
;;; word itself and one for the punctuation.  If there is
;;; no punctuation, then SEPARATE-PUNC simply returns its
;;; input argument WORD unchanged.  Also, exclamation marks
;;; are converted to periods.
(defun separate-punc (word)
  "In case the symbol WORD ends in a punctuation character,
   returns a list of two separate symbols for the word and the
   punctuation.  Also tests for WORD being a symbol at all."
  (if (not (symbolp word)) nil
    (let* ((str (string word))      ; Get the print name of WORD.
           (len (length str))       ; Save its length.
           (last-char (elt str (1- len))) ) ; Pull out the last char.
      (if (or (char= last-char #\.) ; Is it . ! or ?
              (char= last-char #\!)
              (char= last-char #\?) )
          (list (intern (subseq str 0 (1- len))) ; yes, list the two
                (intern (string (excl-to-period last-char))) ); parts of WORD.
          word) ) ) ) ; No, return WORD unchanged.

;;; EXCL-TO-PERIOD maps ! to . and all others to themselves.
(defun excl-to-period (c)
  "Returns period if C is an exclamation mark, and returns
   C unchanged otherwise."
  (if (char= c #\!) #\. c) )

(setf *last-direction* 'north)
(setf *displaying* t)

;;;--------------- Now for the Natural-Language Interface -----------------

;;; Sample Augmented Transition Netowrk to interpret commands such
;;; as "Take a stone from the quarry."

(defvar *s*) ; Used to store the remainder of the sentence
             ; during parsing.

(defvar *bindings*) ; Used to store the results of matching during parsing.

;;; PARSE begins parsing the current list of words from NODE.
(defun parse (node)
  "Parses current input starting at NODE."
  (eval (append '(cond)
                (build-cond-clause (get-arcs node))
                '((t (format t "~%Stuck in parse.  ")
                     (setf *interpretation* '(nil))
                     (setf *success* nil) )) )) )

(defun binding (x)
  "Returns any binding associated with X as a result of
   the last call to MATCH."
  (val x *bindings*) )

;;; BUILD-COND-CLAUSE makes each compressed clause into a
;;; legitimate COND clause.
(defun build-cond-clause (arclist)
  "Returns a component of a COND form from list of arc descriptions."
  (mapcar
    #'(lambda (edge)	
        (cons 
          (list 'setf
                '*bindings*
                (append '(match)
                        (list (cons 'quote
                              (list (first edge)) ))
                        '(*s*) ) )
          (rest edge) ) )
    arclist) )

;;; NEXT displays progress and continues the parse at NODE
;;; operating on WHATS-LEFT of the user's input.
(defun next (node whats-left)
  "Prints NODE and then continues parsing WHATS-LEFT from there."
  (if whats-left (setf *s* (val whats-left *bindings*)))
    ;; If WHATS-LEFT is null, do not change *S*.
  (format t "~a " node)
  (parse node) )

;;; Here is the ATN for command analysis.
;;; It is based on a semantic grammar.
(let ((arcs-table (make-hash-table :size 20)))
  (defun store-arcs (node arcs)
    "Establishes ARCS as the possible transitions out of NODE."
    (setf (gethash node arcs-table) arcs) )
  (defun get-arcs (node)
    "Retrieves the possible transitions out of NODE."
    (gethash node arcs-table) )
  )

(store-arcs 'g1
  '(
      (((bye-verb x) (* y))
          (setf *s* (binding 'y))
          (setf *interpretation* '(bye-command)))
      ((show \.)
          (setf *s* '(\.))
          (setf *interpretation* '(show-command))
          )
      (((go-verb x) (* y))
          (setf *direction-slot* '(direction forward)) ;default direc.
          (setf distance-slot '(steps 1))              ;default dist.
          (next 'g2 'y) )
      (((take-verb x) (* y))
          (setf *command* 'take-command)
          (setf *direction-slot* 
                '(direction unspec))   ; default direc.
          (setf *object-slot* 'stone)  ; default object
          (next 't2 'y) )
      (((put-verb x) (* y))
          (setf *command* 'put-command)
          (setf *direction-slot*
                '(direction forward) ) ; default direc.
          (setf *object-slot*
                (list *carrying*) )    ; default object
          (next 'p2 'y) ))
          )

(store-arcs 'g2
  '(
      ((\.) (next 'g3 nil))            ;end of command
      ((to (* x))		       ; "TO" arc
          (setf *s* (binding 'x))
          (parse 'dnp1)	;Get destination info.
          (next 'g3 nil) )
      ((toward (* x))
          (setf *s* (binding 'x))
          (parse 'dnp1)	;Get directional info.
          (next 'g3 nil) )
      (((direction-adverb x)(* y))
          (setf *direction-slot*
          (list 'direction (normalize-direction (binding 'x))) )
          (next 'g3 'y) ) )
          )

(store-arcs 'g3
  '(
      ((\.) (setf *interpretation*
                  (list 'go-command
                        *direction-slot*
                        distance-slot) )
          (next 'last-node nil) )
	  (((* x)) (next 'last-node nil) ) )
        )

(store-arcs 'dnp1  ; sub-ATN for directional noun phrase
  '(
      ((\.) (format t "~%This must be a name.")(next 'dnp2))
      ((a (* x))
          (next 'dnp2 'x) )
      ((the (* x))
          (next 'dnp2 'x) ) )
        )

(store-arcs 'dnp2
  '(
      (((obj-noun x)(* y))
          (setf *s* (binding 'y))
          (setf *direction-slot*
                (list 'toward (normalize-object (binding 'x))) )
          )
      (((direction-noun x) (* y))
          (setf *s* (binding 'y))
          (setf *direction-slot* (list 'direction (binding 'x)))
          ) ) 
        )

(store-arcs 't2
  '(
      ((up (* x))       ; Ignore particle "up" if present here.
          (next 't3 'x) )
      (((* x))
          (next 't3 nil) ) )
        )

(store-arcs 't3   ; Get object (if any) of take or put verb.
  '(
      (((article x)(* y))
          (parse 'np1)
          (setf *object-slot* np1)
          (next 't4 nil) ) )
        )

(store-arcs 't4
  '(
      ((\.)
          (setf *interpretation*
                (list *command* *object-slot* *direction-slot*) )
          )
      (((dir-prep x)(* y))
          (setf *s* (binding 'y))
          (parse 'dnp1)
          (next 't4 nil) ) )
        )

(store-arcs 'np1  ; Parse noun phrase and set NP1's value to result.
  '(
      (((article x)(* y))
          (cond ((eql (binding 'x) 'the)(setf definite t))
                (t (setf definite nil)) )
          (next 'np2 'y) ) )
        )

(store-arcs 'np2
  '(
      (((obj-noun x)(* y))
          (setf *s* (binding 'y))
          (setf np1 (list (normalize-object (binding 'x))
                          (cons 'definite (list definite)) ))
          ) )
        )

(store-arcs 'p2
  '(
      ((down (* x))     ; Ignore particle "down" if present here.
          (next 't3 'x) )
      ((it (* x))
          (next 'p3 'x) )
      (((* x))
          (next 't3 nil) ) )
        )

(store-arcs 'p3   ; Object seen already.
  '(
      ((down (* x))     ; Ignore particle "down" if present here.
          (next 't4 'x) )
      (((* x))
          (next 't4 nil) ) )
        )

(store-arcs 'last-node
  '(
      ((\.)  (format t "~%I understand you.") )
 ;; QUOTE before X added to fix a bug in the book version SLT 3 Apr 96:
      (((* x))(format t "~%I couldn't parse: ~s" (binding 'x))
 ;; Also, this line added SLT 3 Apr 96:
              (setf *interpretation* '(nil)) ) ; 
        ) )

;;; Functions that support the pattern matching in the ATN:
(defun go-verb (w)
  "Returns T if W is one of these verbs."
  (member w '(go move head walk)) )

(defun take-verb (w)
  "Returns T if W is one of the 'take' verbs."
  (member w '(take pick grab lift carry)) )

(defun put-verb (w)
  "Returns T if W is one of the 'put' verbs."
  (member w '(put drop place release position leave)) )

(defun direction-adverb (w)
  "Returns T if W is a direction adverb."
  (member w '(north east south west northward eastward southward 
              westward right left up down straight ahead)) )

(defun direction-noun (w)
  "Returns T if W is a direction noun."
  (member w '(north east south west)) )

(defun dir-prep (w)
  "Returns T if W is a direction preposition."
  (member w '(to from toward towards)) )

(defun obj-noun (w)
  "Returns T if W is a noun referring to a Stone Land object."
  (member w '(quarry corner place tree well pillar stone gem)) )

(defun article (w)
  "Returns T if W is an article."
  (member w '(a an the)) )

(defun bye-verb (w)
  "Returns T if W means 'goodbye'."
  (member w '(quit bye exit finish bye-bye goodbye stop)))

;;; Functions that support the actions on ATN arcs:
(defun normalize-direction (w)
  "Translates W into the standard symbol for that direction."
  (translate w '((northward . north)(eastward . east)(southward . south)
    (westward . west)(right . east)(left . west)(up . north)
    (down . south)(straight . forward)(ahead . forward) )) )

;;; TRANSLATE looks word W up in DICT (a list of dotted pairs).
(defun translate (w dict)
  "Returns the symbol associated with W in DICT."
  (let ((pair (assoc w dict)))
    (if pair (rest pair)
      w) ) )

(defun normalize-object (w)
  "Translates the English word for an object into its Stone World rep."
  (translate w '((quarry . Q)(tree . T)(well . W)(pillar . P)
                 (stone . S)(gem . G)(mace . M)(you . M) )) )

;;; ACT-UPON is a command dispatcher. SI is the semantic
;;; interpretation which embodies the command.
(defun act-upon (si)
  "Executes the command represented by SI."
  (cond ((eql (first si) 'go-command)
         (setf *success* t) ;T until proven NIL
         (obey-go si) )
        ((eql (first si) 'show-command)
         (setf *success* t) )
	((eql (first si) 'take-command)
         (setf *success* t)
	 (obey-take si) )
        ((eql (first si) 'put-command)
         (setf *success* t)
         (obey-put si) )
        (t (setf *success* nil)
           (format t "~%Can't satisfy that kind of request.") ) ) )

(defun obey-go (si)
  "Executes movement action for Mace."
  (let* ((prms (rest si))
         (direction (cadadr si))
         (distance-slot (caddr si))
         (nsteps (second distance-slot)) )
    (if (eql direction 'forward) (setf direction *last-direction*))
    (dotimes (i nsteps)
      (cond ((move-legal direction)
             (move direction)
             (setf *last-direction* direction))
            (t (setf *success* nil)) ) ) ) )

(defun obey-take (si)
  "Executes a TAKE action."
  (let* ((prms (rest si))
         (object (caar prms))
         (direction (cadadr prms)) )
    (cond ((eql direction 'forward)
           (setf direction *last-direction*) )
          ((eql direction 'unspec)
           (setf direction (find-dir object *directions*)) )
          ((eql direction 'Q)   ; Handle Quarry direction specially.
           (setf direction (find-dir1 'Q *directions*)) )
           )
    (cond ((take-legal direction) (take direction))
          (t (format t "~%Can't take that way.")
             (setf *success* nil) ) ) ) )

;;; FIND-DIR searches in DIRECTIONS to find OBJ.
(defun find-dir (obj directions)
  "Returns the direction, if any, in which OBJ lies,
   relative to Mace's current position.  If OBJ is a
   stone, then Q is considered to satisfy the search."
  (cond ((null directions) nil)
        ((eql obj
             (stone-or-gem              ; Consider Q as S
               (apply #'aref
                      (cons *stoneland*
                            (neighbor-pos (first directions)) ) ) ) )
         (first directions))
        (t (find-dir obj (rest directions))) ) )

;;; FIND-DIR1 is like FIND-DIR, but Q is not mapped to S.
(defun find-dir1 (obj directions)
  "Returns the direction, if any, in which OBJ lies."
  (cond ((null directions) nil)
        ((eql obj
             (apply #'aref
                    (cons *stoneland*
                          (neighbor-pos (first directions)) ) ) )
	 (first directions))
	(t (find-dir1 obj (rest directions))) ) )

(defun obey-put (l)
  "Executes a PUT command."
  (let* ((prms (rest *interpretation*))
         (object (caar prms))  ;thing to put
         (direction (cadadr prms))
         (ok nil) )
    (if (eql object *carrying*) (setf ok t))
    (if (eql direction 'forward)
        (setf direction *last-direction*) )
    (if (and ok (put-legal direction))
        (put-down direction)
        (progn (format t "~%Can't put that way!  ")
               (setf *success* nil) ) ) ) )

(defun produce-reply ()
  "Prints some feedback about the success of the attempt
   to execute the command."
  (if *success* (format t "~%OK, next?")
      (format t "~%I can't quite do that one.~%") ) )

 
;;; requires the function MATCH.  Its definition is given again below:

;;; MATCH.CL -- a recursive pattern-matching function

(defun match (p s)
  "Attempt to find a correspondence between P and S, utilizing
   any special constructs appearing in P.  Return an association
   list of bindings if successful; NIL otherwise."
  (cond
    ((handle-both-null p s))
    ((handle-normal-recursion p s))
    ((atom (first p)) nil)
    ((handle-? p s))
    ((handle-* p s))
    ((handle-restrict-pred p s))
    (t nil) ) )

(defun 1st-pattern-op (p)
  "Return the *, ? or predicate in the first pattern
   construct of P."
  (first (first p)) ) ; same as (CAAR P)

(defun 1st-pattern-variable (p)
  "Return the variable in the first pattern
   construct of P."
  (first (rest (first p))) ) ; same as (CADAR P)

(defun handle-both-null (p s)
  "Test for and handle case when both P and S
   are null."
  (if (and (null p)(null s))
      '((:yes . :yes)) ) )

(defun handle-normal-recursion (p s)
  "Test for and handle case when the first
   elements of P and S are EQL."
  (if (atom (first p))
      (if (eql (first p)(first s))
          (match (rest p)(rest s)) ) ) )

(defun handle-? (p s)
  "Test for and handle the case when (FIRST P) is of
   the form (? X)."
  (if s ; S must not be null
      (if (eql (1st-pattern-op p) '?)
          (let ((rest-match
                  (match (rest p)(rest s)) ))
            (if rest-match
                (acons 
                  (1st-pattern-variable p)
                  (first s))
                  rest-match) ) ) ) )

(defun handle-* (p s)
  "Test for and handle the case when (FIRST P) is of
   the form (* X)."
  (if (eql (1st-pattern-op p) '*)
      (let ((pattern-variable
              (1st-pattern-variable p) )
            (rest-match nil) )
        (cond ; subcase 1 --match 1 element of S:
              ((and s
                    (setf rest-match
                          (match (rest p)
                                 (rest s) ) ) )
               (acons pattern-variable
                      (list (first s))
                      rest-match) )

              ; subcase 2 --match no elements of S:
              ((setf rest-match (match (rest p) s))
               (acons pattern-variable
                      nil
                      rest-match) )

              ; subcase 3 --match more than 1 elt of S:
              ((and s
                    (setf rest-match
                          (match p (rest s)) ) )
               (acons pattern-variable
                      (cons (first s)
                            (val pattern-variable
                                 rest-match) )
                      (rest rest-match)) )
              (t nil) ) ) ) )

(defun handle-restrict-pred (p s)
  "Handle case when (FIRST P) is of the form
   (PREDICATE X)."
  (if s ; S must not be null
    (if (member (1st-pattern-op p)
                '(? *) ) ; Don't apply '? or '*.
        nil
      (if (apply (1st-pattern-op p)
                 (list (first s)) )
          (let ((rest-match
                  (match (rest p) (rest s)) )
                (pattern-variable
                  (1st-pattern-variable p) ) )
            (if rest-match
                (acons pattern-variable
                       (first s)
                       rest-match) ) ) ) ) ) )

;;; The function VAL provides convenient access to
;;; something matched by a variable after matching
;;; with MATCH.
(defun val (variable alist)
  "Return the value associated with VARIABLE
   on ALIST."
  (rest (assoc variable alist)) )
