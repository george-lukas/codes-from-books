;;;; cstngs.lisp - A simple adventure game written in Common Lisp
;;;; George Lukas < last update: Jan/15/2016 - 04:13PM >

;;; Defining some objetcs that player can pick-up
(defvar *objects* '(whiskey-bottle bucket frog chain))

;;; Defining the mini world
(defvar *map* '((living-room (you are in the living-room of a wizards house. there is a wizard snoring loudly on the couch.)
		 (west door garden)
		 (upstairs stairway attic))
		(garden (you are in a beautiful garden. there is a well in front of you.)
		 (east door living-room))
		 (attic (you are in the attic of the wizards house. there is a giant weldind torch in the corner.)
		  (downstairs stairway living-room))))

;;; Defining a list of objects on the map
(defvar *objects-locations* '((whiskey-bottle living-room)
			      (bucket living-room)
			      (chain garden)
			      (frog garden)))

;;; Describing things...
;;; Description of the location of the player on the game
(defvar *location* 'living-room)

;;; (DESCRIBE-LOCATION 'garden *MAP*) => (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU.)
(defun describe-location (location map)
  "Describes a location in the world."
  (second (assoc location map)))

;;; (DESCRIBE-path  '(west door garden)) => (THERE IS A DOOR GOING WEST FROM HERE.)
(defun describe-path (path)
  "Describes a path in the world."
  `(there is a ,(second path) going ,(first path) from here.))

;;; (DESCRIBE-paths 'garden *MAP*) => (THERE IS A DOOR GOING EAST FROM HERE.)
(defun describe-paths (location map)
  "Describes the path in the world."
  (apply #'append (mapcar #'describe-path (nthcdr 2 (assoc location map)))))

;;; If there are any objects on the floor at the location we are standing in,
;;; we'll want to describe them as well.

;;; (IS-AT 'whiskey-bottle 'living-room *OBJECTS-LOCATIONS*) => T
(defun is-at (obj loc obj-loc)
  "A helper function that tells us wether an item is in a given place."
  (eql (second (assoc obj obj-loc)) loc))

;;; (DESCRIBE-floor 'garden *OBJECTS* *OBJECTS-LOCATIONS*) => (YOU SEE A FROG ON THE FLOOR. YOU SEE A CHAIN ON THE FLOOR.)
(defun describe-floor (loc objs obj-loc)
  "This function describe the floor."
  (apply #'append (mapcar (lambda (x)
			    `(you see a ,x on the floor.))
			  (remove-if-not (lambda (x)
					   (is-at x loc obj-loc))
					 objs))))
;;; (LOOK) => 
;; (YOU ARE IN THE LIVING-ROOM OF A WIZARDS HOUSE. THERE IS A WIZARD SNORING
;;  LOUDLY ON THE COUCH. THERE IS A DOOR GOING WEST FROM HERE. THERE IS A STAIRWAY
;;  GOING UPSTAIRS FROM HERE. YOU SEE A WHISKEY-BOTTLE ON THE FLOOR. YOU SEE A
;;  BUCKET ON THE FLOOR.)
(defun look ()
  "LOOK that uses the global variables to feed
all the descriptor functions and describes everything."
  (append (describe-location *location* *map*)
	  (describe-paths *location* *map*)
	  (describe-floor *location* *objects* *objects-locations*)))

;;; Walking around in our world
(defun walk-direction (direction)
  "Takes a direction and let us walk there."
  (let ((next (assoc direction (nthcdr 2 (assoc *location* *map*)))))
    (cond (next (setf *location* (third next)) (look))
	  (t '(you cant go that way.)))))

;;; Casting SPELs - Semantic Program Enhancement Logic
(defmacro defspel (&rest rest) `(defmacro ,@rest))

;;; What this code does is it tells the Lisp compiler that the word
;;; 'walk' is not actually the word walk but the word WALK-DIRECTION
;;; and that the word 'direction' actually has a quote in
;;; front of it, even though we can't see it.
(defspel walk (direction)
  `(walk-direction ',direction))

(defun pickup-object (object)
  "This function checks to see if the object is indeed on the floor
of the current location- If it is, it pushes the new location (the
player's body) onto the list and returns a sentence
letting us know wether it succeeded."
  (cond ((is-at object *location* *objects-locations*) (push (list object 'body) *objects-locations*) `(you are now carrying the ,object))
	(t '(you cannot get that.))))

;;;  (PICKUP whiskey-bottle) => (YOU ARE NOW CARRYING THE WHISKEY-BOTTLE)
(defspel pickup (object)
  `(pickup-object ',object))

(defun inventory ()
  "A command that lets us see our current inventory of items we're
carrying"
  (remove-if-not (lambda (x)
		   (is-at x 'body *objects-locations*))
		 *objects*))

(defun have (object)
  "A function that tells us if he have a certain object on us."
  (member object (inventory)))

;;; Creating special actions in our game
(defparameter *chain-welded* nil)

(defparameter *bucket-filled* nil)
	      
(defspel game-action (command subj obj place &rest rest)
  `(defspel ,command (subject object)
     `(cond ((and (eql *location* ',',place)
		  (eql ',subject ',',subj)
		  (eql ',object ',',obj)
		  (have ',',subj))
	     ,@',rest)
	    (t '(i cant ,',command like that.)))))

(game-action weld chain bucket attic
	     (cond ((and (have 'bucket) (setf *chain-welded* 't))
		    '(the chain is now securely welded to the bucket.))
		   (t '(you do not have a bucket.))))

(game-action dunk bucket well garden
	     (cond (*chain-welded* (setf *bucket-filled* 't) '(the bucket is now full of water))
		   (t '(the water level is too low to reach.))))

;;; (SPLASH BUCKET WIZARD) => 
;; (THE WIZARD AWAKENS FROM HIS SLUMBER AND GREETS YOU WARMLY. HE HANDS YOU A
;;     MAGIC LOW-CARB DONUT- YOU WIN! THE END.)
(game-action splash bucket wizard living-room
	     (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
		    ((have 'frog) '(the wizard awakens and sees that you stole his frog.
				    he is so upset he banishes you to the netherworlds- you lose! the end.))
		    (t '(the wizard awakens from his slumber and greets you warmly. he hands you a magic low-carb donut- you win! the end.))))
