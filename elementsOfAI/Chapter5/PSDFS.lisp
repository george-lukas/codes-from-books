;;; PSDFS.CL -- "Painted Squares" puzzle solving
;;; using an iterative version of Depth-First Search.
;;; Thus this version uses standard AI DFS with OPEN, etc.
;;; (Compare with the recursive algorithm in RECPSDF2.CL)
;;; Also, uses a fairly fancy PRINT-STATE function
;;; for showing solutions.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 5 ("Search") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Define a data structure for the pieces (the painted squares themselves):

(defstruct piece
  "The parts of a square piece."
  number
  pattern)

(defvar *pieces* nil) ; global variable which will hold the list of pieces.

(defun define-piece (number north-pattern west-pattern
                            south-pattern east-pattern)
  (let ((new-piece
          (make-piece
            :number number
            :pattern
              (list north-pattern west-pattern
                      south-pattern east-pattern) ) ))
    (push new-piece *pieces*) ) )

(defconstant striped 1)
(defconstant hashed 2)
(defconstant gray 3)
(defconstant boxed 4)

(setf *pieces* nil)
(define-piece 4 hashed boxed gray gray)
(define-piece 3 boxed gray striped hashed)
(define-piece 2 hashed boxed boxed striped)
(define-piece 1 gray striped striped hashed)

(defstruct state
  "A state consists mainly of a board.
   It is convenient to also note
   which pieces are still not used and
   how many vacancies remain on the board."
  board            ; an array
  unused-pieces    ; a list
  vacancies-left)  ; an integer

(defconstant vacancy 0)

;;; Define the size of the rectangular (or square)
;;; board to be filled:
(defparameter *board-width* 2)
(defparameter *board-length* 2)
(defparameter *board-area*
  (* *board-width* *board-length*) )

;;; Here is a utility function for copying boards:
(defun copy-board (board)
  "Returns a new array equivalent to BOARD."
  (let ((new-board
          (make-array
            (list *board-length*
                  *board-width*) ) ))
    (dotimes (i *board-length*)
      (dotimes (j *board-width*)
        (setf (aref new-board i j)
              (aref board i j) ) ) )
    new-board) )

;;; PLACE-PIECE makes a move, putting a piece on the board.
(defun place-piece (current-state piece row column orientation)
  "Returns a new state obtained from CURRENT-STATE
   by placing PIECE at (ROW, COLUMN) in specified ORIENTATION.
   The legality of this move should be checked beforehand."
  (let* ((new-state
           (make-state
             :board
               (copy-board
                 (state-board current-state) )
             :unused-pieces
               (remove piece
                 (state-unused-pieces current-state) )
             :vacancies-left
               (1- (state-vacancies-left
                     current-state)) ) ))
    (setf (aref (state-board new-state)
                row
                column)
          (list piece orientation) )
    new-state) )

;;; Create a state with an empty board...
(defun create-initial-state ()
  "Returns a (new) initial state."
  (let ((new-board ; 1st create the board:
          (make-array
            (list *board-length*
                  *board-width*)
            :initial-element vacancy) ))
    (make-state    ; 2nd create the state:
        :board new-board
        :unused-pieces *pieces*
        :vacancies-left *board-area*) ) )
          

;;; Functions for manipulating pieces:
;;; ORIENT rotates the piece's pattern according to
;;; the given orientation.
(defun orient (piece orientation)
  "Returns the pattern corresponding to PIECE in
   the particular ORIENTATION."
  (rotate-list (piece-pattern piece) orientation) )

;;; ROTATE-LIST performs N cyclical shifts on list LST.
;;; It's used to help orient a piece.
(defun rotate-list (lst n)
  "Performs N cyclical shifts on list LST."
  (if (zerop n) lst
    (rotate-list
      (cons (first (last lst))
            (butlast lst) )
      (1- n) ) ) )

;;; SIDES-OK tests to see if placing NEW-PIECE in the
;;; specified location and rotation on the board would
;;; conflict with any neighboring pieces already placed.
(defun sides-ok (new-piece orientation row col current-state)
  "Returns T if NEW-PIECE can be placed in ORIENTATION
   at (ROW, COL) given CURRENT-STATE, without causing
   a mismatch with a piece already in place."
  (let ((trial-oriented-pattern (orient new-piece orientation))
        (which-neighbor 0) ; 0 is north, 1 is west, etc.
        (sides-ok t) )
    (dolist (displacement '((-1 0)(0 -1)(1 0)(0 1)))
      (let ((neighbor-row (+ row (first displacement)))
            (neighbor-col (+ col (second displacement))) )
        (setf sides-ok
              (and sides-ok
                (or (< neighbor-row 0)
                    (>= neighbor-row *board-length*)
                    (< neighbor-col 0)
                    (>= neighbor-col *board-width*)
                    (match-sides trial-oriented-pattern
                                 which-neighbor
                                 (oriented-piece-at
                                   neighbor-row
                                   neighbor-col
                                   current-state)
                                 (opposite-dir
                                   which-neighbor) ) ) ) ) )
      (incf which-neighbor) )
    sides-ok) )

;;; OPPOSITE-DIR changes 0 (for north) to 2 (for south), etc.
(defun opposite-dir (direction)
  "Returns the code for the opposite of DIRECTION."
  (mod (+ direction 2) 4) )

(defun oriented-piece-at (row col state)
  "Get the (PIECE ORIENTATION) item at
   board position ROW COL.
   It could also be a vacancy."
  (aref (state-board state) row col) )

(defun match-sides (oriented-pat1 which-side1 oriented-piece2 which-side2)
  "Returns T if either the neighbor (oriented-piece2) is
   a vacancy or it is a piece rotated so that the side
   appearing in position WHICH-SIDE2 matches the pattern
   part of ORIENTED-PAT1 given by WHICH-SIDE1."
  (if (eql oriented-piece2 vacancy) t
    (eql (nth which-side1 oriented-pat1)
         (nth which-side2 (apply #'orient oriented-piece2)) ) ) )

;;; use a 3 x 3 character area to print a square:
(defconstant piece-print-size 3) 

(defun print-state (state)
  (format t "~%------~%")
  (let* ((plot-width (* piece-print-size *board-width*))
         (plot-length (* piece-print-size *board-length*))
         (printing-array
           (make-array
             (list plot-length plot-width)
             :element-type 'character
             :initial-element #\space) ))
     (dotimes (i *board-length*)
       (dotimes (j *board-width*)
         (plot-piece printing-array
           i
           j
           (aref (state-board state) i j) ) ) )
     (dotimes (i plot-length)
       (dotimes (j plot-width)
         (format t "~a" (aref printing-array i j)) )
       (format t "~%") ) )
  (format t "------~%") )

(defconstant pattern-print-chars
  `((,striped . #\S)(,hashed . #\H)(,gray . #\G)(,boxed . #\B)) )

(defun num-to-char (num)
  "Returns a single character representing the
   small integer NUM."
  (char (format nil "~d" num) 0) )

(defun plot-piece (printing-array i j oriented-piece)
  (if (eql oriented-piece vacancy)
      t ; A vacancy - do nothing
        ; (board is initially blank anyway)
    ;; Plot the oriented piece:
    (let* ((starting-row (* i piece-print-size))
           (starting-col (* j piece-print-size))
           (center-row (1+ starting-row))
           (center-col (1+ starting-col))
           (side-number 0) )
      ;; Plot the piece number in the middle:
      (setf (aref printing-array center-row center-col)
            (num-to-char (piece-number (first oriented-piece))) )
      ;; Plot a character representing the pattern
      ;; on each side:
      (dolist (side-displacement '((-1 0)(0 -1)(1 0)(0 1)))
        (setf (aref printing-array
                (+ center-row (first side-displacement))
                (+ center-col (second side-displacement)) )
              (rest (assoc (nth side-number
                             (apply #'orient oriented-piece) )
                           pattern-print-chars) ))
        (incf side-number) ) ) ) )

;;; SUCCESSORS finds all the new states
;;; obtainable by placing one of the unused pieces
;;; in any of the vacant squares.
(defun successors (current-state)
  "Returns a list of successor states of CURRENT-STATE."
  (let ((new-states nil))
    (dolist (location (get-vacancies current-state))
      (dolist (piece (state-unused-pieces current-state))
        (setf new-states
              (try-piece-itr piece
                             (first location)
                             (second location)
                             current-state
                             new-states) ) ) )
    new-states) )

(defun get-vacancies (state)
  "Return a list of the locations of the
   vacant positions in STATE."
  (let ((vacancies nil))
    (dotimes (i *board-length* vacancies)
      (dotimes (j *board-width*)
         (if (eql (aref (state-board state) i j)
                  vacancy)
             (push (list i j) vacancies) ) ) ) ) )

;;; Let SOLUTION-COUNT be a local variable used to keep track of
;;; how many solutions have been found.
(let (solution-count)
  (defun zero-the-count ()
    "Initialize counter value."
    (setf solution-count 0) )
  (defun show (solution nodes-expanded)
    "Increment the count, and print out the SOLUTION."
    (incf solution-count)
    (format t "~%Solution ~d, after ~d nodes expanded:"
            solution-count nodes-expanded)
    (print-state solution) )
  )

;;; TRY-PIECE-ITR attempts to add PIECE to the CURRENT-STATE,
;;; and returns any new states that can be formed this way.
;;; It's a version of TRY-PIECE for this iterative implementation
;;; of depth-first search.
(defun try-piece-itr (piece row col current-state new-states)
  "Tries to place PIECE and continue solving from CURRENT-STATE."
  (dolist (orientation '(0 1 2 3) new-states)
    (setf new-states
      (try-orientation-itr orientation
                           piece
                           row
                           col
                           current-state
                           new-states) ) ) )

;;; TRY-ORIENTATION-ITR attempts to place PIECE in the given
;;; ORIENTATION.  It's a version of TRY-ORIENTATION for this
;;; iterative implementation of depth-first search.
(defun try-orientation-itr
         (orientation piece row col current-state new-states)
  "Tries PIECE in a particular ORIENTATION.
   If OK, includes it on NEW-STATES which is returned."
  (if (sides-ok piece orientation row col current-state)
      (push (place-piece current-state piece row col orientation)
      new-states) )
  new-states)

(defun goal-p (state)
  "Returns T if STATE is a goal state."
  (zerop (state-vacancies-left state)) )

;;; DEPTH-FIRST-SEARCH is the main searching procedure.
(defun depth-first-search (start-state)
  "Performs a depth-first search from START-STATE
   for goal states."
  (let ((open (list start-state))               ;step1
        (closed nil)
        (n-states-expanded 0)
        s
        lst)
    (loop
      (if (null open)(return n-states-expanded));step2
      (setf s (pop open))                       ;step3
      (incf n-states-expanded)
      (push s closed)
      (if (goal-p s)(show s n-states-expanded))
      (setf lst (successors s))                 ;step4
      (setf lst (list-difference lst closed))
      (setf open
        (append lst
          (list-difference open lst) ) )        ;step5
      (format t "~%(LENGTH OPEN) = ~d." (length open))
      ; end of loop -------- this is implicitly  step6
       ) ) )

;; The supporting functions:

(defun list-difference (lst1 lst2)
  "Like SET-DIFFERENCE but preserves ordering in LST1.
   Also, uses STATE= for comparison."
  (dolist (elt lst2 lst1)
    (setf lst1 (remove elt lst1 :test #'state=)) ) )

(defun state= (state1 state2)
  (if (not (= (state-vacancies-left state1)
              (state-vacancies-left state2) ))
         nil)
    (let ((array1 (state-board state1))
          (array2 (state-board state2)) )
      (dotimes (i *board-length*)
        (dotimes (j *board-width*)
          (if (not (equalp (aref array1 i j)
                           (aref array2 i j) ))
            (return-from state= nil) ) ) )
      t) )

;;; TEST-ITR initializes the solution count to 0 and starts
;;; the program.
(defun test-itr ()
  "Find solutions for the sample puzzle with the
   iterative algorithm."
  (zero-the-count)
  (depth-first-search (create-initial-state)) )

