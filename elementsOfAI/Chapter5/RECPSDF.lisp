;;; RECPSDF.CL -- "Painted Squares" puzzle solving
;;; using Depth-First Search.
;;; The DFS is effected through recursion.
;;; Also, uses a simple solution-printing function, unlike that
;;; used in PSDFS.CL.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 5 ("Search") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Define the pieces:

(defstruct piece
  "The parts of a square piece."
  number
  pattern)

(defvar *pieces* nil)

(defun define-piece
  (number north-pattern west-pattern
          south-pattern east-pattern)
  (let ((new-piece
          (make-piece :number number
            :pattern
              (list north-pattern west-pattern
                      south-pattern east-pattern) ) ))
    (push new-piece *pieces*) ) )


(defconstant vacancy 0)

(defconstant striped 1)
(defconstant hashed 2)
(defconstant gray 3)
(defconstant boxed 4)

(setf *pieces* nil)
(define-piece 4 hashed boxed gray gray)
(define-piece 3 boxed gray striped hashed)
(define-piece 2 hashed boxed boxed striped)
(define-piece 1 gray striped striped hashed)

;;; Define the size of the rectangle (or square) to be filled:
(defparameter *board-width* 2)
(defparameter *board-length* 2)
(defparameter *board-area*
  (* *board-width* *board-length*) )

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

(defstruct state
  "A state consists mainly of a board.
   It is convenient to also note
   how many vacancies remain on the board."
  board            ; an array
  unused-pieces    ; a list
  vacancies-left)  ; an integer

;;; PLACE-PIECE makes a move, putting a piece on the board.
(defun place-piece (current-state piece row column orientation)
  "Returns a new state obtained from CURRENT-STATE
   by placing PIECE at (ROW, COLUMN) in ORIENTATION.
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

;;; ROTATE-LIST performs N cyclical shifts on list L.
(defun rotate-list (lst n)
  "Performs N cyclical shifts on list LST."
  (if (zerop n) lst
    (rotate-list
      (cons (first (last lst))
            (butlast lst) )
      (1- n) ) ) )

(defun sides-ok (new-piece orientation row col current-state)
  "Returns T if NEW-PIECE can be placed in ORIENTATION
   at (ROW, COL) given CURRENT-STATE, without causing
   a mismatch with a piece already in place."
  (let ((trial-oriented-pattern (orient new-piece orientation))
        (which-neighbor 0)
        (sides-ok t) )
    (dolist (displacement '((-1 0)(0 -1)(1 0)(0 1)))
      (let ((neighbor-row (+ row (first displacement)))
            (neighbor-col (+ col (second displacement))) )
        (setf sides-ok
              (and sides-ok
                (or (< neighbor-row 0) (>= neighbor-row *board-length*)
                    (< neighbor-col 0) (>= neighbor-col *board-width*)
                    (match-sides trial-oriented-pattern
                                 which-neighbor
                                 (oriented-piece-at
                                   neighbor-row
                                   neighbor-col
                                   current-state)
                                 (opposite-dir which-neighbor) ) ) ) ) )
      (incf which-neighbor) )
    sides-ok) )

(defun opposite-dir (direction)
  "Returns the opposition of DIRECTION."
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


;;; SOLVE-SQUARES attempts to find a solution from
;;; CURRENT-STATE by placing additional pieces.
(defun solve-squares (current-state)
  "Attempts to find a solution from CURRENT-STATE."
  (if (null (state-unused-pieces current-state))
      (show current-state) ;sol'n found
    (let* ((k (- *board-area*
                 (state-vacancies-left current-state) ))
           (next-row (floor k *board-width*))
           (next-col (mod k *board-width*)) )
      (dolist (piece (state-unused-pieces current-state))
              (try-piece piece
                         next-row
                         next-col
                         current-state) ) ) ) )

;;; Let COUNT be a local variable used to keep track of
;;; how many solutions have been found.
(let (count)
  (defun zero-the-count ()
    "Initialize counter value."
    (setf count 0) )
  (defun show (solution)
    "Increment the count, and print out the SOLUTION."
    (incf count)
    (format t "~%Solution ~d:" count)
    (print-solution solution) )
  )

;;; TRY-PIECE attempts to add PIECE to the CURRENT-STATE,
;;; and to recursively solve the remaining subproblem.
(defun try-piece (piece row col current-state)
  "Tries to place PIECE and continue solving from CURRENT-STATE."
  (dolist (orientation '(0 1 2 3))
          (try-orientation orientation
                           piece
                           row
                           col
                           current-state) ) )

;;; TRY-ORIENTATION attempts to place PIECE in the given
;;; ORIENTATION and recursively solve the resulting subproblem.
(defun try-orientation
         (orientation piece row col current-state)
  "Tries PIECE in a particular ORIENTATION."
  (if (sides-ok piece orientation row col current-state)
      (solve-squares
        (place-piece current-state piece row col orientation) )
    nil) )

;;; TEST initializes the solution count to 0 and starts
;;; the program.
(defun test ()
  "Find solutions for the sample puzzle."
  (zero-the-count)
  (solve-squares (create-initial-state)) )

(defun print-solution (solution)
  "Uses simple method to print solution."
  (let ((the-array (state-board solution))
        (oriented-piece-list nil) )
    (dotimes (i *board-length*)
      (dotimes (j *board-width*)
        (let ((the-pair (aref the-array i j)))
          (push (list (piece-number (first the-pair))
                      (second the-pair) )
                oriented-piece-list)
         ) ) )
    (print (reverse oriented-piece-list)) ) )