;;;; AI algorithms, data structures, and idioms in Prolog, Lisp, and Java
;;;; anmls.lisp - Chapter18 - Semantic Networks, Inheritance, and CLOS
;;;; A simple simple semantic network about animals.
;;;; Edited by: George Lukas < last update: Jan/19/2016 - 01:01PM >

;;; These functions define a simple semantic network about animals.
(setf (get 'animal 'covering) 'skin)
(setf (get 'bird 'covering) 'feathers)
(setf (get 'bird 'travel) 'flies)
(setf (get 'bird 'isa) 'animal)
(setf (get 'fish 'isa) 'animal)
(setf (get 'fish 'travel) 'swim)
(setf (get 'ostrich 'isa) 'bird)
(setf (get 'ostrich 'travel) 'walk)
(setf (get 'penguin 'isa) 'bird)
(setf (get 'penguin 'travel) 'walk)
(setf (get 'penguin 'color) 'brown)
(setf (get 'opus 'isa) 'penguin)
(setf (get 'canary 'isa) 'bird)
(setf (get 'canary 'color) 'yellow)
(setf (get 'canary 'sound) 'sing)
(setf (get 'tweety 'isa) 'canary)
(setf (get 'tweety 'color) 'white)
(setf (get 'robin 'isa) 'bird)
(setf (get 'robin 'sound) 'sings)
(setf (get 'robin 'color) 'red)

;;; These functions define a depth first inheritance search of a 
;;; semantic network:
(defun inherit-get (object property)
  (or (get object property)
      (get-from-parents (get object 'isa)
			property)))

(defun get-from-parents (parents property)
  (cond
    ((null parents) nil)
    ((atom parents) (inherit-get parents property))
    (t (or (get-from-parents (first parents) property)
	   (get-from-parents (rest parents) property)))))
