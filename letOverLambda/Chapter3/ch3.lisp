;;;; ch3.lisp - Chapter 3: Macro Basics
;;;; George Lukas < last update: Jan/09/2016 - 07:44PM >

;;; 3.2 - DSL: Domain Specific Language
(defmacro sleep-units (value unit)
  `(sleep
    (* ,value
       ,(case unit
	      ((s) 1)
	      ((m) 60)
	      ((h) 3600)
	      ((d) 86400)
	      ((ms 1/1000))))))

(defmacro unit-of-time (value unit)
  `(* ,value 
      ,(case unit 
	     ((s) 1)
	     ((m) 60)
	     ((h) 3600)
	     ((d) 86400)
	     ((ms) 1/1000)
	     ((us) 1/1000000))))

;;; 3.3 - Control Structures
(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'first letargs)
	      ,@body))
     (,n ,@(mapcar #'second letargs))))

(defun nlet-fact (n)
  (nlet fact ((n n))
	(if (zerop n) 1
	    (* n (fact (- n 1))))))
