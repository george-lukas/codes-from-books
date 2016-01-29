;;;; AI algorithms, data structures, and idioms in Prolog, Lisp, and Java
;;;; pblm.lisp - Chapter19 - Machine Learning in Lisp
;;;; This a simple credit assessment example to demonstrate the ID3 algorithm
;;;; Edited by: George Lukas < last update: Jan/18/2016 - 07:30PM >

;;; The set of training 
(defparameter *examples*
  '(((risk . high) (history . bad) (debt . high) (collateral . none) (income . 0-to-15k))
    ((risk . high) (history . unknown) (debt . high) (collateral . none) (income . 15k-to-35k))
    ((risk . moderate)(history . unknown) (debt . low) (collateral . none) (income . 15k-to-35k))
    ((risk . high)(history . unknown) (debt . low) (collateral . none) (income . 0-to-15k))
    ((risk . low)(history . unknown) (debt . low) (collateral . none) (income . over-35k))
    ((risk . low)(history . unknown) (debt . low) (collateral . adequate) (income . over-35k))
    ((risk . high)(history . bad) (debt . low) (collateral . none) (income . 0-to-15k))
    ((risk . moderate)(history . bad) (debt . low) (collateral . adequate) (income . over-35k))
    ((risk . low)(history . good) (debt . low) (collateral . none) (income . over-35k))
    ((risk . low)(history . good) (debt . high) (collateral . adequate) (income . over-35k))
    ((risk . high)(history . good) (debt . high) (collateral . none) (income . 0-to-15k))
    ((risk . moderate)(history . good) (debt . high) (collateral . none) (income . 15k-to-35k))
    ((risk . low)(history . good) (debt . high) (collateral . none) (income . over-35k))
    ((risk . high)(history . bad) (debt . high) (collateral . none) (income . 15k-to-35k))))

;;; More examples set
(defparameter *test-set*
  (list
   (make-property 
    :name 'history
    :test #'history
    :values '(good bad unknown))
   (make-property 
    :name 'debt
    :test #'debt
    :values '(high low))
   (make-property 
    :name 'collateral
    :test #'collateral
    :values '(none adequate))
   (make-property 
    :name 'income
    :test #'income
    :values '(0-to-15k 15k-to-35k over-35k))))

(defparameter *classifier* 
  (make-property 
   :name 'risk
   :test #'risk
   :values '(high moderate low)))

;;; this is the example frame for this test problem:
(defparameter *credit-examples* 
  (make-example-frame 
   :instances *examples* 
   :properties *test-set*
   :classifier classifier
   :size (list-length *examples*)
   :information (compute-information *examples* *classifier*)))

;;; Given this representation of 'objects', we next define 'property':
(defun history (object)
  (rest (assoc 'history object :test #'equal)))

(defun debt (object)
  (rest (assoc 'debt object :test #'equal)))

(defun collateral (object)
  (rest (assoc 'collateral object :test #'equal)))

(defun income (object)
  (rest (assoc 'income object :test #'equal)))

(defun risk (object)
  (rest (assoc 'risk object :test #'equal)))
