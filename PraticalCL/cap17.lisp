;;;; cap17.lisp - Object Reorientation:Classes
;;;; George Lukas < last update: Jan/01/2016 - 02:05PM >

;;; Slot Specifiers
;;; Object Initialization
(defvar *account-numbers* 0)

(defclass bank-accont ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance 
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))

(defmethod initialize-instance :after ((account bank-accont) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
	  (cond
	    ((>= balance 100000) :gold)
	    ((>= balance 50000) :silver)
	    (t :bronze)))))

(defmethod initialize-instance :after ((account bank-accont)
				       &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
	  (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))

;;; Accessor Functions
(defgeneric balance (account))

(defmethod balance ((account balance-account))
  (slot-value account 'balance))

(defun (setf customer-name) (name account)
  (setf (slot-value account 'customer-name) name))

(defgeneric (setf customer-name) (value account))

(defmethod (setf customer-name) (value (account bank-accont))
  (setf (slot-value account 'customer-name) value))

(defgeneric customer-name (account))

(defmethod customer-name ((account bank-accont))
  (slot-value account 'customer-name))

(defclass bank-accont ()
  ((customer-name
    :initarg :customer-name
    :initform (error "must supply a customer name.")
    :accessor customer-name
    :documentation "Customer's name")
   (balance 
    :initarg :balance 
    :initform 0
    :reader balance
    :documentation "Current account balance")
   (account-number
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank.")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :silver, or bronze.")))

;;; WITH-SLOTS and WITH-ACCESSORS 
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))

(defmethod merge-accounts ((account1 bank-account) (account2 bank-account))
    (with-accessors ((balance1 balance)) account1
      (with-accessors ((balance2 balance)) account2
	(incf balance1 balance2)
	(setf balance2 0))))
