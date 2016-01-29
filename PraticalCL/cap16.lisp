;;;; cap16.lisp - A Portable Pathname Library
;;;; George Lukas < last update: Jan/01/2016 - 11:39AM >

;;; Generic Functions and Methods
;;; A generic function defines an abstract operation, 
;;; specifying its name and a parameter list but no implementation.
(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amound from the account.
Signal an error if the current balance is less than amount."))

;;; Methods indicate what kinds of arguments 
;;; they can handle by 'specializing' the required
;;; parameters defined by the generic function
(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))

(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (whithdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

(defmethod whithdraw ((proxy proxy-account) amount)
  (whithdraw (proxied-account proxy) amount))

(defmethod whithdraw ((account (eql *account-bank-of-president*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))
    (call-next-method)))

;;; Method Combination
;;; The Standard Method Combination
(defmethod whithdraw :before ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (whithdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))
