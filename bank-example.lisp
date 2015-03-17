(defvar *account-numbers* 1000)

(defclass bank-account (account)
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))

(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
            (t :bronze)))))

(defclass checking-account (bank-account) ())

(defclass savings-account (bank-account) ())

(defclass overdraft-account (account) ())

(defgeneric balance (account))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))

(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified AMOUNT from the ACCOUNT.
Signal an error if the current balance is less than the AMOUNT."))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))

(defmethod withdraw :before ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))

(defmethod withdraw :before ((account (eql *account-of-bank-president*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))))

;; -----------------------------------------------------------------------------

