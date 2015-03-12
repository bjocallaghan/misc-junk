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

(defclass bank-account ()
  (customer-name
   balance))

(defclass checking-account (bank-account))

(defclass savings-account (bank-account))
