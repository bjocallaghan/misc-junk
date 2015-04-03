(in-package :village)

(defgeneric connect (sender destination))
(defgeneric disconnect (sender destination))
(defgeneric update (object))

(defclass wired-object ()
  ((subscribers
    :accessor subscribers
    :initform nil)))

(defmethod connect ((sender wired-object) destination)
  (setf (subscribers sender)
        (cons destination (subscribers sender))))

(defmethod disconnect ((sender wired-object) destination)
  (setf (subscribers sender) (remove-if (lambda (x) (eq x destination))
                                        (subscribers sender))))

(defmethod update :after ((sender wired-object))
  (dolist (subscriber (subscribers sender))
    (update subscriber)))
