(defpackage :village
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :village)

(defparameter +drawing-area-width+ 1100)
(defparameter +drawing-area-height+ 650)

(defconstant +target-fps+ 30.0)
(defconstant +time-step+ (/ +target-fps+))

(defclass actor ()
  ((x
    :initarg :x
    :accessor x
    :initform (random +drawing-area-width+))
   (y
    :initarg :y
    :accessor y
    :initform (random +drawing-area-height+))
   (name
    :initarg :name
    :accessor name
    :initform 'unnamed)
   (status
    :initarg :status
    :accessor status
    :initform 'unknown)))

(defgeneric draw (actor cr))
(defgeneric update (actor))

(defmethod draw (actor cr)
  (cairo-set-source-rgb cr 0.8 0.4 0.2)
  (cairo-arc cr (x actor) (- +drawing-area-height+ (y actor)) 3 0 (* 2 pi))
  (cairo-fill cr)
  (cairo-set-source-rgb cr 0.0 0.0 0.0)
  (cairo-set-line-width cr 1)
  (cairo-arc cr (x actor) (- +drawing-area-height+ (y actor)) 3 0 (* 2 pi))
  (cairo-stroke cr)
  (cairo-move-to cr (+ 5 (x actor)) (+ 5 (- +drawing-area-height+ (y actor))))
  (cairo-select-font-face cr "serif" 0 1)
  (cairo-set-font-size cr 10.0)
  (cairo-show-text cr (format nil "~a (~a)" (name actor) (status actor))))

(defmethod update (actor))

(defclass mover (actor)
  ((move-fn
    :initarg :move-fn
    :accessor move-fn
    :initform (make-meander))))

(defmethod update (mover)
  (let ((new-coords (funcall (move-fn mover) (x mover) (y mover))))
    (setf (x mover) (first new-coords))
    (setf (y mover) (second new-coords))
    (list (x mover) (y mover))))

(defun make-meander ()
  (flet ((random-direction ()
           (nth (random 4) '(up down left right))))
    (let ((heading (random-direction)))
      #'(lambda (x y)
          (when (zerop (random 10)) (setf heading (random-direction)))
          (let ((new-coords (unless (zerop (random 3))
                              (case heading
                                (up (list x (1+ y)))
                                (down (list x (1- y)))
                                (left (list (1- x) y))
                                (right (list (1+ x) y))))))
            (if new-coords
                new-coords
                (list x y)))))))

(defun main ()
  (within-main-loop
   (let ((runningp nil)
         (actors (loop repeat 30 collect (make-instance 'mover)))
         (window (make-instance 'gtk-window
                                :type :toplevel
                                :title "Village"))
         (drawing-area (make-instance 'gtk-drawing-area
                                      :width-request +drawing-area-width+
                                      :height-request +drawing-area-height+))
         (box (gtk-box-new :horizontal 0))
         (ctrl-panel (gtk-box-new :vertical 0))
         (start-button (gtk-button-new-with-label "Start"))
         (stop-button (gtk-button-new-with-label "Stop")))
     (labels ((start-animation ()
                (g-timeout-add (floor (* 1000 +time-step+)) #'update-all))
              (update-all (&optional widget)
                (declare (ignore widget))
                (dolist (actor actors) (update actor))
                (gtk-widget-queue-draw drawing-area)
                runningp))
       (g-signal-connect window "destroy" (lambda (widget)
                                            (declare (ignore widget))
                                            (leave-gtk-main)))
       (g-signal-connect start-button "clicked"
                         (lambda (widget)
                           (declare (ignore widget))
                           (unless runningp
                             (setf runningp t)
                             (start-animation))))
       (g-signal-connect stop-button "clicked"
                         (lambda (widget)
                           (declare (ignore widget))
                           (setf runningp nil)))
       (g-signal-connect drawing-area "draw"
                         (lambda (widget cr)
                           (declare (ignore widget))
                           (let ((cr (pointer cr)))
                             (cairo-set-source-rgb cr 0.2 1.0 0.0)
                             (cairo-paint cr)
                             (dolist (actor actors) (draw actor cr))
                             (cairo-destroy cr)
                             t)))
       (gtk-container-add window box)
       (gtk-box-pack-start box drawing-area)
       (gtk-box-pack-start box ctrl-panel)
       (gtk-box-pack-start ctrl-panel start-button)
       (gtk-box-pack-start ctrl-panel stop-button)
       (gtk-widget-show-all window)
       (start-animation)))))
