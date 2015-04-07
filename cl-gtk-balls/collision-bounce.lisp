(defpackage :bounce
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :bounce)

(defparameter +num-balls+ 2)

(defparameter +canvas-width+ 800)
(defparameter +canvas-height+ 600)
(defparameter +target-fps+ 60.0)
(defparameter +time-step+ (/ +target-fps+))

(defparameter +max-dx+ (* 1/2 +canvas-width+))
(defparameter +max-dy+ (* 1/2 +canvas-height+))

(defparameter +min-radius+ 8)
(defparameter +max-radius+ 25)

(defparameter +g-constant+ -1000)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'black) (defconstant black '(0.0 0.0 0.0)))
  (unless (boundp 'white) (defconstant white '(1.0 1.0 1.0)))
  (unless (boundp 'red) (defconstant red '(1.0 0.0 0.0)))
  (unless (boundp 'light-blue) (defconstant light-blue '(0.6 0.6 1.0)))
  (unless (boundp 'dark-blue) (defconstant dark-blue '(0.0 0.0 1.0)))
  (unless (boundp 'deep-green) (defconstant deep-green '(0.8 0.4 0.2)))
  (unless (boundp 'purple) (defconstant purple '(1.0 0.0 1.0))))
(defmacro cairo-set-source-color (cr color)
  `(apply #'cairo-set-source-rgb (cons ,cr ,color)))
(defmacro cairo-circle (cr x-center y-center radius)
  `(cairo-arc ,cr ,x-center ,y-center ,radius 0 (* 2 pi)))

(defun random-integer (lower upper)
  "Returns a random integer between LOWER and UPPER (inclusive)."
  (+ lower (random (1+ (- upper lower)))))

(defgeneric energy (object))
(defgeneric mass (object))
(defgeneric update (object))
(defgeneric draw (cr object))

(defclass ball ()
  ((density
    :initarg :density
    :accessor density
    :initform (error "must specify density"))
   (radius
    :initarg :radius
    :accessor radius
    :initform (error "must specify radius"))
   (x
    :initarg :x
    :accessor x
    :initform (error "must specify x"))
   (y
    :initarg :y
    :accessor y
    :initform (error "must specify y"))
   (dx
    :initarg :dx
    :accessor dx
    :initform (error "must specify dx"))
   (dy
    :initarg :dy
    :accessor dy
    :initform (error "must specify dy"))
   (color
    :initarg :color
    :accessor color
    :initform (error "must specify color"))))

(defun random-ball ()
  (let ((radius (random-integer +min-radius+ +max-radius+)))
    (make-instance 'ball
                   :density 1
                   :radius radius
                   :x (random-integer radius (- +canvas-width+ radius))
                   :y (random-integer radius (- +canvas-height+ radius))
                   :dx (random-integer 1 5)
                   :dy (random-integer 1 5)
                   :color (random-color))))

(defun ball-1 ()
  (make-instance 'ball
                 :density 1
                 :radius 10
                 :x 20
                 :y 300
                 :dx 10
                 :dy 0
                 :color

(defmethod update ((ball ball))
  (with-slots (radius x y dx dy) ball
    (when (or (> (+ x radius) +canvas-width+)
              (< (- x radius) 0))
      (setf dx (- dx)))
    (when (or (> (+ y radius) +canvas-height+)
              (< (- y radius) 0))
      (setf dy (- dy)))
    (incf x dx)
    (incf y dy)))

(defmethod draw (cr (ball ball))
  (with-slots (x y color radius) ball
    (cairo-set-source-color cr color)
    (cairo-circle cr x y radius)
    (cairo-fill cr)
    (cairo-set-line-width cr 1)
    (cairo-set-source-color cr black)
    (cairo-circle cr x y radius)
    (cairo-stroke cr)))
  
(defun random-color ()
  (let ((base (list (random 1.0) (random 1.0) (random 1.0))))
    (mapcar (lambda (x) (/ x (apply #'max base))) base)))

(defun main ()
  (within-main-loop
    (let ((runningp t)
          (balls (loop repeat +num-balls+ collect (random-ball)))
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Bouncing Balls"))
          (canvas (make-instance 'gtk-drawing-area
                                 :width-request +canvas-width+
                                 :height-request +canvas-height+))
          (box (gtk-box-new :vertical 2))
          (start-button (gtk-button-new-with-label "Start"))
          (stop-button (gtk-button-new-with-label "Stop"))
          (add-button (gtk-button-new-with-label "Add")))
      (labels ((start-animation ()
                 (g-timeout-add (floor (* 1000 +time-step+)) #'update-all))
               (update-all (&optional widget)
                 (declare (ignore widget))
                 (dolist (ball balls) (update ball))
                 (gtk-widget-queue-draw canvas)
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
        (g-signal-connect add-button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (push (random-ball) balls)
                            (gtk-widget-queue-draw canvas)
                            t))
        (g-signal-connect canvas "draw"
                          (lambda (widget cr)
                            (declare (ignore widget))
                            (let ((cr (pointer cr)))
                              (cairo-set-source-rgb cr 1 1 1)
                              (cairo-paint cr)
                              (dolist (ball balls) (draw cr ball))
                              (cairo-destroy cr)
                              t)))
        (gtk-container-add window box)
        (gtk-box-pack-start box canvas)
        (gtk-box-pack-start box start-button)
        (gtk-box-pack-start box stop-button)
        (gtk-box-pack-start box add-button)
        (gtk-widget-show-all window)
        (start-animation)
        ))))
