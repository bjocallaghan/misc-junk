(defpackage :bounce
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :bounce)

(defparameter +num-balls+ 30)

(defparameter +canvas-width+ 800)
(defparameter +canvas-height+ 600)
(defparameter +target-fps+ 60.0)
(defparameter +time-step+ (/ +target-fps+))

(defparameter +max-dx+ (* 1/2 +canvas-width+))
(defparameter +max-dy+ (* 1/2 +canvas-height+))

(defparameter +min-radius+ 10)
(defparameter +max-radius+ 15)

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
(defgeneric collision (object-1 object2))
(defgeneric velocity (object))
(defgeneric overlapp (object-1 object-2))

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
                   :dx (random-integer 10 300)
                   :dy (random-integer 10 300)
                   :color (random-color))))

(defmethod mass ((ball ball))
  (with-slots (radius density) ball
    (* pi radius radius density)))

(defmethod velocity ((ball ball))
  (with-slots (dx dy) ball
    (sqrt (+ (* dx dx) (* dy dy)))))

(defmethod energy ((ball ball))
  (* .5 (mass ball) (velocity ball) (velocity ball)))

(defmethod update ((ball ball))
  (with-slots (radius x y dx dy) ball
    (when (or (> (+ x radius) +canvas-width+)
              (< (- x radius) 0))
      (setf dx (- dx)))
    (when (or (> (+ y radius) +canvas-height+)
              (< (- y radius) 0))
      (setf dy (- dy)))
    (incf x (* +time-step+ dx))
    (incf y (* +time-step+ dy))))

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

(defmethod collision ((ball-1 ball) (ball-2 ball))
  ;;; see http://en.wikipedia.org/wiki/Elastic_collision the new-dx and new-dy
  ;;; functions are taken directly from the physics equations presented there
  (labels ((movement-angle (ball)
             (with-slots (dx dy) ball
               (atan dy dx)))
           (contact-angle (ball other)
             (atan (- (y other) (y ball)) (- (x other) (x ball))))
           (new-dy (ball other)
             (let ((phi (contact-angle ball other))
                   (theta-1 (movement-angle ball))
                   (theta-2 (movement-angle other))
                   (v1 (velocity ball))
                   (m1 (mass ball))
                   (v2 (velocity other))
                   (m2 (mass other)))
               (+ (* (/ (+ (* v1 (cos (- theta-1 phi)) (- m1 m2)) (* 2 m2 v2 (cos (- theta-2 phi)))) (+ m1 m2)) (sin phi)) (* v1 (sin (- theta-1 phi)) (sin (+ phi (/ pi 2)))))))
           (new-dx (ball other)
             (let ((phi (contact-angle ball other))
                   (theta-1 (movement-angle ball))
                   (theta-2 (movement-angle other))
                   (v1 (velocity ball))
                   (m1 (mass ball))
                   (v2 (velocity other))
                   (m2 (mass other)))
               (+ (* (/ (+ (* v1 (cos (- theta-1 phi)) (- m1 m2)) (* 2 m2 v2 (cos (- theta-2 phi)))) (+ m1 m2)) (cos phi)) (* v1 (sin (- theta-1 phi)) (cos (+ phi (/ pi 2))))))))
    (let ((new-dx-1 (new-dx ball-1 ball-2))
          (new-dx-2 (new-dx ball-2 ball-1))
          (new-dy-1 (new-dy ball-1 ball-2))
          (new-dy-2 (new-dy ball-2 ball-1)))
      (with-slots (dx dy) ball-1
        (setf dx new-dx-1)
        (setf dy new-dy-1))
      (with-slots (dx dy) ball-2
        (setf dx new-dx-2)
        (setf dy new-dy-2)))
    (loop
       do (progn (update ball-1) (update ball-2))
       while (overlapp ball-1 ball-2))))

(defun distance (one two)
  (sqrt (+ (expt (- (x one) (x two)) 2)
           (expt (- (y one) (y two)) 2))))

(defmethod overlapp ((ball-1 ball) (ball-2 ball))
  (when (< (distance ball-1 ball-2)
           (+ (radius ball-1) (radius ball-2)))
    t))

(defun do-collisions (balls)
  (labels ((check-ball-against (ball others)
             (when others
               (when (overlapp ball (car others)) (collision ball (car others)))
               (check-ball-against ball (cdr others)))))
    (when balls
      (check-ball-against (car balls) (cdr balls))
      (do-collisions (cdr balls)))))

(defun main ()
  (within-main-loop
    (let ((runningp nil)
          (balls (loop repeat +num-balls+ collect (random-ball)))
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Bouncing Balls"))
          (canvas (make-instance 'gtk-drawing-area
                                 :width-request +canvas-width+
                                 :height-request +canvas-height+))
          (box (gtk-box-new :vertical 2))
          (energy-label (gtk-label-new "<uninit>"))
          (start-button (gtk-button-new-with-label "Start"))
          (stop-button (gtk-button-new-with-label "Stop"))
          (step-button (gtk-button-new-with-label "Step"))
          (add-button (gtk-button-new-with-label "Add")))
      (labels ((start-animation ()
                 (g-timeout-add (floor (* 1000 +time-step+)) #'update-all))
               (update-all (&optional widget)
                 (declare (ignore widget))
                 (dolist (ball balls) (update ball))
                 (gtk-widget-queue-draw canvas)
                 (do-collisions balls)
                 (let ((text (format nil "Total system energy: ~2$"
                                     (reduce #'+(mapcar #'energy balls)))))
                   (gtk-label-set-text energy-label text))
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
        (g-signal-connect step-button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (update-all)
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
        (gtk-box-pack-start box step-button)
        (gtk-box-pack-start box add-button)
        (gtk-box-pack-start box energy-label)
        (gtk-widget-show-all window)
        (start-animation)))))

;;; for specific tests

(defun ball-1 ()
  (make-instance 'ball
                 :density 1
                 :radius 40
                 :x 50
                 :y 200
                 :dx 0
                 :dy 250
                 :color red))

(defun ball-2 ()
  (make-instance 'ball
                 :density 1
                 :radius 20
                 :x 50
                 :y 400
                 :dx 0
                 :dy -500
                 :color red))
