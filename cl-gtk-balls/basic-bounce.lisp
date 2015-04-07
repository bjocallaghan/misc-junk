(defpackage :bounce
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :bounce)

(defparameter +num-balls+ 30)

(defconstant +canvas-width+ 800)
(defconstant +canvas-height+ 600)
(defconstant +target-fps+ 60.0)
(defconstant +time-step+ (/ +target-fps+))

(defconstant +max-dx+ (* 1/2 +canvas-width+))
(defconstant +max-dy+ (* 1/2 +canvas-height+))

(defconstant +min-radius+ 8)
(defconstant +max-radius+ 25)

(defconstant +g-constant+ -1000)

(defun random-from-range (lower upper)
  (declare (type (integer -32000 32000) lower upper))
  (+ lower (random (- upper lower))))

(defun make-ball ()
  (let* ((radius (random-from-range +min-radius+ +max-radius+))
         (color (random-color))
         (y (+ (random +canvas-height+) radius (* .1 +canvas-height+)))
         (dy (random-from-range (- +max-dy+) +max-dy+)))
    (list :time-step +time-step+
          :radius radius
          :x (random-from-range radius (- +canvas-width+ radius))
          :y y
          :dx (random-from-range (- +max-dx+) +max-dx+)
          :dy dy
          :max-speed (sqrt (* 2 (+ (abs (* +g-constant+ (- y radius)))
                                   (* 0.5 dy dy))))
          :color color
          :current-color color)))

(defun update-ball (ball)
  (incf (getf ball :x) (* (getf ball :dx) +time-step+))
  (when (or (< (getf ball :x) (getf ball :radius))
            (> (getf ball :x) (- +canvas-width+ (getf ball :radius))))
    (setf (getf ball :dx) (- (getf ball :dx))))
  (incf (getf ball :dy) (* +g-constant+ +time-step+))
  (incf (getf ball :y) (* (getf ball :dy) +time-step+))
  (when (< (getf ball :y) (getf ball :radius))
    (setf (getf ball :dy) (getf ball :max-speed))
    (setf (getf ball :y) (getf ball :radius)))
  (setf (getf ball :current-color)
        (mapcar (lambda (x)
                  (* x (sqrt (- 1 (/ (abs (getf ball :dy)) (getf ball :max-speed))))))
                (getf ball :color))))

(defun draw-ball (ball cr)
  (apply #'cairo-set-source-rgb (cons cr (getf ball :current-color)))
  (cairo-arc cr (getf ball :x) (- +canvas-height+ (getf ball :y))
             (getf ball :radius) 0 (* 2 pi))
  (cairo-fill cr)
  (cairo-set-line-width cr 1)
  (cairo-set-source-rgb cr 0.0 0.0 0.0)
  (cairo-arc cr (getf ball :x) (- +canvas-height+ (getf ball :y))
             (getf ball :radius) 0 (* 2 pi))
  (cairo-stroke cr))

(defun random-color ()
  (let ((base (list (random 1.0) (random 1.0) (random 1.0))))
    (mapcar (lambda (x) (/ x (apply #'max base))) base)))

(defun bounce-main ()
  (within-main-loop
   (let ((runningp t)
         (balls (loop repeat +num-balls+ collect (make-ball)))
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
                (g-timeout-add (floor (* 1000 +time-step+)) #'update))
              (update (&optional widget)
                (declare (ignore widget))
                (dolist (ball balls) (update-ball ball))
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
                           (let ((ball (make-ball)))
                             (push ball balls))
                           (gtk-widget-queue-draw canvas)
                           t))
       (g-signal-connect canvas "draw"
                         (lambda (widget cr)
                           (declare (ignore widget))
                           (let ((cr (pointer cr)))
                             (cairo-set-source-rgb cr 1.0 1.0 1.0)
                             (cairo-paint cr)
                             (dolist (ball balls) (draw-ball ball cr))
                             (cairo-destroy cr)
                             t)))
       (gtk-container-add window box)
       (gtk-box-pack-start box canvas)
       (gtk-box-pack-start box start-button)
       (gtk-box-pack-start box stop-button)
       (gtk-box-pack-start box add-button)
       (gtk-widget-show-all window)
       (start-animation)))))

(defun main ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
;                                 :type :toplevel
                                 :title "Bouncing Balls")))
      (gtk-container-add window (gtk-label-new "hello windows 7 gtk world"))
      (gtk-widget-show-all window))))
