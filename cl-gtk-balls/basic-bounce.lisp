(defpackage :bounce
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :bounce)

(defparameter +num-balls+ 150)

(defparameter +canvas-width+ 800)
(defparameter +canvas-height+ 600)
(defparameter +target-fps+ 60.0)
(defparameter +time-step+ (/ +target-fps+))

(defparameter +max-dx+ (* 1/2 +canvas-width+))
(defparameter +max-dy+ (* 1/2 +canvas-height+))

(defparameter +min-radius+ 8)
(defparameter +max-radius+ 25)

(defparameter +g-constant+ -1000)

(defun random-range (lower upper)
  (+ lower (random (- upper lower))))

(defun timestamp ()
  (let ((day-names '("Monday" "Tuesday" "Wednesday"
                     "Thursday" "Friday" "Saturday" "Sunday")))
    (multiple-value-bind
          (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
      (declare (ignore dst-p))
      (format nil "~a ~d/~2,'0d/~d ~2,'0d:~2,'0d:~2,'0d (GMT~@d)"
              (nth day-of-week day-names) month date
              year hour minute second (- tz)))))

(defun make-ball ()
  (flet ((in-bounds-p (ball)
           (and (>= (getf ball :x) (getf ball :radius))
                (<= (getf ball :x) (- +canvas-width+ (getf ball :radius)))
                (>= (getf ball :y) (getf ball :radius))
                (<= (getf ball :y) (- +canvas-height+ (getf ball :radius)))))
         (make-ball ()
           (let* ((radius (random-range +min-radius+ +max-radius+))
                  (y (+ (random +canvas-height+) radius))
                  (dy (random-range (- +max-dy+) +max-dy+)))
             (list :time-step +time-step+
                   :radius radius
                   :x (random +canvas-width+)
                   :y y
                   :dx (random-range (- +max-dx+) +max-dx+)
                   :dy dy
                   :max-speed (sqrt (* 2 (+ (abs (* +g-constant+ (- y radius)))
                                            (* 0.5 dy dy))))
                   :color (random-color)))))
    (do ((ball (make-ball) (make-ball)))
        ((in-bounds-p ball) ball))))

(defun update-ball (ball)
  (incf (getf ball :x) (* (getf ball :dx) +time-step+))
  (when (or (< (getf ball :x) (getf ball :radius))
            (> (getf ball :x) (- +canvas-width+ (getf ball :radius))))
    (setf (getf ball :dx) (- (getf ball :dx))))
  (incf (getf ball :dy) (* +g-constant+ +time-step+))
  (incf (getf ball :y) (* (getf ball :dy) +time-step+))
  (when (< (getf ball :y) (getf ball :radius))
    (setf (getf ball :dy) (getf ball :max-speed))
    (setf (getf ball :y) (getf ball :radius))))

(defun draw-ball (ball cr)
  (apply #'cairo-set-source-rgb (cons cr (getf ball :color)))
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
   (let ((runningp nil)
         (balls (loop repeat +num-balls+ collect (make-ball)))
         (window (make-instance 'gtk-window
                                :type :toplevel
                                :title "Bouncing Balls"))
         (canvas (make-instance 'gtk-drawing-area
                                :width-request +canvas-width+
                                :height-request +canvas-height+))
         (box (gtk-box-new :vertical 2))
         (run-button (gtk-button-new-with-label "Start"))
         (add-button (gtk-button-new-with-label "Add")))
     (labels ((update (&optional widget)
                (declare (ignore widget))
                (dolist (ball balls) (update-ball ball))
                (gtk-widget-queue-draw canvas)
                t))
       (g-signal-connect window "destroy" (lambda (widget)
                                            (declare (ignore widget))
                                            (leave-gtk-main)))
       (g-signal-connect run-button "clicked"
                         (lambda (widget)
                           (declare (ignore widget))
                           (g-timeout-add (floor (* 1000 +time-step+))
                                          #'update)))
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
       (gtk-box-pack-start box run-button)
       (gtk-box-pack-start box add-button)
       (gtk-widget-show-all window)))))
