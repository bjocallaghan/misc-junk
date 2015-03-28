(defpackage :bounce
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :bounce)

(defparameter +num-balls+ 150)

(defparameter +canvas-width+ 600)
(defparameter +canvas-height+ 400)
(defparameter +target-fps+ 60.0)
(defparameter +time-step+ (/ +target-fps+))

(defparameter +max-dx+ (* 1/2 +canvas-width+))
(defparameter +max-dy+ (* 1/2 +canvas-height+))

(defparameter +min-radius+ 2)
(defparameter +max-radius+ 20)

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
                    (list :time-step +time-step+
                          :radius (random-range +min-radius+ +max-radius+)
                          :x (random +canvas-width+)
                          :y (random +canvas-height+)
                          :dx (random-range (- +max-dx+) +max-dx+)
                          :dy (random-range (- +max-dy+) +max-dy+)
                          :color (random-color))))
    (do ((ball (make-ball) (make-ball)))
        ((in-bounds-p ball) ball))))

(defun update-ball (ball)
  (incf (getf ball :x) (* (getf ball :dx) +time-step+))
  (incf (getf ball :y) (* (getf ball :dy) +time-step+))
  (when (or (< (getf ball :x) (getf ball :radius))
            (> (getf ball :x) (- +canvas-width+ (getf ball :radius))))
    (setf (getf ball :dx) (- (getf ball :dx))))
  (when (or (< (getf ball :y) (getf ball :radius))
            (> (getf ball :y) (- +canvas-height+ (getf ball :radius))))
    (setf (getf ball :dy) (- (getf ball :dy)))))

    ;;     self.dy += G_CONSTANT * self.time_step
    ;;     self.y += (self.dy * self.time_step)
    ;;     if (self.y-self.radius) < 0:
    ;;         self.y = self.radius
    ;;         self.temp_max_speed *= .8
    ;;         if self.temp_max_speed < .2 * self.max_speed:
    ;;             self.temp_max_speed = self.max_speed
    ;;         self.dy = self.temp_max_speed


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
  (flet ((rand-val ()
                   (/ (float (random 100)) 100)))
    (list (rand-val) (rand-val) (rand-val))))

(defun bounce-main ()
  (within-main-loop
   (let ((balls (loop repeat +num-balls+ collect (make-ball)))
         (window (make-instance 'gtk-window
                                :type :toplevel
                                :title "Bouncing Balls"))
         (canvas (make-instance 'gtk-drawing-area
                                :width-request +canvas-width+
                                :height-request +canvas-height+))
         (box (gtk-box-new :vertical 2))
         (step-button (gtk-button-new-with-label "Step"))
         (add-button (gtk-button-new-with-label "Add")))
     (labels ((update (widget)
                      (declare (ignore widget))
                      (dolist (ball balls) (update-ball ball))
                      (gtk-widget-queue-draw canvas)
                      t))
       (g-signal-connect window "destroy" (lambda (widget)
                                            (declare (ignore widget))
                                            (leave-gtk-main)))
       (g-signal-connect step-button "clicked" #'update)
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
       (gtk-box-pack-start box step-button)
       (gtk-box-pack-start box add-button)
       (gtk-widget-show-all window)))))
