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
    :initform (make-meander))
   (name
    :initform 'mover)
   (status
    :initform 'meandering)))

(defclass sheep (mover)
  ((herd
    :initarg :herd
    :accessor herd
    :initform (error "Must supply a herd"))
   (name
    :initform 'sheep)))

(defun make-herd (num-sheep)
  (let (herd)
    (dotimes (i num-sheep)
      (push (make-instance 'sheep :herd nil) herd))
    (dolist (sheep herd)
      (setf (herd sheep) herd))
    herd))

(defmethod update (mover)
  (let ((new-coords (funcall (move-fn mover) (x mover) (y mover))))
    (setf (x mover) (first new-coords))
    (setf (y mover) (second new-coords))
    (list (x mover) (y mover))))

(defun distance (one two)
  (sqrt (+ (expt (- (x one) (x two)) 2)
           (expt (- (y one) (y two)) 2))))

(defmethod update ((mover sheep))
  (let* ((others (remove-if (lambda (x) (eq x mover)) (herd mover)))
         (dist (reduce #'min (mapcar (lambda (x) (distance x mover)) others))))
    (setf (status mover) (cond
                           ((> dist 100) 'lonely)
                           ((< dist 20) 'crowded)
                           (t 'content))))
  (call-next-method))

(defun make-meander ()
  (flet ((random-direction ()
           (random (* 2 pi))))
    (let ((heading (random-direction)))
      #'(lambda (x y)
          (when (zerop (random 20)) (setf heading (random-direction)))
          (let ((tmp-heading (+ heading (random .3) -0.15)))
            (list (+ x (cos tmp-heading))
                  (+ y (sin tmp-heading))))))))

(defun main ()
  (within-main-loop
   (let ((runningp t)
         (actors (make-herd 40))
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
