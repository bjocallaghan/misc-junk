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
  (cairo-arc cr (x actor) (- +drawing-area-height+ (y actor)) 5 0 (* 2 pi))
  (cairo-fill cr)
  (cairo-set-source-rgb cr 0.0 0.0 0.0)
  (cairo-set-line-width cr 1)
  (cairo-arc cr (x actor) (- +drawing-area-height+ (y actor)) 5 0 (* 2 pi))
  (cairo-stroke cr)
  (cairo-move-to cr (+ 5 (x actor)) (+ 5 (- +drawing-area-height+ (y actor))))
  (cairo-select-font-face cr "serif" 0 1)
  (cairo-set-font-size cr 10.0)
  (cairo-show-text cr (format nil "~a (~a)" (name actor) (status actor))))

(defmethod update (actor))

(defclass mover (actor)
  ((direction-fn
    :initarg :direction-fn
    :accessor direction-fn
    :initform (make-meander))
   (motivation
    :initarg :motivation
    :accessor motivation
    :initform 1.0)
   (heading
    :accessor heading
    :initform (random-heading))
   (max-speed
    :initarg :max-speed
    :accessor max-speed
    :initform 1.0)
   (name
    :initform 'mover)
   (status
    :initform 'meandering)))

(defmethod draw ((actor mover) cr)
  (cairo-set-source-rgb cr 0.0 0.0 1.0)
  (cairo-set-line-width cr 3.0)
  (cairo-move-to cr (x actor) (- +drawing-area-height+ (y actor)))
  (cairo-line-to cr
                 (+ (x actor) (* 15 (cos (heading actor))))
                 (- +drawing-area-height+ (+ (y actor) (* 9 (sin (heading actor))))))
  (cairo-stroke cr)
  (call-next-method))

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
  (when (< (random 1.0) (motivation mover))
    (let ((heading (funcall (direction-fn mover)))
          (speed (max-speed mover)))
      (setf (heading mover) heading)
      (incf (x mover) (* speed (cos heading)))
      (incf (y mover) (* speed (sin heading))))))

(defun distance (one two)
  (sqrt (+ (expt (- (x one) (x two)) 2)
           (expt (- (y one) (y two)) 2))))

(defmethod update ((mover sheep))
  (let* ((others (remove-if (lambda (x) (eq x mover)) (herd mover)))
         (nearest (car (sort others #'< :key (lambda (x) (distance x mover)))))
         (dist (distance nearest mover))
         (new-status (cond
                           ((> dist 200) 'isolated)
                           ((> dist 60) 'lonely)
                           ((< dist 20) 'crowded)
                           (t 'content))))
    (unless (eq new-status (status mover))
      (setf (status mover) new-status)
      (cond
        ((eq (status mover) 'isolated)
         (progn
           (setf (motivation mover) .2)
           (setf (direction-fn mover) (make-meander (heading mover)))
           ))
        ((eq (status mover) 'lonely)
         (progn
           (setf (motivation mover) .4)
           (setf (direction-fn mover) (make-seek mover nearest))
           ))
        ((eq (status mover) 'crowded)
         (progn
           (setf (motivation mover) .5)
           (setf (direction-fn mover) (make-avoid mover nearest))
           ))
        ((eq (status mover) 'content)
         (progn
           (setf (motivation mover) .1)
           (setf (direction-fn mover) (make-meander (heading mover)))
           ))))    
    (setf (motivation mover) (cond 
                               ((eq (status mover) 'isolated) .2)
                               ((eq (status mover) 'lonely) .4)
                               ((eq (status mover) 'crowded) .3)
                               ((eq (status mover) 'content) .1))))
  (call-next-method))

(defun random-heading ()
  (random (* 2 pi)))

(defun make-meander (&optional heading)
  (let ((heading (if (null heading) (random-heading) heading)))
    #'(lambda ()
        (when (zerop (random 20)) (setf heading (random-heading)))
        (+ heading (random .3) -0.15))))

(defun make-seek (self friend)
  (let ((heading (bearing self friend)))
    #'(lambda ()
        (+ heading (random .3) -0.15))))

(defun make-avoid (self friend)
  (let ((heading (- (bearing self friend))))
    #'(lambda ()
        (+ heading (random .3) -0.15))))

(defun bearing (self other)
  (let ((result (atan (- (y other) (y self)) (- (x other) (x self)))))
    (if (> (x self) (x other))
        (+ result pi)
        result)))

(defun main ()
  (within-main-loop
   (let ((runningp nil)
         (actors (make-herd 20))
         (window (make-instance 'gtk-window
                                :type :toplevel
                                :title "Village"))
         (drawing-area (make-instance 'gtk-drawing-area
                                      :width-request +drawing-area-width+
                                      :height-request +drawing-area-height+))
         (box (gtk-box-new :horizontal 0))
         (ctrl-panel (gtk-box-new :vertical 0))
         (start-button (gtk-button-new-with-label "Start"))
         (stop-button (gtk-button-new-with-label "Stop"))
         (step-button (gtk-button-new-with-label "Step")))
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
       (g-signal-connect step-button "clicked"
                         (lambda (widget)
                           (declare (ignore widget))
                           (update-all)))
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
       (gtk-box-pack-start ctrl-panel step-button)
       (gtk-widget-show-all window)
       (start-animation)))))
