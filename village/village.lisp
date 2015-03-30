(defpackage :village
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :village)

(defparameter +drawing-area-width+ 1100)
(defparameter +drawing-area-height+ 650)

(defconstant +target-fps+ 30.0)
(defconstant +time-step+ (/ +target-fps+))

(defgeneric add-actor (environment actor))
(defgeneric update (object))
(defgeneric draw (cr object))
(defgeneric draw-label (cr actor))
(defgeneric herd (sheep))

(defclass actor ()
  ((x
    :initarg :x
    :accessor x
    :initform (error "must supply x coordinate"))
   (y
    :initarg :y
    :accessor y
    :initform (error "must supply y coordinate"))
   (name
    :initarg :name
    :accessor name
    :initform 'unnamed)
   (environment
    :initarg :environment
    :accessor environment
    :initform nil)))

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
    :initform 50)
   (name
    :initform 'mover)
   (status
    :initform 'meandering)))

(defclass sheep (mover)
  ((name
    :initform 'sheep)
   (status
    :initarg :status
    :accessor status
    :initform :content)))

(defmethod herd ((sheep sheep))
  (actors (environment sheep)))

(defmethod update ((mover mover))
  (when (< (random 1.0) (motivation mover))
    (let ((heading (funcall (direction-fn mover)))
          (speed (max-speed mover)))
      (setf (heading mover) heading)
      (incf (x mover) (* speed (time-step (environment mover)) (cos heading)))
      (incf (y mover) (* speed (time-step (environment mover)) (sin heading))))))

(defmethod update ((sheep sheep))
  (let* ((others (remove-if (lambda (x) (eq x sheep)) (herd sheep)))
         (nearest (car (sort others #'< :key (lambda (x) (distance x sheep)))))
         (dist (distance nearest sheep))
         (new-status (cond
                           ((> dist 200) :isolated)
                           ((> dist 60) :lonely)
                           ((< dist 20) :crowded)
                           (t :content))))
    (unless (and (eq new-status (status sheep))
                 (or (eq (status sheep) :content)
                     (eq (status sheep) :isolated)))
      (setf (status sheep) new-status)
      (ecase (status sheep)
        (:isolated
         (progn
           (setf (motivation sheep) .2)
           (setf (direction-fn sheep) (make-meander (heading sheep)))
           ))
        (:lonely
         (progn
           (setf (motivation sheep) .4)
           (setf (direction-fn sheep) (make-seek sheep nearest))
           ))
        (:crowded
         (progn
           (setf (motivation sheep) .5)
           (setf (direction-fn sheep) (make-avoid sheep nearest))
           ))
        (:content
         (progn
           (setf (motivation sheep) .1)
           (setf (direction-fn sheep) (make-meander (heading sheep)))
           ))))    
    (setf (motivation sheep) (cond 
                               ((eq (status sheep) :isolated) .2)
                               ((eq (status sheep) :lonely) .4)
                               ((eq (status sheep) :crowded) .3)
                               ((eq (status sheep) :content) .1))))
  (call-next-method))

(defmethod draw :around (cr object)
  (when cr (call-next-method)))

(defmethod draw (cr (actor actor))
  (cairo-set-source-rgb cr 0.8 0.4 0.2)
  (cairo-arc cr (x actor) (- +drawing-area-height+ (y actor)) 5 0 (* 2 pi))
  (cairo-fill cr)
  (cairo-set-source-rgb cr 0.0 0.0 0.0)
  (cairo-set-line-width cr 1)
  (cairo-arc cr (x actor) (- +drawing-area-height+ (y actor)) 5 0 (* 2 pi))
  (cairo-stroke cr)
  (draw-label cr actor))

(defmethod draw (cr (sheep sheep))
  (let ((color (ecase (status sheep)
                 (:content '(1.0 1.0 1.0))
                 (:crowded '(1.0 0.0 0.0))
                 (:lonely '(0.6 0.6 1.0))
                 (:isolated '(0.0 0.0 1.0)))))
    (apply #'cairo-set-source-rgb (cons cr color))
    (cairo-arc cr (x sheep) (- +drawing-area-height+ (y sheep)) 5 0 (* 2 pi))
    (cairo-fill cr)
    (cairo-set-source-rgb cr 0.0 0.0 0.0)
    (cairo-set-line-width cr 1)
    (cairo-arc cr (x sheep) (- +drawing-area-height+ (y sheep)) 5 0 (* 2 pi))
    (cairo-stroke cr)))

(defmethod draw :after (cr (actor mover))
  (cairo-set-source-rgb cr 1.0 0.0 1.0)
  (cairo-set-line-width cr 3.0)
  (cairo-move-to cr (x actor) (- +drawing-area-height+ (y actor)))
  (let ((lead-length 12))
    (cairo-line-to cr
                   (+ (x actor) (* lead-length (cos (heading actor))))
                   (- +drawing-area-height+
                      (+ (y actor) (* lead-length (sin (heading actor)))))))
  (cairo-stroke cr))

(defmethod draw-label (cr actor)
  (draw-text cr (x actor) (y actor)
             (format nil "~a" actor)))

(defmethod draw-label (cr (mover sheep))
  (draw-text cr (x mover) (y mover)
             (format nil "~a: ~a" (name mover) (status mover))))

(defun draw-text (cr x y text)
  (cairo-move-to cr (+ 5 x) (+ 5 (- +drawing-area-height+ y)))
  (cairo-select-font-face cr "serif" 0 1)
  (cairo-set-font-size cr 10.0)
  (cairo-show-text cr text))

(defun distance (one two)
  (sqrt (+ (expt (- (x one) (x two)) 2)
           (expt (- (y one) (y two)) 2))))

(defun bearing (self other)
  (atan (- (y other) (y self)) (- (x other) (x self))))

(defun bearing-test ()
  "I'm not confident bearing is working right. Need to do some coding. Not
using :rt, because I don't have that package and am not connected to the
Internet."
  (let ((s1 (make-instance 'actor :x 100 :y 100))
        (s2 (make-instance 'actor :x 100 :y 200))
        (s3 (make-instance 'actor :x 200 :y 100))
        (s4 (make-instance 'actor :x 200 :y 200)))
    (format t "~&s1 s2 should be ~3d: ~a" 90 (* 180 (/ pi) (bearing s1 s2)))
    (format t "~&s1 s2 should be ~3d: ~a" 270 (* 180 (/ pi) (bearing s2 s1)))
    (format t "~&s1 s2 should be ~3d: ~a" 0 (* 180 (/ pi) (bearing s1 s3)))
    (format t "~&s1 s2 should be ~3d: ~a" 180 (* 180 (/ pi) (bearing s3 s1)))
    (format t "~&s1 s2 should be ~3d: ~a" 45 (* 180 (/ pi) (bearing s1 s4)))
    (format t "~&s1 s2 should be ~3d: ~a" 225 (* 180 (/ pi) (bearing s4 s1)))
    (format t "~&s1 s2 should be ~3d: ~a" 315 (* 180 (/ pi) (bearing s2 s3)))
    (format t "~&s1 s2 should be ~3d: ~a" 135 (* 180 (/ pi) (bearing s3 s2)))
    (format t "~&s1 s2 should be ~3d: ~a" 0 (* 180 (/ pi) (bearing s2 s4)))
    (format t "~&s1 s2 should be ~3d: ~a" 180 (* 180 (/ pi) (bearing s4 s2)))
    (format t "~&s1 s2 should be ~3d: ~a" 90 (* 180 (/ pi) (bearing s3 s4)))
    (format t "~&s1 s2 should be ~3d: ~a" 270 (* 180 (/ pi) (bearing s4 s3)))))

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
  (let ((heading (+ pi (bearing self friend))))
    #'(lambda ()
        (+ heading (random .3) -0.15))))

(defclass environment ()
  ((actors
    :initarg :actors
    :accessor actors
    :initform nil)
   (time-step
    :initarg :time-step
    :accessor time-step
    :initform (error "must specify time-step"))))

(defclass bounded-environment (environment)
  ((x-min
    :initarg :x-min
    :accessor x-min
    :initform (error "must specify boundaries"))
   (x-max
    :initarg :x-max
    :accessor x-max
    :initform (error "must specify boundaries"))
   (y-min
    :initarg :y-min
    :accessor y-min
    :initform (error "must specify boundaries"))
   (y-max
    :initarg :y-max
    :accessor y-max
    :initform (error "must specify boundaries"))))

(defmethod update ((environment environment))
  (dolist (actor (actors environment)) (update actor)))

(defmethod add-actor ((environment environment) (actor actor))
  (setf (environment actor) environment)
  (setf (actors environment) (cons actor (actors environment))))

(defun make-medium-herd-world ()
  (let ((world (make-instance 'bounded-environment :time-step +time-step+
                              :x-min 0 :y-min 0 :x-max 700 :y-max 500)))
    (dotimes (i 20)
      (let ((x (+ 50 (random 600)))
            (y (+ 50 (random 400))))
        (add-actor world (make-instance 'sheep :x x :y y))))
    world))

(defparameter *medium-herd-world* (make-medium-herd-world))

(defun make-two-crowded-sheep-world ()
  (let ((world (make-instance 'bounded-environment :time-step +time-step+
                              :x-min 0 :y-min 0 :x-max 700 :y-max 500)))
    (add-actor world (make-instance 'sheep :x 300 :y 300))
    (add-actor world (make-instance 'sheep :x 305 :y 300))
    world))

(defparameter *two-crowded-sheep-world* (make-two-crowded-sheep-world))

(defun make-nucleated-sheep-world ()
  (let ((world (make-instance 'bounded-environment :time-step +time-step+
                              :x-min 0 :y-min 0 :x-max 700 :y-max 500)))
    (dotimes (i 8) (add-actor world (make-instance 'sheep :x (+ i 200) :y 150)))
    (dotimes (i 8) (add-actor world (make-instance 'sheep :x (+ i 350) :y 350)))
    (dotimes (i 8) (add-actor world (make-instance 'sheep :x (+ i 500) :y 150)))
    world))

(defparameter *nucleated-sheep-world* (make-nucleated-sheep-world))

;; (defclass environment-window (gtk-window)
;;   ((runningp
;;     :initarg :runningp
;;     :accessor runningp
;;     :initform t)
;;    (

(defun main ()
  (within-main-loop
   (let ((runningp t)
         ;(environment *medium-herd-world*)
         ;(environment *two-crowded-sheep-world*)
         (environment *nucleated-sheep-world*)
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
                (update environment)
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
                             (cairo-set-source-rgb cr 0.1 0.7 0.0)
                             (cairo-paint cr)
                             (dolist (actor (actors environment))
                               (draw cr actor))
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
