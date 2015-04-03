(defpackage :village
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :village)

;;; convenience stuff i have defined. this seems like something that should be
;;; part of the api... am i missing it?
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

(defconstant +target-fps+ 30.0)
(defconstant +time-step+ (/ +target-fps+))

(defgeneric add-actor (environment actor))
(defgeneric update (object))
(defgeneric draw (cr object))
(defgeneric draw-label (cr actor))
(defgeneric draw-heading-indicator (cr object))
(defgeneric herd (sheep))

(defclass wired-object ()
  ((subscribers
    :accessor subscribers
    :initform nil)))

(defgeneric connect (sender destination))
(defgeneric disconnect (sender destination))

(defmethod connect ((sender wired-object) destination)
  (setf (subscribers sender)
        (cons destination (subscribers sender))))

(defmethod disconnect ((sender wired-object) destination)
  (setf (subscribers sender) (remove-if (lambda (x) (eq x destination))
                                        (subscribers sender))))

(defmethod update :after ((sender wired-object))
  (dolist (subscriber (subscribers sender))
    (update subscriber)))

(defclass actor (wired-object)
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
   (size
    :initarg :size
    :accessor size
    :initform (+ 5 (random 2)))
   (status
    :initarg :status
    :accessor status
    :initform :content)))

(defclass baby-sheep (sheep)
  ((name
    :initform 'baby-sheep)
   (size
    :initform 3)
   (parent
    :initarg :parent
    :accessor parent
    :initform (error "must supply parent"))))

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
                               ((eq (status sheep) :content) .05))))
  (call-next-method))

(defmethod draw (cr (actor actor))
  (cairo-set-source-color cr deep-green)
  (cairo-circle cr (x actor) (y actor) 5)
  (cairo-fill cr)
  (cairo-set-source-color cr black)
  (cairo-set-line-width cr 1)
  (cairo-circle cr (x actor) (y actor) 5)
  (cairo-stroke cr)
  (draw-label cr actor))

(defmethod draw (cr (mover mover))
  (draw-heading-indicator cr mover)
  (call-next-method))

(defmethod draw (cr (sheep sheep))
  (cairo-save cr)
  (with-slots (x y heading status size) sheep
    (let ((color (ecase status
                   (:content white)
                   (:crowded red)
                   (:lonely light-blue)
                   (:isolated dark-blue))))
      (cairo-translate cr x y)
      (let ((head-size (* .5 size))
            (head-location (* .9 size)))

        ;(draw-heading-indicator cr sheep)
        ;(draw-label cr sheep)

        ;;; draw body
        (cairo-rotate cr heading)
        (cairo-scale cr 1.25 1)
        ;; body-outline
        (cairo-set-source-color cr black)
        (cairo-circle cr 0 0 (1+ size))
        (cairo-fill cr)
        ;; body
        (cairo-set-source-color cr color)
        (cairo-circle cr 0 0 size)
        (cairo-fill cr)
        ;; head
        (cairo-set-source-color cr black)
        (cairo-circle cr head-location 0 head-size)
        (cairo-fill cr))))
  (cairo-restore cr))

(defmethod draw-heading-indicator (cr (mover mover))
  (cairo-set-source-color cr purple)
  (cairo-set-line-width cr 3.0)
  (with-slots (heading) mover
    (let ((length 12))
      (cairo-move-to cr 0 0)
      (cairo-line-to cr (* length (cos heading)) (* length (sin heading)))
      (cairo-stroke cr))))

(defmethod draw-label (cr actor)
  (draw-text cr 0 0 (format nil "~a" actor)))

(defmethod draw-label (cr (mover sheep))
  (draw-text cr 0 0 (format nil "~a: ~a" (name mover) (status mover))))

(defun draw-text (cr x y text)
  (cairo-save cr)
  (cairo-scale cr 1 -1)
  (cairo-move-to cr (+ 5 x) (+ 5 y))
  (cairo-select-font-face cr "serif" 0 1)
  (cairo-set-font-size cr 10.0)
  (cairo-show-text cr text)
  (cairo-restore cr))

(declaim (inline distance))
(defun distance (one two)
  (sqrt (+ (expt (- (x one) (x two)) 2)
           (expt (- (y one) (y two)) 2))))

(declaim (inline bearing))
(defun bearing (self other)
  (atan (- (y other) (y self)) (- (x other) (x self))))

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
   (runtime
    :accessor runtime
    :initform 0.0)
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
  (incf (runtime environment) (time-step environment))
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
                              :x-min 0 :y-min 0 :x-max 400 :y-max 400)))
    (dotimes (i 1) (add-actor world (make-instance 'sheep :x (+ i 4) :y 4)))
    (dotimes (i 8) (add-actor world (make-instance 'sheep :x (+ i 200) :y 150)))
    (dotimes (i 8) (add-actor world (make-instance 'sheep :x (+ i 350) :y 350)))
    (dotimes (i 8) (add-actor world (make-instance 'sheep :x (+ i 500) :y 150)))
    world))

(defparameter *nucleated-sheep-world* (make-nucleated-sheep-world))

(defclass sheep-details-box (gtk-box)
  ((initialized-p
    :accessor initialized-p
    :initform nil)
   (x-value
    :accessor x-value
    :initform (gtk-label-new "<not updated yet>"))
   (y-value
    :accessor y-value
    :initform (gtk-label-new "<not updated yet>"))
   (sheep
    :initarg :sheep
    :accessor sheep
    :initform (error "must have sheep")))
  (:metaclass gobject-class))

(defmethod initialize-instance :after ((box sheep-details-box)
                                       &rest initargs
                                       &key (column-types nil column-types-p)
                                         &allow-other-keys)
  (declare (ignore initargs column-types column-types-p))
  (let ((prop-box (gtk-box-new :horizontal 2)))
    (gtk-box-pack-start prop-box (gtk-label-new "Y:"))
    (gtk-box-pack-start prop-box (y-value box))
    (gtk-box-pack-end box prop-box :expand nil :fill nil))
  (let ((prop-box (gtk-box-new :horizontal 2)))
    (gtk-box-pack-start prop-box (gtk-label-new "X:"))
    (gtk-box-pack-start prop-box (x-value box))
    (gtk-box-pack-end box prop-box :expand nil :fill nil)))

(defmethod update ((box sheep-details-box))
  (gtk-label-set-text (x-value box) (format nil "~1$" (x (sheep box))))
  (gtk-label-set-text (y-value box) (format nil "~1$" (x (sheep box)))))

(defun sheep-details-box-new (sheep)
  "Returns a new instance of SHEEP-DETAILS-BOX using SHEEP."
  (let ((box (make-instance 'sheep-details-box
                 :sheep sheep
                 :orientation :vertical
                 :spacing 0)))
    (connect sheep box)
    box))

(defclass environment-viewer (gtk-grid)
  ((environment
    :initarg :environment
    :accessor environment
    :initform (error "must supply environment"))
   (canvas
    :accessor canvas
    :initform (make-instance 'gtk-drawing-area
;                             :width-request 300 :height-request 200
                             :hexpand t :halign :fill :vexpand t :valign :fill))
   (x
    :initarg :x
    :accessor x
    :initform 0)
   (y
    :initarg :y
    :accessor y
    :initform 0)
   (x-min-label
    :accessor x-min-label
    :initform (make-instance 'gtk-label :label "<unset>" :halign :start))
   (y-min-label
    :accessor y-min-label
    :initform (make-instance 'gtk-label :label "<unset>" :halign :end
                             :valign :end :width-request 80))
   (x-max-label
    :accessor x-max-label
    :initform (make-instance 'gtk-label :label "<unset>" :halign :end))
   (y-max-label
    :accessor y-max-label
    :initform (make-instance 'gtk-label :label "<unset>" :halign :end
                             :valign :start :width-request 80))
   (zoom-label
    :accessor zoom-label
    :initform (make-instance 'gtk-label :label "<unset>" :halign :center))
   (zoom
    :initarg :zoom
    :accessor zoom
    :initform 1))
  (:metaclass gobject-class))

(defmethod initialize-instance :after ((viewer environment-viewer)
                                       &rest initargs
                                       &key (column-types nil column-types-p)
                                         &allow-other-keys)
  (declare (ignore initargs column-types column-types-p))
  (with-slots (canvas x-min-label x-max-label y-min-label y-max-label zoom-label
                      x y zoom environment) viewer
    (gtk-grid-attach viewer canvas 1 0 3 2)
    (gtk-grid-attach viewer y-min-label 0 1 1 1)
    (gtk-grid-attach viewer y-max-label 0 0 1 1)
    (gtk-grid-attach viewer x-min-label 1 2 1 1)
    (gtk-grid-attach viewer zoom-label 2 2 1 1)
    (gtk-grid-attach viewer x-max-label 3 2 1 1)
    (labels ((redraw-all (widget cr)
               (declare (ignore widget))
               (let* ((cr (pointer cr))
                      (width (gtk-widget-get-allocated-width
                              canvas))
                      (height (gtk-widget-get-allocated-height canvas)))
                 (gtk-label-set-text x-min-label
                                     (format nil "~1$" (- x (/ (* .5 width)
                                                               zoom))))
                 (gtk-label-set-text x-max-label
                                     (format nil "~1$" (+ x (/ (* .5 width)
                                                               zoom))))
                 (gtk-label-set-text y-min-label
                                     (format nil "~1$" (- y (/ (* .5 height)
                                                               zoom))))
                 (gtk-label-set-text y-max-label
                                     (format nil "~1$" (+ y (/ (* .5 height)
                                                               zoom))))
                 (gtk-label-set-text zoom-label
                                     (format nil "Zoom: ~1$%" (* 100.0 zoom)))
                 (cairo-translate cr 0
                                  (+ height))
                 (cairo-translate cr
                                  (* .5 width) (* -.5 height))
                 (cairo-scale cr (zoom viewer) (- zoom))
                 (cairo-set-source-rgb cr 0.0 0.0 0.2)
                 (cairo-paint cr)
                 (cairo-set-source-rgb cr 0.1 0.7 0.0)
                 (cairo-rectangle cr
                                  0 0 (x-max environment) (y-max environment))
                 (cairo-fill cr)
                 (dolist (actor (actors environment)) (draw cr actor))
                 (cairo-destroy cr)
                 t)))
      (g-signal-connect canvas "draw" #'redraw-all))))

(defun environment-viewer-new (environment zoom)
  (make-instance 'environment-viewer
                 :environment environment
                 :zoom zoom))

(defclass environment-control (gtk-window)
  ((environment
    :initarg :environment
    :accessor environment
    :initform (error "must give environment"))
   (runningp
    :initarg :runningp
    :accessor runningp
    :initform t)
   (start-stop-button
    :accessor start-stop-button
    :initform (make-instance 'gtk-button :label "Start/Stop" :valign :start))
   (step-button
    :accessor step-button
    :initform (make-instance 'gtk-button :label "Step" :valign :start))
   (time-label
    :accessor time-label
    :initform (make-instance 'gtk-label :label "<unset>"
                             :valign :end :halign :start)))
  (:metaclass gobject-class))

(defmethod initialize-instance :after ((window environment-control)
                                       &rest initargs
                                       &key (column-types nil column-types-p)
                                         &allow-other-keys)
  (declare (ignore initargs column-types column-types-p))
  (with-slots (environment runningp start-stop-button step-button
                           time-label) window
    (let ((grid (gtk-grid-new))
          (viewer (environment-viewer-new environment 1))
          (animation-box (make-instance 'gtk-box 
                                        :orientation :horizontal
                                        :homogeneous t
                                        :width-request 200))
          (time-box (make-instance 'gtk-box 
                                   :orientation :horizontal
                                   :homogeneous nil
                                   :width-request 200)))
      (labels ((start-animation ()
                 (g-timeout-add (floor (* 1000 (time-step environment)))
                                #'update-all))
               (update-all (&optional widget)
                 (declare (ignore widget))
                 (update environment)
                 (gtk-widget-queue-draw viewer)
                 (gtk-label-set-text time-label
                                     (format nil "~2$" (runtime environment)))
                 runningp))
        (gtk-container-add window grid)
        (gtk-grid-attach grid viewer 0 0 1 2)
        (gtk-box-pack-start animation-box start-stop-button)
        (gtk-box-pack-start animation-box step-button)
        (gtk-grid-attach grid animation-box 1 0 1 1)
        (gtk-box-pack-end time-box time-label)
        (gtk-box-pack-end time-box (make-instance 'gtk-label 
                                                  :label "Time: "
                                                  :valign :end
                                                  :halign :end))
        (gtk-grid-attach grid time-box 1 1 1 1)
        (start-animation)
        (g-signal-connect start-stop-button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (setf runningp (not runningp))
                            (when runningp (start-animation))))
        (g-signal-connect step-button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (update-all)))
        (g-signal-connect window "destroy" (lambda (widget)
                                             (declare (ignore widget))
                                             (setf runningp nil)
                                             (leave-gtk-main)))))))

(defun environment-control-new (environment)
  (make-instance 'environment-control :environment environment))

(defun main6 ()
  (within-main-loop
    (let ((window (environment-control-new *nucleated-sheep-world*)))
       (gtk-widget-show-all window))))
