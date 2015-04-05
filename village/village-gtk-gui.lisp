(in-package :village)

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
                      (width (gtk-widget-get-allocated-width canvas))
                      (height (gtk-widget-get-allocated-height canvas))
                      (x-min (- x (/ (* .5 width) zoom)))
                      (x-max (+ x (/ (* .5 width) zoom)))
                      (y-min (- y (/ (* .5 height) zoom)))
                      (y-max (+ y (/ (* .5 height) zoom))))
                 (gtk-label-set-text x-min-label (format nil "~1$" x-min))
                 (gtk-label-set-text x-max-label (format nil "~1$" x-max))
                 (gtk-label-set-text y-min-label (format nil "~1$" y-min))
                 (gtk-label-set-text y-max-label (format nil "~1$" y-max))
                 (gtk-label-set-text zoom-label
                                     (format nil "Zoom: ~1$%" (* 100.0 zoom)))
                 (cairo-translate cr 0 (+ height))
                 (cairo-translate cr (* .5 width) (* -.5 height))
                 (cairo-scale cr (zoom viewer) (- zoom))
                 (cairo-translate cr (- x) (- y))
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
    (let* ((grid (gtk-grid-new))
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
               (map-up () (incf (y viewer) (/ 50 (zoom viewer))))
               (map-down () (decf (y viewer) (/ 50 (zoom viewer))))
               (map-right () (incf (x viewer) (/ 50 (zoom viewer))))
               (map-left () (decf (x viewer) (/ 50 (zoom viewer))))
               (zoom-in () (setf (zoom viewer) (* 4/3 (zoom viewer))))
               (zoom-out () (setf (zoom viewer) (* 3/4 (zoom viewer))))
               (pause-resume (&optional widget)
                 (declare (ignore widget))
                 (setf runningp (not runningp))
                 (when runningp (start-animation)))
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
        (g-signal-connect start-stop-button "clicked" #'pause-resume)
        (g-signal-connect step-button "clicked"
                          (lambda (widget)
                            (declare (ignore widget))
                            (update-all)))
        (g-signal-connect window "key-press-event"
                          (lambda (widget event)
                            (declare (ignore widget))
                            (let ((code (gdk-event-key-keyval event)))
                              (setf cl-user::examine (cons code cl-user::examine))
                              (case code
                                (65362 (map-up))
                                (65364 (map-down))
                                (65363 (map-right))
                                (65361 (map-left))
                                (119 (zoom-in))
                                (115 (zoom-out))
                                (32 (pause-resume))
                                ))
                            (gtk-widget-queue-draw viewer)
                            t))
        (g-signal-connect window "destroy" (lambda (widget)
                                             (declare (ignore widget))
                                             (setf runningp nil)
                                             (leave-gtk-main)))))))

(defun environment-control-new (environment)
  (make-instance 'environment-control :environment environment
                 :width-request 1360
                 :height-request 700))
