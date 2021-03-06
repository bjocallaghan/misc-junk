(defparameter +drawing-area-width+ 500)
(defparameter +drawing-area-height+ 400)

(defparameter drawing-area (make-instance 'gtk-drawing-area
                                           :width-request +drawing-area-width+
                                           :height-request +drawing-area-height+))

(defun main ()
  (within-main-loop
   (let ((runningp nil)
         ;(environment *medium-herd-world*)
         ;(environment *two-crowded-sheep-world*)
          (environment *nucleated-sheep-world*)
         (window (make-instance 'gtk-window
                                :type :toplevel
                                :title "Village"))
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
                             (cairo-scale cr 1 -1)
                             (cairo-translate cr 0 (- (gtk-widget-get-allocated-height drawing-area)))
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
       (let ((s1 (car (actors environment))))
         (let ((box (make-instance 'sheep-details-box :orientation :vertical :sheep s1)))
           (connect s1 box)
           (gtk-box-pack-start ctrl-panel box :padding 4))
         (gtk-box-pack-start ctrl-panel (sheep-details-box-new s1)))
       (gtk-widget-show-all window)
       (start-animation)))))

(defun main3 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window))
          (sub-window (make-instance 'gtk-window))
          (box (gtk-box-new :vertical 3))
          (sub-box (gtk-box-new :vertical 3))
          (dummy1 (gtk-label-new "dummy1"))
          (dummy2 (gtk-label-new "dummy2"))
          (dummy3 (gtk-label-new "dummy3"))
          (dummy4 (gtk-label-new "dummy4"))
          (dummy5 (gtk-label-new "dummy5"))
          (dummy6 (gtk-label-new "dummy6")))

      (g-signal-connect window "destroy" (lambda (widget)
                                           (declare (ignore widget))
                                           (leave-gtk-main)))

      (gtk-container-add sub-window sub-box)
      (gtk-box-pack-start sub-box dummy4)
      (gtk-box-pack-start sub-box dummy5)
      (gtk-box-pack-start sub-box dummy6)

      (gtk-container-add window box)
      (gtk-box-pack-start box dummy1)
      (gtk-box-pack-start box sub-window)
      (gtk-box-pack-start box dummy2)
      (gtk-box-pack-start box dummy3)
      
      (gtk-widget-show-all sub-window)
      (gtk-widget-show-all window))))

(defun grid-test-main ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window))
          (grid (gtk-grid-new))
          (dummy1 (gtk-button-new-with-label "dummy1"))
          (dummy2 (gtk-button-new-with-label "dummy2"))
          (dummy3 (gtk-button-new-with-label "dummy3"))
          (dummy4 (gtk-button-new-with-label "dummy4"))
          (dummy5 (gtk-button-new-with-label "dummy5"))
          (dummy6 (gtk-button-new-with-label "dummy6")))
      (g-signal-connect window "destroy" (lambda (widget)
                                           (declare (ignore widget))
                                           (leave-gtk-main)))

      (gtk-grid-attach grid dummy1 0 0 1 1)
      (gtk-grid-attach grid dummy2 0 1 1 1)
      (gtk-grid-attach grid dummy3 0 2 1 1)
      (gtk-grid-attach grid dummy4 1 2 1 1)
      (gtk-grid-attach grid dummy5 3 2 1 1)
      (gtk-grid-attach grid dummy6 1 0 2 2)

      (gtk-container-add window grid)

      (gtk-widget-show-all window))))

(defun bearing-test ()
  "I wasn't confident bearing was working right. Needed to do some checking. Not
using :rt because I don't have that package and wasn't connected to the Internet
at the time."
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

(defparameter s5 (car (actors *medium-herd-world*)))

(defparameter *d-box* (gtk-label-new "hello"))
(defun d-msg (message)
  (gtk-label-set-text *d-box* (format nil "~a" message)))

(defun main2 (environment)
  (within-main-loop
    (let ((window (make-instance 'gtk-window))
          (box (gtk-box-new :vertical 0))
          (viewer (environment-viewer-new environment 4)))
      (g-signal-connect window "key-press-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (d-msg (format nil "a key was pressed at the window level"))
                          ;(setf thing event)
                          nil))
      (g-signal-connect window "destroy" (lambda (widget)
                                           (declare (ignore widget))
                                           (leave-gtk-main)))
      (gtk-container-add window box)
      (gtk-box-pack-start box viewer)
      (gtk-box-pack-start box *d-box*)
      (gtk-widget-show-all window)
      )))
