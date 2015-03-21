(asdf:load-system :cl-cffi-gtk)

(defpackage :gtk-tutorial
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :gtk-tutorial)

(defun example-simple-window ()
  (within-main-loop
    (let ((main (make-instance 'gtk-window
                               :type :toplevel
                               :title "Hello Buttons"
                               :border-width 12
                               :default-width 250
                               :default-height 75))
          (box (make-instance 'gtk-box
                              :orientation :horizontal
                              :spacing 6))
          (button (make-instance 'gtk-button
                                 :label "Button 1")))
      (g-signal-connect main "destroy" (lambda (widget)
                                         (declare (ignore widget))
                                         (leave-gtk-main)))
      (g-signal-connect button "clicked"
                        (lambda (widget) (declare (ignore widget))
                                (format t "~&Button 1 was pressed.~%")))
      (g-signal-connect main "delete-event"
                        (lambda (widget event) (declare (ignore widget event))
                                (format t "~&Delete event occurred.~%")
                                +gdk-event-propagate+))
      (gtk-container-add main button)
      (gtk-widget-show-all main))))


(let ((surface nil))
  (defun example-drawing ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :type :toplevel
                                   :title "Example Drawing"))
            (frame (make-instance 'gtk-frame
                                  :shadow-type :in))
            (area (make-instance 'gtk-drawing-area
                                 :width-request 250
                                 :height-request 200)))
        (g-signal-connect window "destroy" (lambda (widget)
                                             (declare (ignore widget))
                                             (leave-gtk-main)))
        (g-signal-connect area "draw"
                          (lambda (widget cr)
                            (declare (ignore widget))
                            (let ((cr (pointer cr)))
                              (cairo-set-source-surface cr surface 0.0 0.0)
                              (cairo-paint cr)
                              (cairo-destroy cr)
                              +gdk-event-propagate+)))
        (g-signal-connect area "configure-event"
                          (lambda (widget event)
                            (declare (ignore event))
                            (when surface
                              (cairo-surface-destroy surface))
                            (setf surface
                                  (gdk-window-create-similar-surface
                                   (gtk-widget-window widget)
                                   :color
                                   (gtk-widget-get-allocated-width widget)
                                   (gtk-widget-get-allocated-height widget)))
                            ;; Clear surface
                            (let ((cr (cairo-create surface)))
                              (cairo-set-source-rgb cr 1.0 1.0 1.0)
                              (cairo-paint cr)
                              (cairo-destroy cr))
                            (format t "leave event 'configure-event'~%")
                            +gdk-event-stop+))
        ;; Event signals
        (g-signal-connect area "motion-notify-event"
                          (lambda (widget event)
                            (format t "MOTION-NOTIFY-EVENT ~A~%" event)
                            (when (member :button1-mask (gdk-event-motion-state event))
                              (let ((cr (cairo-create surface))
                                    (x (gdk-event-motion-x event))
                                    (y (gdk-event-motion-y event)))
                                (cairo-rectangle cr (- x 3.0) (- y 3.0) 6.0 6.0)
                                (cairo-fill cr)
                                (cairo-destroy cr)
                                (gtk-widget-queue-draw-area widget
                                                            (truncate (- x 3.0))
                                                            (truncate (- y 3.0))
                                                            6
                                                            6)))
                            ;; We have handled the event, stop processing
                            +gdk-event-stop+))
        (g-signal-connect area "button-press-event"
                          (lambda (widget event)
                            (format t "BUTTON-PRESS-EVENT ~A~%" event)
                            (if (eql 1 (gdk-event-button-button event))
                                (let ((cr (cairo-create surface))
                                      (x (gdk-event-button-x event))
                                      (y (gdk-event-button-y event)))
                                  (cairo-rectangle cr (- x 3.0) (- y 3.0) 6.0 6.0)
                                  (cairo-fill cr)
                                  (cairo-destroy cr)
                                  (gtk-widget-queue-draw-area widget
                                                              (truncate (- x 3.0))
                                                              (truncate (- y 3.0))
                                                              6
                                                              6))
                                ;; Clear surface
                                (let ((cr (cairo-create surface)))
                                  (cairo-set-source-rgb cr 1.0 1.0 1.0)
                                  (cairo-paint cr)
                                  (cairo-destroy cr)
                                  (gtk-widget-queue-draw widget)))))
        (gtk-widget-add-events area
                               '(:button-press-mask
                                 :pointer-motion-mask))
        (gtk-container-add frame area)
        (gtk-container-add window frame)
        (gtk-widget-show-all window)))))
