(asdf:load-system :cl-cffi-gtk)

(defpackage :gtk-tutorial
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :gtk-tutorial)

(defun example-simple-window ()
  (within-main-loop
    (let ((main (make-instance 'gtk-window
                               :type :toplevel
                               :title "Getting Started..."
                               :border-width 12
                               :default-width 400))
          (button (make-instance 'gtk-button
                                 :label "Hello GTK World!")))
      (g-signal-connect main "destroy" (lambda (widget)
                                         (declare (ignore widget))
                                         (leave-gtk-main)))
      (g-signal-connect button "clicked"
                        (lambda (widget) (declare (ignore widget))
                                (format t "~&Hello world clicked.~%")
                                (gtk-widget-destroy main)))
      (g-signal-connect main "delete-event"
                        (lambda (widget event) (declare (ignore widget event))
                                (format t "~&Delete event occurred.~%")
                                +gdk-event-propagate+))
      (gtk-container-add main button)
      (gtk-widget-show-all main))))
