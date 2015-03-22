(defpackage :image-demo
  (:nicknames :id)
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :image-demo)

(defun swap-pictures (window image-1 image-2)
  (if (member image-1 (gtk-container-get-children window))
      (progn
        (gtk-container-remove window image-1)
        (gtk-container-add window image-2))
      (progn
        (gtk-container-remove window image-2)
        (gtk-container-add window image-1)))
  (gtk-widget-show-all window))

(defun zimage-demo ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "image-thing"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400))
          (image-1 (gtk-image-new-from-file "/home/brian/solved.dot.png"))
          (image-2 (gtk-image-new-from-file "/home/brian/blank.dot.png")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect window "key-press-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (format t "~& hello")
                          (swap-pictures window image-1 image-2)
                          +gdk-event-stop+))
      (gtk-widget-add-events window '(:key-press-mask))
      (gtk-container-add window image-1)
      (gtk-widget-show-all window))))
