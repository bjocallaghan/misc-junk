(defpackage :gtk-prime
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :gtk-prime)

(defun main ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window :title "Close this")))
      (gtk-container-add window (gtk-label-new "Close this when/if visible."))
      (gtk-widget-show-all window))))

(main)
