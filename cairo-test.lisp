(defpackage :cairo-demo
  (:nicknames :cd)
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :cairo-demo)

(defun zcairo-demo ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cairo demo pad"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect window "draw"
                        (lambda (widget cr)
                          (let ((cr (pointer cr))
                                (window (gtk-widget-window widget)))
                            (cairo-set-source-rgb cr 1.0 1.0 0.0)
                            (cairo-paint cr)
                            (cairo-scale cr
                                         (gdk-window-get-width window)
                                         (gdk-window-get-height window))
                            ;; drawing starts(?) here
                            (cairo-set-line-width cr 0.001)
                            (cairo-set-source-rgb cr 0.0 0.0 1.0)
                            (cairo-rectangle cr 0.25 0.5 0.5 0.25)
                            (cairo-stroke cr)
                            (cairo-destroy cr)
                            t)))
      (gtk-widget-show-all window))))
