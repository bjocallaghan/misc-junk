(asdf:load-system :cl-cffi-gtk)

(defpackage :gtk-tutorial
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :gtk-tutorial)

(defun make-box (homogeneous spacing expand fill padding)
  (format t "~2&i wonder where this message will go")
  (let ((giant-box (make-instance 'gtk-box
                                  :orientation :horizontal
                                  :spacing spacing
                                  :homogeneous homogeneous)))
    (dotimes (i 5)
      (let ((label (format nil "hello~d" (floor 100000000 (expt 10 (random 8))))))
        (gtk-box-pack-start giant-box (gtk-button-new-with-label label)
                            :expand expand
                            :fill fill
                            :padding padding)))
    giant-box))
    
(defun example-packing ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Box packing demo"
                                 :type :toplevel
                                 :border-width 12))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :spacing 6))
          (quit-button (make-instance 'gtk-button
                                      :label "quit"))
          (quit-box (make-instance 'gtk-box
                                  :orientation :horizontal)))
      (g-signal-connect quit-button "clicked" (lambda (widget)
                                                (declare (ignore widget))
                                                (gtk-widget-destroy window)))
      (g-signal-connect window "destroy" (lambda (widget)
                                           (declare (ignore widget))
                                           (leave-gtk-main)))
      ;; (gtk-box-pack-start vbox (make-box t 6 t t 6) :expand nil)
      ;; (gtk-box-pack-start vbox (make-box t 0 t t 6) :expand nil)
      ;; (gtk-box-pack-start vbox (make-box t 6 t t 6) :expand nil)
      ;; (gtk-box-pack-start vbox (make-box t 10 t t 0) :expand nil)
      ;; (gtk-box-pack-start vbox (make-box t 0 t t 50) :expand nil)
      (gtk-box-pack-start vbox (make-box nil 0 nil nil 10) :expand nil)
      (gtk-box-pack-start vbox (gtk-separator-new :horizontal)
                          :expand nil
                          :padding 6)
      (gtk-box-pack-end quit-box quit-button :expand nil)
      (gtk-box-pack-start vbox quit-box :expand nil)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

(defun simpler-packing ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Box packing demo"
                                 :type :toplevel
                                 :border-width 12))
          (box (make-instance 'gtk-box))
          (quit-button (make-instance 'gtk-button
                                      :label "quit")))
      (g-signal-connect quit-button "clicked" (lambda (widget)
                                                (declare (ignore widget))
                                                (gtk-widget-destroy window)))
      (g-signal-connect window "destroy" (lambda (widget)
                                           (declare (ignore widget))
                                           (leave-gtk-main)))
      ;(gtk-box-pack-start vbox (make-box) :expand nil)
      (gtk-box-pack-start box quit-button)
      (gtk-container-add window box)
      (gtk-widget-show-all window))))
