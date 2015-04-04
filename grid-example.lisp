(defpackage :grid-example
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :grid-example)

(defun make-grid (homogeneous spacing expand align margin)
  (let ((box (make-instance 'gtk-grid
                            :orientation :horizontal
                            :column-homogeneous homogeneous
                            :column-spacing spacing)))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label "gtk-container-add"
                                      :hexpand expand
                                      :halgin align
                                      :margin margin))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label "box"
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label "button"
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label (if expand "T" "NIL")
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label (format nil "~A" align)
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    (gtk-container-add box
                       (make-instance 'gtk-button
                                      :label (format nil "~A" margin)
                                      :hexpand expand
                                      :halign align
                                      :margin margin))
    box))

(defun example-grid-packing (&optional (spacing 0))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Grid Packing"
                                 :type :toplevel
                                 :border-width 12
                                 :default-height 200
                                 :default-width 300))
          (vbox (make-instance 'gtk-grid
                               :orientation :vertical
                               :row-spacing 6))
          (button (make-instance 'gtk-button
                                 :label "Quit"))
          (quitbox (make-instance 'gtk-box
                                  :orientation :horizontal)))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add vbox
                         (make-instance 'gtk-label
                                        :label
                                        (format nil
                                         "GtkGrid homogeneous nil spacing ~A"
                                         spacing)
                                        :xalign 0
                                        :yalign 0
                                        :vexpand nil
                                        :valign :start))
      (gtk-container-add vbox (gtk-separator-new :horizontal))
      (gtk-container-add vbox (make-grid nil spacing nil :center 0))
      (gtk-container-add vbox (make-grid nil spacing t :center 0))
      (gtk-container-add vbox (make-grid nil spacing t :fill 0))
      (gtk-container-add vbox (gtk-separator-new :horizontal))
      (gtk-container-add vbox
                         (make-instance 'gtk-label
                                        :label
                                        (format nil
                                           "GtkGrid homogeneous t spacing ~A"
                                           spacing)
                                         :xalign 0
                                         :yalign 0
                                         :vexpand nil
                                         :valign :start
                                         :margin 6))
      (gtk-container-add vbox (gtk-separator-new :horizontal))
      (gtk-container-add vbox (make-grid t spacing t :center 0))
      (gtk-container-add vbox (make-grid t spacing t :fill 0))
      (gtk-container-add vbox (gtk-separator-new :horizontal))
      (gtk-container-add quitbox button)
      (gtk-container-add vbox quitbox)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))

(defun example-grid-packing-2 ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Grid Packing"
                                 :border-width 12
                                 :default-width 300))
          (grid (make-instance 'gtk-grid
                                :column-homogeneous t
                                :row-homogeneous t))
          (button1 (make-instance 'gtk-button
                                  :label "Button 1"))
          (button2 (make-instance 'gtk-button
                                  :label "Button 2"))
          (quit (make-instance 'gtk-button
                               :label "Quit")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect quit "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk-widget-destroy window)))
      (gtk-grid-attach grid button1 0 1 1 1)
      (gtk-grid-attach grid button2 1 1 1 1)
      (gtk-grid-attach grid quit    0 2 2 1)
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))

