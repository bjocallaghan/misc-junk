(defpackage :village
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp))

(in-package :village)

(defparameter cl-user::examine nil)

;;; poor man's system definition -- i'll need to revamp this later
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +target-fps+ 30.0)
  (defconstant +time-step+ (/ +target-fps+))

  (dolist (part-name '(
                       "wiring"
                       "cairo-module"
                       "sim-objects"
                       "village-gtk-gui"
                       "premade-worlds"
                       ))
    (let ((lisp-filename (make-pathname :defaults *load-pathname*
                                        :type "lisp" :name part-name))
          (fasl-filename (make-pathname :defaults *load-pathname*
                                        :type "fasl" :name part-name)))
      (format t "Compiling: ~a..." fasl-filename)
      (compile-file lisp-filename)
      (format t "Compiling complete: ~a." fasl-filename)
      (load fasl-filename))))

(defun main ()
  (within-main-loop
    (let ((window (environment-control-new *nucleated-sheep-world*)))
       (gtk-widget-show-all window))))
