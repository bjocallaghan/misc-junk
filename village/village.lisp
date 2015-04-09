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
                       "cairo-module"
                       "wiring"
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
    (let ((window (environment-control-new *medium-herd-world*)))
    ;(let ((window (environment-control-new *nucleated-herd-world*)))
       (gtk-widget-show-all window))))
