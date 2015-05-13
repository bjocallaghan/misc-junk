(defpackage :society
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp
        :lisp-unit :closer-mop :cxml :flexi-streams))

(in-package :society)

(defclass person ()
  ((age
    :initarg :age
    :accessor age
    :initform (error "must specify age"))))

(defclass female (person) ())
(defclass male (person ()))

