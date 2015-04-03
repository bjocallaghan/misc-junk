(in-package :village)

;;; convenience stuff i have defined. this seems like something that should be
;;; part of the api... am i missing it?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'black) (defconstant black '(0.0 0.0 0.0)))
  (unless (boundp 'white) (defconstant white '(1.0 1.0 1.0)))
  (unless (boundp 'red) (defconstant red '(1.0 0.0 0.0)))
  (unless (boundp 'light-blue) (defconstant light-blue '(0.6 0.6 1.0)))
  (unless (boundp 'dark-blue) (defconstant dark-blue '(0.0 0.0 1.0)))
  (unless (boundp 'deep-green) (defconstant deep-green '(0.8 0.4 0.2)))
  (unless (boundp 'purple) (defconstant purple '(1.0 0.0 1.0))))
(defmacro cairo-set-source-color (cr color)
  `(apply #'cairo-set-source-rgb (cons ,cr ,color)))
(defmacro cairo-circle (cr x-center y-center radius)
  `(cairo-arc ,cr ,x-center ,y-center ,radius 0 (* 2 pi)))
