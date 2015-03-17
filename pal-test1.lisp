(use-package :pal)

(defun routine1 ()
  (with-pal (:title "Grid routine")
    (clear-screen (color 255 255 0))
    (wait-keypress)))
