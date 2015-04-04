(in-package :village)

(defun make-medium-herd-world ()
  (let ((world (make-instance 'bounded-environment :time-step +time-step+
                              :x-min 0 :y-min 0 :x-max 700 :y-max 500)))
    (dotimes (i 20)
      (let ((x (+ 50 (random 600)))
            (y (+ 50 (random 400))))
        (add-actor world (make-instance 'sheep :x x :y y))))
    world))

(defparameter *medium-herd-world* (make-medium-herd-world))

(defun make-two-crowded-sheep-world ()
  (let ((world (make-instance 'bounded-environment :time-step +time-step+
                              :x-min 0 :y-min 0 :x-max 700 :y-max 500)))
    (add-actor world (make-instance 'sheep :x 300 :y 300))
    (add-actor world (make-instance 'sheep :x 305 :y 300))
    world))

(defparameter *two-crowded-sheep-world* (make-two-crowded-sheep-world))

(defun make-nucleated-sheep-world ()
  (let ((world (make-instance 'bounded-environment :time-step +time-step+
                              :x-min 0 :y-min 0 :x-max 400 :y-max 400)))
    (dotimes (i 1) (add-actor world (make-instance 'sheep :x (+ i 4) :y 4)))
    (dotimes (i 8) (add-actor world (make-instance 'sheep :x (+ i 200) :y 150)))
    (dotimes (i 8) (add-actor world (make-instance 'sheep :x (+ i 350) :y 350)))
    (dotimes (i 8) (add-actor world (make-instance 'sheep :x (+ i 500) :y 150)))
    (let ((mamas (mapcar (lambda (x) (nth x (actors world))) '(4 6 12 20 21))))
      (dolist (mama mamas)
        (with-slots (x y) mama
          (dotimes (i (1+ (random 2)))
            (add-actor world (make-instance 'baby-sheep
                                            :x x :y y :parent mama))))))
    world))

(defparameter *nucleated-sheep-world* (make-nucleated-sheep-world))
