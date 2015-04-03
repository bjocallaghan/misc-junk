(in-package :village)

(defgeneric add-actor (environment actor))
(defgeneric draw (cr object))
(defgeneric draw-label (cr actor))
(defgeneric draw-heading-indicator (cr object))
(defgeneric herd (sheep))

(defclass actor (wired-object)
  ((x
    :initarg :x
    :accessor x
    :initform (error "must supply x coordinate"))
   (y
    :initarg :y
    :accessor y
    :initform (error "must supply y coordinate"))
   (name
    :initarg :name
    :accessor name
    :initform 'unnamed)
   (environment
    :initarg :environment
    :accessor environment
    :initform nil)))

(defclass mover (actor)
  ((direction-fn
    :initarg :direction-fn
    :accessor direction-fn
    :initform (make-meander))
   (motivation
    :initarg :motivation
    :accessor motivation
    :initform 1.0)
   (heading
    :accessor heading
    :initform (random-heading))
   (max-speed
    :initarg :max-speed
    :accessor max-speed
    :initform 50)
   (name
    :initform 'mover)
   (status
    :initform 'meandering)))

(defclass sheep (mover)
  ((name
    :initform 'sheep)
   (size
    :initarg :size
    :accessor size
    :initform (+ 5 (random 2)))
   (status
    :initarg :status
    :accessor status
    :initform :content)))

(defclass baby-sheep (sheep)
  ((name
    :initform 'baby-sheep)
   (size
    :initform 3)
   (parent
    :initarg :parent
    :accessor parent
    :initform (error "must supply parent"))))

(defmethod herd ((sheep sheep))
  (actors (environment sheep)))

(defmethod update ((mover mover))
  (when (< (random 1.0) (motivation mover))
    (let ((heading (funcall (direction-fn mover)))
          (speed (max-speed mover)))
      (setf (heading mover) heading)
      (incf (x mover) (* speed (time-step (environment mover)) (cos heading)))
      (incf (y mover) (* speed (time-step (environment mover)) (sin heading))))))

(defmethod update ((sheep sheep))
  (let* ((others (remove-if (lambda (x) (eq x sheep)) (herd sheep)))
         (nearest (car (sort others #'< :key (lambda (x) (distance x sheep)))))
         (dist (distance nearest sheep))
         (new-status (cond
                           ((> dist 200) :isolated)
                           ((> dist 60) :lonely)
                           ((< dist 20) :crowded)
                           (t :content))))
    (unless (and (eq new-status (status sheep))
                 (or (eq (status sheep) :content)
                     (eq (status sheep) :isolated)))
      (setf (status sheep) new-status)
      (ecase (status sheep)
        (:isolated
         (progn
           (setf (motivation sheep) .2)
           (setf (direction-fn sheep) (make-meander (heading sheep)))
           ))
        (:lonely
         (progn
           (setf (motivation sheep) .4)
           (setf (direction-fn sheep) (make-seek sheep nearest))
           ))
        (:crowded
         (progn
           (setf (motivation sheep) .5)
           (setf (direction-fn sheep) (make-avoid sheep nearest))
           ))
        (:content
         (progn
           (setf (motivation sheep) .1)
           (setf (direction-fn sheep) (make-meander (heading sheep)))
           ))))    
    (setf (motivation sheep) (cond
                               ((eq (status sheep) :isolated) .2)
                               ((eq (status sheep) :lonely) .4)
                               ((eq (status sheep) :crowded) .3)
                               ((eq (status sheep) :content) .05))))
  (call-next-method))

(defmethod draw (cr (actor actor))
  (cairo-set-source-color cr deep-green)
  (cairo-circle cr (x actor) (y actor) 5)
  (cairo-fill cr)
  (cairo-set-source-color cr black)
  (cairo-set-line-width cr 1)
  (cairo-circle cr (x actor) (y actor) 5)
  (cairo-stroke cr)
  (draw-label cr actor))

(defmethod draw (cr (mover mover))
  (draw-heading-indicator cr mover)
  (call-next-method))

(defmethod draw (cr (sheep sheep))
  (cairo-save cr)
  (with-slots (x y heading status size) sheep
    (let ((color (ecase status
                   (:content white)
                   (:crowded red)
                   (:lonely light-blue)
                   (:isolated dark-blue))))
      (cairo-translate cr x y)
      (let ((head-size (* .5 size))
            (head-location (* .9 size)))

        ;(draw-heading-indicator cr sheep)
        ;(draw-label cr sheep)

        ;;; draw body
        (cairo-rotate cr heading)
        (cairo-scale cr 1.25 1)
        ;; body-outline
        (cairo-set-source-color cr black)
        (cairo-circle cr 0 0 (1+ size))
        (cairo-fill cr)
        ;; body
        (cairo-set-source-color cr color)
        (cairo-circle cr 0 0 size)
        (cairo-fill cr)
        ;; head
        (cairo-set-source-color cr black)
        (cairo-circle cr head-location 0 head-size)
        (cairo-fill cr))))
  (cairo-restore cr))

(defmethod draw-heading-indicator (cr (mover mover))
  (cairo-set-source-color cr purple)
  (cairo-set-line-width cr 3.0)
  (with-slots (heading) mover
    (let ((length 12))
      (cairo-move-to cr 0 0)
      (cairo-line-to cr (* length (cos heading)) (* length (sin heading)))
      (cairo-stroke cr))))

(defmethod draw-label (cr actor)
  (draw-text cr 0 0 (format nil "~a" actor)))

(defmethod draw-label (cr (mover sheep))
  (draw-text cr 0 0 (format nil "~a: ~a" (name mover) (status mover))))

(defun draw-text (cr x y text)
  (cairo-save cr)
  (cairo-scale cr 1 -1)
  (cairo-move-to cr (+ 5 x) (+ 5 y))
  (cairo-select-font-face cr "serif" 0 1)
  (cairo-set-font-size cr 10.0)
  (cairo-show-text cr text)
  (cairo-restore cr))

(declaim (inline distance))
(defun distance (one two)
  (sqrt (+ (expt (- (x one) (x two)) 2)
           (expt (- (y one) (y two)) 2))))

(declaim (inline bearing))
(defun bearing (self other)
  (atan (- (y other) (y self)) (- (x other) (x self))))

(defun random-heading ()
  (random (* 2 pi)))

(defun make-meander (&optional heading)
  (let ((heading (if (null heading) (random-heading) heading)))
    #'(lambda ()
        (when (zerop (random 20)) (setf heading (random-heading)))
        (+ heading (random .3) -0.15))))

(defun make-seek (self friend)
  (let ((heading (bearing self friend)))
    #'(lambda ()
        (+ heading (random .3) -0.15))))

(defun make-avoid (self friend)
  (let ((heading (+ pi (bearing self friend))))
    #'(lambda ()
        (+ heading (random .3) -0.15))))

(defclass environment ()
  ((actors
    :initarg :actors
    :accessor actors
    :initform nil)
   (runtime
    :accessor runtime
    :initform 0.0)
   (time-step
    :initarg :time-step
    :accessor time-step
    :initform (error "must specify time-step"))))

(defclass bounded-environment (environment)
  ((x-min
    :initarg :x-min
    :accessor x-min
    :initform (error "must specify boundaries"))
   (x-max
    :initarg :x-max
    :accessor x-max
    :initform (error "must specify boundaries"))
   (y-min
    :initarg :y-min
    :accessor y-min
    :initform (error "must specify boundaries"))
   (y-max
    :initarg :y-max
    :accessor y-max
    :initform (error "must specify boundaries"))))

(defmethod update ((environment environment))
  (incf (runtime environment) (time-step environment))
  (dolist (actor (actors environment)) (update actor)))

(defmethod add-actor ((environment environment) (actor actor))
  (setf (environment actor) environment)
  (setf (actors environment) (cons actor (actors environment))))
