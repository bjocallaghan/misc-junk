(in-package :village)

(defparameter *parent-lines-visible-p* nil)
(defparameter *sheep-labels-visible-p* t)
(defparameter *heading-indicators-visible-p* nil)
(defparameter *color-sheep-by-status-p* t)

(defgeneric add-actor (environment actor))
(defgeneric draw (cr object))
(defgeneric draw-label (cr actor))
(defgeneric draw-heading-indicator (cr object))
(defgeneric herd (sheep))
(defgeneric expiredp (behavior current-time))
(defgeneric evaluate-situation (mover))
(defgeneric collidedp (object-1 object-2))
(defgeneric detect-collisions (environment))

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
  ((behavior
    :accessor behavior
    :initform nil)
   (heading
    :accessor heading
    :initform (random-heading))
   (max-speed
    :initarg :max-speed
    :accessor max-speed
    :initform (error "must supply a max speed"))
   (size
    :initarg :size
    :accessor size
    :initform 5)
   (alertedp
    :accessor alertedp
    :initform nil)))

(defclass sheep (mover)
  ((name
    :initform 'sheep)
   (max-speed
    :initform 25)
   (size
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
  (remove-if-not (lambda (x) (eq 'sheep (type-of x)))
                 (actors (environment sheep))))

(defmethod update ((mover mover))
  (with-slots (x y max-speed environment heading behavior) mover
    (when (< (random 1.0) (motivation behavior))
      (let ((new-heading (funcall (direction-fn behavior)))
            (time-step (time-step environment)))
        (setf heading new-heading)
        (incf x (* max-speed time-step (cos new-heading)))
        (incf y (* max-speed time-step (sin new-heading)))))))

(defclass behavior ()
  ((name
    :initarg :name
    :accessor name
    :initform (error "must supply a name for this behavior"))
   (direction-fn
    :initarg :direction-fn
    :accessor direction-fn
    :initform (error "must supply a movement function"))
   (motivation
    :initarg :motivation
    :accessor motivation
    :initform 1.0)
   (expiration
    :initarg :expiration
    :accessor expiration
    :initform (error "must supply expiration"))))

(defun behavior-new (name motivation expiration direction-fn)
  (make-instance 'behavior 
                 :name name
                 :motivation motivation
                 :expiration expiration
                 :direction-fn direction-fn))

(defmethod expiredp ((behavior behavior) current-time)
  (>= current-time (expiration behavior)))

(defmethod evaluate-situation ((sheep sheep))
  (let* ((others (remove-if (lambda (x) (eq x sheep)) (herd sheep)))
         (nearest (car (sort others #'< :key (lambda (x) (distance x sheep)))))
         (distance-to-nearest (distance nearest sheep))
         (current-time (runtime (environment sheep))))
    (with-slots (status behavior heading) sheep
      (setf status (cond
                     ((> distance-to-nearest 75) :isolated)
                     ((> distance-to-nearest 60) :lonely)
                     ((< distance-to-nearest 20) :crowded)
                     (t :content)))
      (setf behavior
            (case status
              (:content (behavior-new "meandering" .15 (+ 8 current-time)
                                      (make-meander-fn heading)))
              (:isolated (behavior-new "lost" .25 (+ 1 current-time)
                                      (make-meander-fn heading)))
              (:lonely (behavior-new "rejoining flock" .3 (+ 2 current-time)
                                      (make-seek-fn sheep nearest)))
              (:crowded (behavior-new "moving away" .4 (+ .5 current-time)
                                      (make-avoid-fn sheep nearest))))))))

(defmethod evaluate-situation ((baby baby-sheep))
  (with-slots (status behavior heading environment parent) baby
    (let ((distance-to-parent (distance parent baby))
          (current-time (runtime environment)))
      (setf status (if (< distance-to-parent 30) :content :too-far))
      (setf behavior (if (eq status :content)
                         (behavior-new "meandering" .4 (+ 4 current-time)
                                      (make-meander-fn heading))
                         (behavior-new "returning" .6 (+ 1 current-time)
                                        (make-seek-fn baby parent)))))))

(defmethod update ((sheep sheep))
  (with-slots (environment behavior expiration alertedp) sheep
    (when (or (null behavior)
              alertedp
              (expiredp behavior (runtime environment)))
      (evaluate-situation sheep))
    (setf alertedp nil)
    (call-next-method)))

(defun make-meander-fn (&optional heading)
  (let ((heading (if (null heading) (random-heading) heading)))
    #'(lambda ()
        (when (zerop (random 20)) (setf heading (random-heading)))
        (+ heading (random .3) -0.15))))

(defun make-seek-fn (self friend)
  (let ((heading (bearing self friend)))
    #'(lambda ()
        (+ heading (random .3) -0.15))))

(defun make-avoid-fn (self friend)
  (let ((heading (+ pi (bearing self friend))))
    #'(lambda ()
        (+ heading (random .3) -0.15))))

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
    (let ((color (case status
                   (:crowded red)
                   (:alerted '(0.0 1.0 0.0))
                   (:lonely light-blue)
                   (:isolated dark-blue)
                   (otherwise white))))
      (unless *color-sheep-by-status-p* (setf color white))
      (cairo-translate cr x y)
      (let ((head-size (* .5 size))
            (head-location (* .9 size)))

        (when *heading-indicators-visible-p* (draw-heading-indicator cr sheep))
        (when *sheep-labels-visible-p* (draw-label cr sheep))

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

(defmethod draw (cr (baby baby-sheep))
  (when *parent-lines-visible-p*
    (with-slots (x y parent) baby
      (let ((parent-x (x parent))
            (parent-y (y parent)))
        (cairo-set-line-width cr 2)
        (cairo-set-source-color cr purple)
        (cairo-move-to cr x y)
        (cairo-line-to cr parent-x parent-y)
        (cairo-stroke cr))))
  (call-next-method))

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

(defmethod draw-label (cr (sheep sheep))
  (with-slots (name status behavior environment) sheep
    (draw-text cr 0 0 (format nil "~a: ~a (~a ~1$)"
                              name status (name behavior)
                              (- (expiration behavior) (runtime environment))))))

(defun draw-text (cr x y text)
  (cairo-save cr)
  (cairo-scale cr 1 -1)
  (cairo-move-to cr (+ 5 x) (+ 5 y))
  (cairo-select-font-face cr "serif" 0 1)
  (cairo-set-font-size cr 10.0)
  (cairo-set-source-color cr purple)
  (cairo-show-text cr text)
  (cairo-restore cr))

(declaim (inline distance bearing random-heading))
(defun distance (one two)
  (sqrt (+ (expt (- (x one) (x two)) 2)
           (expt (- (y one) (y two)) 2))))
(defun bearing (self other)
  (atan (- (y other) (y self)) (- (x other) (x self))))
(defun random-heading ()
  (random (* 2 pi)))

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
  (dolist (actor (actors environment)) (update actor))
  (detect-collisions environment))

(defmethod add-actor ((environment environment) (actor actor))
  (setf (environment actor) environment)
  (setf (actors environment) (cons actor (actors environment))))

;;; the math on these collision detection methods doesn't use object size, and
;;; thus isn't 100% accurate, but it's reasonably fast and a pretty good
;;; approximation (for now)

(defparameter *collision-distance* 8)

(defmethod collidedp ((actor-1 actor) (actor-2 actor))
  (< (distance actor-1 actor-2) *collision-distance*))

(defmethod detect-collisions ((environment environment))
  "Returns a list of objects which are currently in a collision state. Checks
collisions by walking down the list once (ordered by x coordinate), with each
element doing a small walk of subsequent elements until the
x-coordinate-distance exceeds the collision threshold. Still a lot of checks,
but fewer than N-choose-2."
  (labels ((check-collisions (primary list acc)
             (if (or (null list)
                     (> (- (x (car list)) (x primary)) *collision-distance*))
                 (when acc (cons primary acc))
                 (progn
                   (when (<= (distance (car list) primary) *collision-distance*)
                     (setf acc (cons (car list) acc))
                     (check-collisions primary (cdr list) acc)))))
           (walk-list (list)
             (when list
               (dolist (actor (check-collisions (car list) (cdr list) nil))
                 (setf (status actor) :alerted)
                 (setf (alertedp actor) t))
               (walk-list (cdr list)))))
    (walk-list (sort (copy-seq (actors environment))
                     #'< :key (lambda (a) (x a))))))
