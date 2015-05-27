(defpackage :genetic
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp
        :closer-mop
        :lisp-unit :cxml :flexi-streams))

(in-package :genetic)

(remove-tests :all)

(defparameter *character-candidates*
  (coerce "abcdefghijklmnopqrstuvwxyz" 'list))
(defparameter *target* "hello world")

(defgeneric size (object))
(defgeneric best-score (population))
(defgeneric worst-score (population))
(defgeneric mean-score (population))
(defgeneric evolve (population))
(defgeneric get-parent (population))
(defgeneric mate (parent-1 parent-2))
(defgeneric possibly-mutate (chromosome chance))
(defgeneric fitness (chromosome target))

(defclass chromosome ()
  ((internal-string
    :initarg :internal-string
    :accessor internal-string
    :initform (error "must provide string"))))

(defclass population ()
  ((populace
    :initarg :populace
    :accessor populace
    :initform (error "must provide initial population"))
   (generation
    :initarg :generation
    :accessor generation
    :initform 1)
   (target
    :initarg :target
    :accessor target
    :initform (error "must provide initial target"))))

(defun random-element (list)
  (elt list (random (length list))))

(defun random-string (&key (candidates *character-candidates*)
                        (minimum-length 12) (maximum-length 12))
  (let (characters)
    (dotimes (_ (+ minimum-length (random (1+ (- maximum-length minimum-length)))))
      (push (random-element candidates) characters))
    (coerce characters 'string)))

(defun make-population (size &key (target *target*))
  (let (populace)
    (dotimes (_ size)
      (push (make-instance 'chromosome :internal-string (random-string))
            populace))
    (make-instance 'population :populace populace :target target)))

(defmethod size ((population population))
  (length (populace population)))

(defmethod fitness ((chromosome chromosome) (target string))
  (fitness (internal-string chromosome) target))

(defmethod fitness ((input string) (target string))
  (let ((score 0))
    (dotimes (index (length target) score)
      (decf score (abs (- (char-code (elt target index))
                          (char-code (elt input index))))))))

(define-test fitness
  (assert-equal 0 (fitness "hello" "hello"))
  (assert-equal -19 (fitness "hxllo" "hello"))
  (assert-equal -12 (fitness "helxo" "hello"))
  (assert-equal -31 (fitness "hxlxo" "hello")))

(defmethod best-score ((population population))
  (apply #'max (mapcar (lambda (x) (fitness x (target population)))
                       (populace population))))

(defmethod worst-score ((population population))
  (apply #'min (mapcar (lambda (x) (fitness x (target population)))
                       (populace population))))

(defmethod mean-score ((population population))
  (/ (reduce #'+ (mapcar (lambda (x) (fitness x (target population)))
                         (populace population)))
     (size population)))

(defparameter *sample-pop* (make-population 400))

(defmethod print-object ((object population) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":gen ~d :best ~d :worst ~d :mean ~1$" (generation object)
            (best-score object) (worst-score object) (mean-score object))))

(defmethod print-object ((object population) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":gen~C~d~C:best~C~d~C:worst~C~d~C:mean~C~1$~C" #\tab (generation object) #\tab #\tab 
            (best-score object) #\tab #\tab (worst-score object) #\tab #\tab (mean-score object) #\tab)))

(defmethod print-object ((object chromosome) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s score:~d" (internal-string object) (fitness object *target*))))

(defmethod get-parent ((population population))
  (with-slots (populace target) population
    (car (sort (list (random-element populace)
                     (random-element populace)
                     (random-element populace))
               #'> :key (lambda (x) (fitness x target))))))

(defmethod mate ((parent-1 chromosome) (parent-2 chromosome))
  (let ((string-1 (internal-string parent-1))
        (string-2 (internal-string parent-2))
        (crossover (random 12))) ; TODO: fix hard coded
    (possibly-mutate (make-instance 'chromosome
                                    :internal-string
                                    (concatenate 'string
                                                 (subseq string-1 0 crossover)
                                                 (subseq string-2 crossover)))
                     .1)))                 

(defmethod possibly-mutate ((chromosome chromosome) chance)
  (let (new-characters)
    (dolist (character (coerce (internal-string chromosome) 'list))
      (push (code-char (+ (char-code character) (if (< (random 1.0) chance)
                                                    (- (random 10) 5)
                                                    0)))
            new-characters))
    (make-instance 'chromosome :internal-string
                   (coerce (nreverse new-characters) 'string))))

(defmethod evolve ((population population))
  (let ((new-populace))
    (dotimes (_ (size population))
      (let ((parent-1 (get-parent population))
            (parent-2 (get-parent population)))
        (push (mate parent-1 parent-2) new-populace)))
    (setf (populace population) new-populace)
    (incf (generation population)))
  population)

(let ((population (make-population 500)))
  (dotimes (_ 100)
    (format t "~&~a" population)
    (evolve population)))

(run-tests :all)
