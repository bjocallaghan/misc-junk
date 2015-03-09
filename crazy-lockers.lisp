;;;; The Crazy Locker Game -- A Jeff Y. Problem

(defun change-state (lockers locker-address)
  "Flip the state of a locker in array LOCKERS at LOCKER-ADDRESS."
  (let ((current-state (aref lockers locker-address)))
    (setf (aref lockers locker-address)
          (if (eql current-state :closed) :opened :closed))))

(defun num-open (lockers)
  (reduce #'+ lockers :key #'(lambda (x) (if (eql x :opened) 1 0))))

(defun locker-experiment (num-lockers &optional verbose)
  "Returns number of open lockers after doing a crazy-principal locker
experiment with NUM-LOCKERS lockers."
  (let ((lockers (make-array num-lockers :initial-element :closed)))
    (dotimes (student-num num-lockers)
      (do ((locker-address student-num (+ locker-address (1+ student-num))))
          ((>= locker-address num-lockers))
        (change-state lockers locker-address))
      (when verbose
        (format t "after student ~2d ~2d open: ~a ~%"
                (1+ student-num)
                (num-open lockers)
                lockers)))
    (num-open lockers)))

;; CL-USER> (locker-experiment 6 t)
;; after student  1  6 open: #(OPENED OPENED OPENED OPENED OPENED OPENED) 
;; after student  2  3 open: #(OPENED CLOSED OPENED CLOSED OPENED CLOSED) 
;; after student  3  3 open: #(OPENED CLOSED CLOSED CLOSED OPENED OPENED) 
;; after student  4  4 open: #(OPENED CLOSED CLOSED OPENED OPENED OPENED) 
;; after student  5  3 open: #(OPENED CLOSED CLOSED OPENED CLOSED OPENED) 
;; after student  6  2 open: #(OPENED CLOSED CLOSED OPENED CLOSED CLOSED) 
;; 2
;; CL-USER> (loop for i from 1 to 30 collect (locker-experiment i))
;; (1 1 1 2 2 2 2 2 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5)
;; CL-USER> (locker-experiment 1000)
;; 31

(defun locker-experiment-simplified (num-lockers)
  "Returns number of open lockers after doing a crazy-principal locker
experiment with NUM-LOCKERS lockers."
  (let ((lockers (apply #'vector (loop repeat num-lockers collect nil))))
    (dotimes (student-num num-lockers)
      (do ((locker-address student-num (+ locker-address (1+ student-num))))
          ((>= locker-address num-lockers))
        (setf (elt lockers locker-address) (not (elt lockers locker-address)))))
    (reduce #'+ lockers :key #'(lambda (x) (if x 1 0)))))

;; CL-USER> (locker-experiment-simplified 1000)
;; 31

(defun locker-experiment-efficient (num-lockers)
  "Efficiently returns number of open lockers after doing a crazy-principal
locker experiment with NUM-LOCKERS lockers.

Found a pattern that I don't fully understand, but here it is translated into an
algorithm. [later edit: see locker-experiment-super-efficient]"
  (do ((i 1 (1+ i))
       (next-sum 3 (+ next-sum (* 2 (1+ i)) 1)))
      ((>= next-sum num-lockers) i)))

;; CL-USER> (locker-experiment-efficient 1000)
;; 31

(defun locker-experiment-super-efficient (num-lockers)
  "Efficiently returns number of open lockers after doing a crazy-principal
locker experiment with NUM-LOCKERS lockers.

A locker is flipped as many times as the number of factors it has. If it has an
even number of factors, in its final stage it will remain in a closed state. If
it has an odd number of factors, in its final stage it will be open. The only
numbers that have an odd number of factors are the perfect squares. Therefore,
if you take the square root of any given number, that is the number of perfect
squares equal or less than that number, and therefore, the answer to this
question."
  (isqrt num-lockers))

;; CL-USER> (locker-experiment-super-efficient 1000)
;; 31

;; CL-USER> (loop for i from 1 to 1000
;;               always (= (locker-experiment i)
;;                         (locker-experiment-simplified i)
;;                         (locker-experiment-efficient i)
;;                         (locker-experiment-super-efficient i)))
;; T

;; CL-USER> (time (dotimes (i 100000) (locker-experiment 1000)))
;; Evaluation took:
;;   12.451 seconds of real time
;;   12.448880 seconds of total run time (12.433280 user, 0.015600 system)
;;   [ Run times consist of 0.230 seconds GC time, and 12.219 seconds non-GC time. ]
;;   99.98% CPU
;;   42,266,217,838 processor cycles
;;   3,998,362,416 bytes consed
;; NIL
;; CL-USER> (time (dotimes (i 100000) (locker-experiment-simplified 1000)))
;; Evaluation took:
;;   8.064 seconds of real time
;;   8.065251 seconds of total run time (8.049651 user, 0.015600 system)
;;   [ Run times consist of 0.264 seconds GC time, and 7.802 seconds non-GC time. ]
;;   100.01% CPU
;;   27,366,335,815 processor cycles
;;   5,599,580,464 bytes consed
;; NIL
;; CL-USER> (time (dotimes (i 100000) (locker-experiment-efficient 1000)))
;; Evaluation took:
;;   0.090 seconds of real time
;;   0.078000 seconds of total run time (0.078000 user, 0.000000 system)
;;   86.67% CPU
;;   314,414,578 processor cycles
;;   0 bytes consed
;; NIL
;; CL-USER> (time (dotimes (i 100000) (locker-experiment-super-efficient 1000)))
;; Evaluation took:
;;   0.020 seconds of real time
;;   0.015600 seconds of total run time (0.015600 user, 0.000000 system)
;;   80.00% CPU
;;   53,744,837 processor cycles
;;   0 bytes consed  
;; NIL

(ql:quickload :rt)
(rt:deftest all-methods-equal
    (loop for i from 1 to 1000
       always (= (locker-experiment i)
                 (locker-experiment-simplified i)
                 (locker-experiment-efficient i)
                 (locker-experiment-super-efficient i))) t)
