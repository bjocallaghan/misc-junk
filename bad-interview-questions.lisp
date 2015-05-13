(defpackage :interview
  (:use :common-lisp :lisp-unit :split-sequence))

(in-package :interview)

(lisp-unit:remove-tests :all)

;;; fizzbuzz

(defun fizzbuzzify (n)
  (cond
    ((zerop (mod n 15)) 'fizzbuzz)
    ((zerop (mod n 5)) 'fizz)
    ((zerop (mod n 3)) 'buzz)
    (t n)))

(define-test fizzbuzzify
  (assert-equal 'fizz (fizzbuzzify 5))
  (assert-equal 'fizz (fizzbuzzify 25))
  (assert-equal 'buzz (fizzbuzzify 3))
  (assert-equal 'buzz (fizzbuzzify 9))
  (assert-equal 'fizzbuzz (fizzbuzzify 15))
  (assert-equal 'fizzbuzz (fizzbuzzify 45)))

;; (loop for i from 1 to 100 do (format t "~&~a" (fizzbuzzify i)))

;;; area function

(defun area (radius)
  (* pi radius radius))

;;; pi approximation--unfinished, but close
(defun pi-approximate (iterations &optional verbose)
  (let ((tolerance .001)
        (value 0.0))
    (declare (ignore tolerance))
    (do ((iteration 1 (+ iteration 2))
         (multiplier 1 (* -1 multiplier))
         (j 1 (1+ j)))
        ((= j iterations) (float (* 4 value)))
      (let ((term (* multiplier (/ iteration))))
        (when verbose
          (format t "~&iteration: ~d || term: ~a" iteration term))
        (incf value term)
        (when verbose
          (format t "~&~8$" (* 4 value)))))))

;;; detecting cycles in a linked list

(defun cyclicalp (list &optional (acc nil))
  (if (find list acc :test 'eq)
      t
      (progn
        (push list acc)
        (if (null list)
            nil
            (cyclicalp (cdr list) acc)))))

(define-test cyclicalp
  (let ((list-1 '(2 3 4))
        (list-2 '(5 6 7)))
    (setf (cdr (last list-1)) list-1)
    (assert-true (cyclicalp list-1))
    (assert-false (cyclicalp list-2))))

;;; string-from-binary-string

(defun integer-from-binary-string (string)
  "Returns integer equivalent of a 'binary string' STRING. e.g. \"1011\" -> 11."
  (let ((place-value 1)
        (running-sum 0))
    (dolist (digit (nreverse (coerce string 'list)) running-sum)
      (ecase digit
        (#\0 nil)
        (#\1 (incf running-sum place-value)))
      (setf place-value (* 2 place-value)))))

(define-test integer-from-binary-string
  (dolist (pair '((3 "11")
                  (1 "1")
                  (0 "0")
                  (2 "10")
                  (8 "1000")
                  (104 "01101000")))
    (assert-equal (first pair) (integer-from-binary-string (second pair))))
  (assert-error 'error (integer-from-binary-string " "))
  (assert-error 'error (integer-from-binary-string "121"))
  (assert-error 'error (integer-from-binary-string "aaa")))

(defun string-from-binary-string (string)
  "Returns the 'normal' string equivalent of a 'binary string' STRING.
e.g. \"01101000 01100101 01101100 01101100 01101111\" -> \"hello\""
  (coerce (mapcar (lambda (x) (code-char (integer-from-binary-string x)))
                  (split-sequence #\space string)) 'string))

(define-test string-from-binary-string
  (assert-equal "hello" (string-from-binary-string
                         "01101000 01100101 01101100 01101100 01101111")))

(run-tests :all)
