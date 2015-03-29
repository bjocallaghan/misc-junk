(defun timestamp ()
  (let ((day-names '("Monday" "Tuesday" "Wednesday"
                     "Thursday" "Friday" "Saturday" "Sunday")))
    (multiple-value-bind
          (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
      (declare (ignore dst-p))
      (format nil "~a ~d/~2,'0d/~d ~2,'0d:~2,'0d:~2,'0d (GMT~@d)"
              (nth day-of-week day-names) month date
              year hour minute second (- tz)))))
