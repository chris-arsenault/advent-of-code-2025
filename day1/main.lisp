#!/usr/bin/env sbcl --script

(defun read-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line collect line)))

(defun elapsed-ms (start end)
  (* 1000.0 (/ (- end start) internal-time-units-per-second)))

(defun simulate (lines)
  (let ((pos 50)
        (zero 0)
        (cross 0))
    (dolist (raw lines)
      (let* ((line (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
        (when (> (length line) 0)
          (let* ((sign (if (char= (char line 0) #\R) 1 -1))
                 (mag (parse-integer line :start 1))
                 (first (if (= sign 1) (- 100 pos) pos)))
            (when (= first 0) (setf first 100))
            (when (>= mag first)
              (incf cross (1+ (truncate (- mag first) 100))))
            (setf pos (mod (+ pos (* sign mag)) 100))
            (when (= pos 0) (incf zero))))))
    (values zero cross pos)))

(defun main ()
  (let* ((lines (read-lines "input.txt"))
         (t0 (get-internal-real-time))
         (sim (multiple-value-list (simulate lines)))
         (t1 (get-internal-real-time))
         (elapsed (elapsed-ms t0 t1)))
    (destructuring-bind (zero crossings final-pos) sim
      (format t "zero_landings=~A crossings=~A final_pos=~A elapsed_ms=~,3f~%"
              zero crossings final-pos elapsed))))

(main)
