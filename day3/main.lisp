#!/usr/bin/env sbcl --script

(defun read-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line collect line)))

(defun elapsed-ms (start end)
  (* 1000.0 (/ (- end start) internal-time-units-per-second)))

(defun string-digits (s)
  (loop for ch across s collect (- (char-code ch) (char-code #\0))))

(defun best-two (s)
  (let* ((digits (string-digits (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
         (n (length digits)))
    (if (< n 2)
        0
        (let* ((suffix (make-array (1+ n)))
               (best -1))
          (setf (aref suffix n) 0)
          (loop for i from (1- n) downto 0 do
            (setf (aref suffix i) (max (aref suffix (1+ i)) (nth i digits))))
          (loop for i from 0 below (1- n) do
            (let ((candidate (+ (* 10 (nth i digits)) (aref suffix (1+ i)))))
              (when (> candidate best) (setf best candidate))))
          best))))

(defun best-k (s k)
  (let* ((digits (string-digits (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
         (drop (- (length digits) k))
         (stack '()))
    (dolist (d digits)
      (loop while (and (> drop 0) stack (< (car stack) d)) do
        (pop stack)
        (decf drop))
      (push d stack))
    (setf stack (nreverse stack))
    (when (> (length stack) k)
      (setf stack (subseq stack 0 k)))
    (parse-integer (map 'string #'digit-char stack))))

(defun solve (lines &key (k 12))
  (let ((p1 0)
        (p2 0))
    (dolist (line lines)
      (let ((trim (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
        (when (> (length trim) 0)
          (incf p1 (best-two trim))
          (incf p2 (best-k trim k)))))
    (values p1 p2)))

(defun main ()
  (let* ((path (or (probe-file "input_eric.txt") (probe-file "input.txt")))
         (lines (read-lines path))
         (t0 (get-internal-real-time))
         (res (multiple-value-list (solve lines :k 12)))
         (t1 (get-internal-real-time))
         (elapsed (elapsed-ms t0 t1)))
    (destructuring-bind (p1 p2) res
      (format t "max-2-digit-sum=~A max-12-digit-sum=~A elapsed_ms=~,3f~%"
              p1 p2 elapsed))))

(main)
