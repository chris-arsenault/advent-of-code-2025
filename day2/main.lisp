#!/usr/bin/env sbcl --script

(defun read-file-string (path)
  (with-open-file (in path :direction :input :element-type 'character)
    (let* ((len (file-length in))
           (data (make-string len)))
      (read-sequence data in)
      data)))

(defun elapsed-ms (start end)
  (* 1000.0 (/ (- end start) internal-time-units-per-second)))

(defun split-on-chars (str chars)
  (let ((parts '())
        (start 0)
        (len (length str)))
    (labels ((delim-p (ch) (find ch chars :test #'char=))
             (emit (end)
               (when (> end start)
                 (push (subseq str start end) parts))))
      (loop for i from 0 below len
            for ch = (char str i) do
              (if (delim-p ch)
                  (progn
                    (emit i)
                    (setf start (1+ i)))
                  nil))
      (emit len))
    (nreverse parts)))

(defun parse-ranges (text)
  (let ((ranges '()))
    (dolist (part (split-on-chars text '(#\, #\Newline #\Return)))
      (when (> (length part) 0)
        (let* ((dash (position #\- part))
               (a (parse-integer part :end dash))
               (b (parse-integer part :start (1+ dash))))
          (push (list a b) ranges))))
    (nreverse ranges)))

(defun digits-len (n)
  (length (write-to-string n)))

(defun pow10 (k)
  (loop with res = 1
        repeat k do (setf res (* res 10))
        finally (return res)))

(defun generate-even-half (max-n)
  (let* ((max-len (digits-len max-n))
         (vals '()))
    (loop for half-len from 1 to (floor max-len 2) do
      (let* ((start (pow10 (1- half-len)))
             (limit (pow10 half-len))
             (mul (pow10 half-len)))
        (loop for num from start below limit
              for n = (+ (* num mul) num)
              while (<= n max-n) do
                (push n vals))))
    (coerce (sort vals #'<) 'vector)))

(defun generate-periodic (max-n)
  (let* ((max-len (digits-len max-n))
         (set (make-hash-table))
         (vals '()))
    (loop for base-len from 1 to (ceiling max-len 2) do
      (let ((start (pow10 (1- base-len)))
            (limit (pow10 base-len)))
        (loop for base from start below limit do
          (let ((base-str (write-to-string base)))
            (loop for reps from 2 to (floor max-len base-len) do
              (let* ((s (with-output-to-string (out)
                          (dotimes (_ reps) (write-string base-str out))))
                     (n (parse-integer s)))
                (when (> n max-n) (return))
                (unless (gethash n set)
                  (setf (gethash n set) t)
                  (push n vals))))))))
    (coerce (sort vals #'<) 'vector)))

(defun prefix-sums (vec)
  (let* ((n (length vec))
         (pref (make-array (1+ n) :element-type 'integer)))
    (setf (aref pref 0) 0)
    (loop for i from 0 below n do
      (setf (aref pref (1+ i)) (+ (aref pref i) (aref vec i))))
    pref))

(defun lower-bound (vec value)
  (let ((lo 0)
        (hi (length vec)))
    (loop while (< lo hi) do
      (let ((mid (ash (+ lo hi) -1)))
        (if (< (aref vec mid) value)
            (setf lo (1+ mid))
            (setf hi mid))))
    lo))

(defun upper-bound (vec value)
  (let ((lo 0)
        (hi (length vec)))
    (loop while (< lo hi) do
      (let ((mid (ash (+ lo hi) -1)))
        (if (<= (aref vec mid) value)
            (setf lo (1+ mid))
            (setf hi mid))))
    lo))

(defun range-sum (vec pref lo hi)
  (let ((i (lower-bound vec lo))
        (j (upper-bound vec hi)))
    (- (aref pref j) (aref pref i))))

(defun solve (text)
  (let* ((ranges (parse-ranges text))
         (max-n (apply #'max (mapcar #'second ranges)))
         (even-vals (generate-even-half max-n))
         (even-pref (prefix-sums even-vals))
         (periodic (generate-periodic max-n))
         (periodic-pref (prefix-sums periodic))
         (p1 0)
         (p2 0))
    (dolist (r ranges)
      (let ((a (first r)) (b (second r)))
        (incf p1 (range-sum even-vals even-pref a b))
        (incf p2 (range-sum periodic periodic-pref a b))))
    (values p1 p2)))

(defun main ()
  (let* ((text (read-file-string "input.txt"))
         (t0 (get-internal-real-time))
         (res (multiple-value-list (solve text)))
         (t1 (get-internal-real-time))
         (elapsed (elapsed-ms t0 t1)))
    (destructuring-bind (p1 p2) res
      (format t "repeated-halves-sum=~A repeated-pattern-sum=~A elapsed_ms=~,3f~%"
              p1 p2 elapsed))))

(main)
