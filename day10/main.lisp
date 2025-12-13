#!/usr/bin/env sbcl --script

(defun read-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line collect line)))

(defun elapsed-ms (start end)
  (* 1000.0 (/ (- end start) internal-time-units-per-second)))

(defun parse-between (line start-ch end-ch)
  (let ((lb (position start-ch line))
        (rb (position end-ch line :from-end t)))
    (when (and lb rb (> rb lb))
      (subseq line (1+ lb) rb))))

(defun split-numbers (s &key (sep #\,))
  (let ((nums '())
        (start 0)
        (len (length s)))
    (loop for i from 0 to len do
      (when (or (= i len) (char= (char s i) sep))
        (let ((part (string-trim '(#\Space) (subseq s start i))))
          (when (> (length part) 0)
            (push (parse-integer part) nums)))
        (setf start (1+ i))))
    (nreverse nums)))

(defun parse-buttons (line)
  (let ((buttons '())
        (pos 0)
        (len (length line)))
    (loop while (< pos len) do
      (let ((lb (position #\( line :start pos))
            (rb (position #\) line :start pos)))
        (if (and lb rb (< lb rb))
            (progn
              (push (split-numbers (subseq line (1+ lb) rb) :sep #\,) buttons)
              (setf pos (1+ rb)))
            (return))))
    (nreverse buttons)))

(defun parse-targets (line)
  (let ((chunk (parse-between line #\{ #\})))
    (when chunk (split-numbers chunk :sep #\,))))

;; ---------- Part A (GF(2)) ----------
(defun rref-binary (matrix target lights buttons)
  (let ((pivot-cols (make-array lights :initial-element -1))
        (rank 0))
    (loop for col from 0 below buttons while (< rank lights) do
      (let ((pivot -1))
        (loop for r from rank below lights do
          (when (= 1 (bit (nth r matrix) col))
            (setf pivot r)
            (return)))
        (when (>= pivot 0)
          (rotatef (nth rank matrix) (nth pivot matrix))
          (rotatef (aref target rank) (aref target pivot))
          (setf (aref pivot-cols rank) col)
          (loop for r from (1+ rank) below lights do
            (when (= 1 (bit (nth r matrix) col))
              (setf (nth r matrix) (bit-xor (nth r matrix) (nth rank matrix)))
              (setf (aref target r) (logxor (aref target r) (aref target rank)))))
          (incf rank))))
    ;; back substitution
    (loop for i downfrom (1- rank) to 0 do
      (let ((col (aref pivot-cols i)))
        (when (>= col 0)
          (loop for r from 0 below i do
            (when (= 1 (bit (nth r matrix) col))
              (setf (nth r matrix) (bit-xor (nth r matrix) (nth i matrix)))
              (setf (aref target r) (logxor (aref target r) (aref target i))))))))
    (values rank pivot-cols)))

(defun solve-min-presses (matrix target lights buttons rank pivot-cols)
  (let ((pivot-set (make-hash-table))
        (free-cols '()))
    (loop for i from 0 below rank do
      (when (>= (aref pivot-cols i) 0)
        (setf (gethash (aref pivot-cols i) pivot-set) t)))
    (loop for c from 0 below buttons do
      (unless (gethash c pivot-set) (push c free-cols)))
    (setf free-cols (nreverse free-cols))
    (let ((free-count (length free-cols)))
      (if (> free-count 20)
          ;; greedy back-substitution
          (let ((sol (make-array buttons :initial-element 0)))
            (loop for i downfrom (1- rank) to 0 do
              (let* ((col (aref pivot-cols i))
                     (val (aref target i))
                     (row (nth i matrix)))
                (loop for c from 0 below buttons do
                  (when (and (/= c col) (= 1 (bit row c)))
                    (setf val (logxor val (aref sol c)))))
                (setf (aref sol col) val)))
            (loop for c from 0 below buttons sum (aref sol c)))
          ;; enumerate free variables
          (let ((best nil)
                (total-combos (ash 1 free-count)))
            (loop for mask from 0 below total-combos do
              (let ((sol (make-array buttons :initial-element 0))
                    (weight 0))
                (loop for k from 0 below free-count do
                  (when (/= 0 (logand mask (ash 1 k)))
                    (let ((col (nth k free-cols)))
                      (setf (aref sol col) 1)
                      (incf weight))))
                (loop for i downfrom (1- rank) to 0 do
                  (let* ((col (aref pivot-cols i))
                         (val (aref target i))
                         (row (nth i matrix)))
                    (loop for c from 0 below buttons do
                      (when (and (/= c col) (= 1 (bit row c)))
                        (setf val (logxor val (aref sol c)))))
                    (setf (aref sol col) val)
                    (incf weight val)))
                (when (or (null best) (< weight best))
                  (setf best weight))))
            best)))))

(defun part1 (lines)
  (let ((total 0))
    (dolist (line lines)
      (let* ((pattern (parse-between line #\[ #\]))
             (buttons (parse-buttons line)))
        (when pattern
          (let* ((lights (length pattern))
                 (target (make-array lights :element-type 'bit))
                 (button-count (length buttons))
                 (matrix (loop for _ from 0 below lights
                               collect (make-array button-count :element-type 'bit :initial-element 0))))
            (loop for i from 0 below lights do
              (setf (aref target i) (if (char= (char pattern i) #\#) 1 0)))
            (loop for idx from 0 below button-count
                  for btn in buttons do
                    (dolist (pos btn)
                      (when (< pos lights)
                        (setf (bit (nth pos matrix) idx) (logxor (bit (nth pos matrix) idx) 1)))))
            (multiple-value-bind (rank pivot-cols) (rref-binary matrix target lights button-count)
              (let ((ok t))
                (loop for r from rank below lights do
                  (let ((row (nth r matrix)))
                    (when (and (= 0 (count 1 (coerce row 'list)))
                               (= 1 (aref target r)))
                      (setf ok nil))))
                (when ok
                  (incf total (solve-min-presses matrix target lights button-count rank pivot-cols)))))))))
    total))

;; ---------- Part B (floating-point RREF) ----------
(defconstant +eps+ 1.0d-9)

(defun solve-machine-part2 (targets buttons)
  (let* ((counters (length targets))
         (n-buttons (length buttons)))
    (when (or (zerop counters) (zerop n-buttons))
      (return-from solve-machine-part2 0))
    ;; Build matrix counters x buttons
    (let ((mat (make-array (list counters n-buttons) :element-type 'double-float :initial-element 0.0d0))
          (rhs (make-array counters :element-type 'double-float)))
      (loop for i from 0 below counters do
        (setf (aref rhs i) (coerce (nth i targets) 'double-float)))
      (loop for cidx from 0 below n-buttons
            for btn in buttons do
        (dolist (t-idx btn)
          (when (and (>= t-idx 0) (< t-idx counters))
            (setf (aref mat t-idx cidx) 1.0d0))))
      ;; RREF
      (let ((pivot-cols (make-array counters :initial-element -1))
            (row 0))
        (loop for col from 0 below n-buttons while (< row counters) do
          (let ((pivot -1)
                (best-val 0.0d0))
            (loop for r from row below counters do
              (let ((val (abs (aref mat r col))))
                (when (and (> val +eps+) (> val best-val))
                  (setf best-val val pivot r))))
            (when (>= pivot 0)
              (when (/= pivot row)
                (loop for c from 0 below n-buttons do
                  (rotatef (aref mat row c) (aref mat pivot c)))
                (rotatef (aref rhs row) (aref rhs pivot)))
              (let ((piv (aref mat row col)))
                (loop for c from col below n-buttons do
                  (setf (aref mat row c) (/ (aref mat row c) piv)))
                (setf (aref rhs row) (/ (aref rhs row) piv)))
              (loop for r from 0 below counters do
                (unless (= r row)
                  (let ((f (aref mat r col)))
                    (when (> (abs f) +eps+)
                      (loop for c from col below n-buttons do
                        (decf (aref mat r c) (* f (aref mat row c))))
                      (decf (aref rhs r) (* f (aref rhs row)))))))
              (setf (aref pivot-cols row) col)
              (incf row))))
        (let ((rank row))
          ;; Consistency check
          (loop for r from rank below counters do
            (let ((maxv 0.0d0))
              (loop for c from 0 below n-buttons do
                (setf maxv (max maxv (abs (aref mat r c)))))
              (when (and (< maxv +eps+) (> (abs (aref rhs r)) +eps+))
                (return-from solve-machine-part2 nil))))
          ;; Find free columns
          (let ((used (make-array n-buttons :initial-element nil)))
            (loop for i from 0 below rank do
              (when (>= (aref pivot-cols i) 0)
                (setf (aref used (aref pivot-cols i)) t)))
            (let ((free-cols (loop for c from 0 below n-buttons unless (aref used c) collect c)))
              (let* ((free-count (length free-cols))
                     (coef (make-array (list rank free-count) :element-type 'double-float :initial-element 0.0d0)))
                (loop for r from 0 below rank do
                  (loop for f from 0 below free-count do
                    (setf (aref coef r f) (aref mat r (nth f free-cols)))))
                (let ((sum-targets (reduce #'+ targets))
                      (best 0)
                      (free-vals (make-array free-count :initial-element 0)))
                  (setf best sum-targets)
                  (labels ((evaluate (cur)
                             (when (>= cur best) (return-from evaluate))
                             (let ((sum cur))
                               (loop for r from 0 below rank do
                                 (let ((v (aref rhs r)))
                                   (loop for f from 0 below free-count do
                                     (decf v (* (aref coef r f) (aref free-vals f))))
                                   (when (< v (- +eps+)) (return-from evaluate))
                                   (let ((iv (round v)))
                                     (when (> (abs (- iv v)) +eps+) (return-from evaluate))
                                     (incf sum iv)
                                     (when (>= sum best) (return-from evaluate)))))
                               (when (< sum best)
                                 (setf best sum))))
                           (quick (idx cur cap)
                             (when (>= cur best) (return-from quick))
                             (if (= idx free-count)
                                 (evaluate cur)
                                 (loop for v from 0 to cap do
                                   (when (>= (+ cur v) best) (return))
                                   (setf (aref free-vals idx) v)
                                   (quick (1+ idx) (+ cur v) cap))))
                           (dfs (idx cur)
                             (when (>= cur best) (return-from dfs))
                             (if (= idx free-count)
                                 (evaluate cur)
                                 (let ((maxv (- best cur)))
                                   (loop for v from 0 to maxv do
                                     (setf (aref free-vals idx) v)
                                     (dfs (1+ idx) (+ cur v)))))))
                    (evaluate 0)
                    (let ((seed-cap (min 400 best)))
                      (when (and (> free-count 0) (> seed-cap 0))
                        (quick 0 0 seed-cap)))
                    (if (> free-count 0)
                        (dfs 0 0)
                        (evaluate 0)))
                  best)))))))))

(defun part2 (lines)
  (let ((total 0))
    (dolist (line lines)
      (let ((targets (parse-targets line))
            (buttons (parse-buttons line)))
        (when (and targets buttons)
          (let ((result (solve-machine-part2 targets buttons)))
            (when result
              (incf total result))))))
    total))

(defun main ()
  (let* ((lines (read-lines "input.txt"))
         (t0 (get-internal-real-time))
         (p1 (part1 lines))
         (p2 (part2 lines))
         (t1 (get-internal-real-time))
         (elapsed (elapsed-ms t0 t1)))
    (format t "min_lights_presses=~A min_counter_presses=~A elapsed_ms=~,3f~%" p1 p2 elapsed)))

(main)
