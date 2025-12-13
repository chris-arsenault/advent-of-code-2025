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
              ;; check consistency
              (let ((ok t))
                (loop for r from rank below lights do
                  (let ((row (nth r matrix)))
                    (when (and (= 0 (count 1 (coerce row 'list)))
                               (= 1 (aref target r)))
                      (setf ok nil))))
                (when ok
                  (incf total (solve-min-presses matrix target lights button-count rank pivot-cols)))))))))
    total))

;; ---------- Part B (integer system) ----------
(defun rref-rational (aug rows cols)
  (let ((pivot-cols (make-array rows :initial-element -1))
        (rank 0))
    (loop for col from 0 below cols while (< rank rows) do
      (let ((pivot -1))
        (loop for r from rank below rows do
          (unless (zerop (aref aug r col))
            (setf pivot r)
            (return)))
        (when (>= pivot 0)
          (when (/= pivot rank)
            (loop for c from col to cols do
              (rotatef (aref aug rank c) (aref aug pivot c))))
          (let ((piv (aref aug rank col)))
            (loop for c from col to cols do
              (setf (aref aug rank c) (/ (aref aug rank c) piv))))
          (loop for r from 0 below rows do
            (unless (= r rank)
              (let ((factor (aref aug r col)))
                (when (/= factor 0)
                  (loop for c from col to cols do
                    (decf (aref aug r c) (* factor (aref aug rank c)))))))))
          (setf (aref pivot-cols rank) col)
          (incf rank)))
    (values rank pivot-cols)))

(defun integer-solution (aug rows cols targets)
  (multiple-value-bind (rank pivot-cols) (rref-rational aug rows cols)
    ;; consistency check
    (loop for r from rank below rows do
      (let ((all-zero t))
        (loop for c from 0 below cols do
          (unless (zerop (aref aug r c)) (setf all-zero nil)))
        (when (and all-zero (not (zerop (aref aug r cols))))
          (return-from integer-solution nil))))
    (let ((pivot-set (make-hash-table))
          (free-cols '()))
      (loop for i from 0 below rank do
        (when (>= (aref pivot-cols i) 0)
          (setf (gethash (aref pivot-cols i) pivot-set) t)))
      (loop for c from 0 below cols do
        (unless (gethash c pivot-set) (push c free-cols)))
      (setf free-cols (nreverse free-cols))
      (let* ((free-count (length free-cols))
             (rhs (make-array rank))
             (coef (make-array (list rank free-count) :initial-element 0)))
        (loop for r from 0 below rank do
          (setf (aref rhs r) (aref aug r cols))
          (loop for f from 0 below free-count do
            (setf (aref coef r f) (aref aug r (nth f free-cols)))))
        (let ((best nil)
              (assign (make-array free-count))
              (cap (max 10 (apply #'max targets))))
          (labels ((dfs (idx sum-so-far)
                     (when (and best (>= sum-so-far best)) (return-from dfs nil))
                     (if (= idx free-count)
                         (let ((total sum-so-far))
                           (loop for r from 0 below rank do
                             (let ((val (aref rhs r)))
                               (loop for f from 0 below free-count do
                                 (decf val (* (aref coef r f) (aref assign f))))
                               (when (or (< val 0) (not (integerp val)))
                                 (return-from dfs nil))
                               (incf total val)))
                           (when (or (null best) (< total best))
                             (setf best total))
                           nil)
                         (loop for v from 0 to cap do
                           (setf (aref assign idx) v)
                           (dfs (1+ idx) (+ sum-so-far v))))))
            (dfs 0 0)
            best))))))

(defun part2 (lines)
  (let ((total 0))
    (dolist (line lines)
      (let ((targets (parse-targets line))
            (buttons (parse-buttons line)))
        (when (and targets buttons)
          (let* ((rows (length targets))
                 (cols (length buttons))
                 (aug (make-array (list rows (1+ cols)) :initial-element 0)))
            (loop for r from 0 below rows do
              (setf (aref aug r cols) (nth r targets)))
            (loop for c from 0 below cols do
              (dolist (idx (nth c buttons))
                (when (< idx rows)
                  (incf (aref aug idx c)))))
            (let ((best (integer-solution aug rows cols targets)))
              (when best (incf total best)))))))
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
