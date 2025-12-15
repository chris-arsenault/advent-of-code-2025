#!/usr/bin/env sbcl --script

(defun read-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line collect line)))

(defun split-commas (s)
  (let ((parts '())
        (start 0)
        (len (length s)))
    (loop for i from 0 below len
          for ch = (char s i) do
            (when (char= ch #\,)
              (push (subseq s start i) parts)
              (setf start (1+ i))))
    (push (subseq s start len) parts)
    (nreverse parts)))

(defun load-points (lines)
  (let ((pts '()))
    (dolist (line lines)
      (let ((trim (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
        (when (> (length trim) 0)
          (let* ((parts (split-commas trim))
                 (x (parse-integer (first parts)))
                 (y (parse-integer (second parts)))
                 (z (parse-integer (third parts))))
            (push (list x y z) pts)))))
    (nreverse pts)))

(defstruct (dsu (:constructor %make-dsu (parent size components)))
  parent size components)

(defun make-dsu-init (n)
  (let ((p (make-array n))
        (s (make-array n)))
    (loop for i from 0 below n do
      (setf (aref p i) i
            (aref s i) 1))
    (%make-dsu p s n)))

(defun dsu-find (d x)
  (let* ((p (dsu-parent d))
         (px (aref p x)))
    (if (= px x)
        x
        (let ((root (dsu-find d px)))
          (setf (aref p x) root)
          root))))

(defun dsu-union (d a b)
  (let* ((ra (dsu-find d a))
         (rb (dsu-find d b)))
    (if (= ra rb)
        nil
        (let ((sa (aref (dsu-size d) ra))
              (sb (aref (dsu-size d) rb)))
          (when (< sa sb)
            (rotatef ra rb)
            (rotatef sa sb))
          (setf (aref (dsu-parent d) rb) ra)
          (incf (aref (dsu-size d) ra) sb)
          (decf (dsu-components d))
          t))))

(defun build-edges (pts)
  (let* ((n (length pts))
         (edges '()))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n do
        (destructuring-bind (xi yi zi) (nth i pts)
          (destructuring-bind (xj yj zj) (nth j pts)
            (let* ((dx (- xi xj))
                   (dy (- yi yj))
                   (dz (- zi zj))
                   (d2 (+ (* dx dx) (* dy dy) (* dz dz))))
              (push (list d2 i j) edges))))))
    (sort edges #'< :key #'first)))

(defun part1 (n edges &optional (k 1000))
  (let ((dsu (make-dsu-init n)))
    (loop for idx from 0 below (min k (length edges)) do
      (destructuring-bind (_ a b) (nth idx edges)
        (declare (ignore _))
        (dsu-union dsu a b)))
    (let ((sizes '()))
      (loop for i from 0 below n do
        (when (= (dsu-find dsu i) i)
          (push (aref (dsu-size dsu) i) sizes)))
      (setf sizes (sort sizes #'>))
      (loop while (< (length sizes) 3) do (push 1 sizes))
      (* (first sizes) (second sizes) (third sizes)))))

(defun part2 (pts edges)
  (let* ((n (length pts))
         (dsu (make-dsu-init n))
         (last-prod 0))
    (dolist (edge edges)
      (destructuring-bind (_ a b) edge
        (declare (ignore _))
        (when (dsu-union dsu a b)
          (setf last-prod (* (first (nth a pts)) (first (nth b pts))))
          (when (= (dsu-components dsu) 1)
            (return)))))
    last-prod))

(defun elapsed-ms (start end)
  (* 1000.0 (/ (- end start) internal-time-units-per-second)))

(defun main ()
  (let ((t0 (get-internal-real-time)))
    (let* ((lines (read-lines "input.txt"))
           (pts (load-points lines))
           (edges (build-edges pts))
           (p1 (part1 (length pts) edges 1000))
           (p2 (part2 pts edges))
           (t1 (get-internal-real-time))
           (elapsed (elapsed-ms t0 t1)))
      (format t "top3_product=~A final_join_x_product=~A elapsed_ms=~,3f~%" p1 p2 elapsed))))

(main)
