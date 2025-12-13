#!/usr/bin/env sbcl --script

(defun read-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line collect line)))

(defun elapsed-ms (start end)
  (* 1000.0 (/ (- end start) internal-time-units-per-second)))

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
                 (y (parse-integer (second parts))))
            (push (list x y) pts)))))
    (nreverse pts)))

(defun max-rectangle-any (pts)
  (let ((best 0)
        (n (length pts)))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n do
        (let* ((p-i (nth i pts))
               (p-j (nth j pts))
               (x1 (first p-i)) (y1 (second p-i))
               (x2 (first p-j)) (y2 (second p-j)))
          (when (and (/= x1 x2) (/= y1 y2))
            (let ((area (* (abs (- x1 x2)) (abs (- y1 y2)))))
              (when (> area best) (setf best area)))))))
    best))

(defun orientation (p q r)
  (let* ((val (- (* (- (second q) (second p)) (- (first r) (first q)))
                 (* (- (first q) (first p)) (- (second r) (second q))))))
    (cond ((> val 0) 1)
          ((< val 0) -1)
          (t 0))))

(defun on-segment (p q r)
  (and (<= (min (first p) (first r)) (first q) (max (first p) (first r)))
       (<= (min (second p) (second r)) (second q) (max (second p) (second r)))))

(defun segments-intersect (p1 q1 p2 q2)
  (let* ((o1 (orientation p1 q1 p2))
         (o2 (orientation p1 q1 q2))
         (o3 (orientation p2 q2 p1))
         (o4 (orientation p2 q2 q1)))
    (cond
      ((and (/= o1 o2) (/= o3 o4)) t)
      ((and (= o1 0) (on-segment p1 p2 q1)) t)
      ((and (= o2 0) (on-segment p1 q2 q1)) t)
      ((and (= o3 0) (on-segment p2 p1 q2)) t)
      ((and (= o4 0) (on-segment p2 q1 q2)) t)
      (t nil))))

(defun point-in-poly (poly point)
  ;; Ray casting
  (let* ((n (length poly))
         (x (first point))
         (y (second point))
         (inside nil))
    (loop for i from 0 below n
          for j = (mod (1- i) n) do
            (destructuring-bind (xi yi) (nth i poly)
              (destructuring-bind (xj yj) (nth j poly)
                (when (and (/= yi yj)
                           (<= (min yi yj) y)
                           (< y (max yi yj)))
                  (let ((xint (+ xi (* (- y yi) (/ (- xj xi) (- yj yi))))))
                    (when (< xint x) (setf inside (not inside))))))))
    inside))

(defun rectangle-inside-p (poly corners)
  ;; corners list of 4 points axis-aligned rectangle
  (dolist (c corners)
    (unless (point-in-poly poly c) (return-from rectangle-inside-p nil)))
  (let ((edges (loop for i from 0 below (length poly)
                     collect (list (nth i poly) (nth (mod (1+ i) (length poly)) poly)))))
    (let ((rect-edges (list (list (first corners) (second corners))
                            (list (second corners) (third corners))
                            (list (third corners) (fourth corners))
                            (list (fourth corners) (first corners)))))
      (dolist (re rect-edges)
        (destructuring-bind (a b) re
          (dolist (e edges)
            (destructuring-bind (p q) e
              (when (segments-intersect a b p q)
                (return-from rectangle-inside-p nil))))))))
  t)

(defun max-rectangle-inside (pts poly)
  (let ((best 0)
        (n (length pts)))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n do
        (let* ((p1 (nth i pts)) (p2 (nth j pts))
               (x1 (first p1)) (y1 (second p1))
               (x2 (first p2)) (y2 (second p2)))
          (when (and (/= x1 x2) (/= y1 y2))
            (let* ((xmin (min x1 x2)) (xmax (max x1 x2))
                   (ymin (min y1 y2)) (ymax (max y1 y2))
                   (corners (list (list xmin ymin)
                                  (list xmin ymax)
                                  (list xmax ymax)
                                  (list xmax ymin))))
              (when (rectangle-inside-p poly corners)
                (let ((area (* (- xmax xmin) (- ymax ymin))))
                  (when (> area best) (setf best area)))))))))
    best))

(defun main ()
  (let* ((lines (read-lines "input.txt"))
         (pts (load-points lines))
         (poly pts) ; polygon given by same list
         (t0 (get-internal-real-time))
         (p1 (max-rectangle-any pts))
         (p2 (max-rectangle-inside pts poly))
         (t1 (get-internal-real-time))
         (elapsed (elapsed-ms t0 t1)))
    (format t "max_rect_area=~A max_green_rect_area=~A elapsed_ms=~,3f~%" p1 p2 elapsed)))

(main)
