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
    (coerce (nreverse pts) 'vector)))

(defun max-rectangle-any (pts)
  (let ((best 0)
        (n (length pts)))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n do
        (let* ((p-i (aref pts i))
               (p-j (aref pts j))
               (x1 (first p-i)) (y1 (second p-i))
               (x2 (first p-j)) (y2 (second p-j))
               (dx (abs (- x1 x2)))
               (dy (abs (- y1 y2)))
               (area (* (1+ dx) (1+ dy))))
          (when (> area best) (setf best area)))))
    best))

(defun point-on-edge-p (px py x1 y1 x2 y2)
  (cond
    ((= x1 x2)
     (and (= px x1) (>= py (min y1 y2)) (<= py (max y1 y2))))
    ((= y1 y2)
     (and (= py y1) (>= px (min x1 x2)) (<= px (max x1 x2))))
    (t nil)))

(defun point-inside-p (px py poly)
  (let* ((n (length poly))
         (inside nil)
         (j (1- n)))
    (loop for i from 0 below n do
      (let* ((p-j (aref poly j))
             (p-i (aref poly i))
             (x1 (first p-j)) (y1 (second p-j))
             (x2 (first p-i)) (y2 (second p-i)))
        (when (point-on-edge-p px py x1 y1 x2 y2)
          (return-from point-inside-p t))
        (when (and (/= y1 y2)
                   (not (eq (> y1 py) (> y2 py))))
          (let ((x-int (+ x1 (truncate (* (- x2 x1) (- py y1)) (- y2 y1)))))
            (when (< px x-int)
              (setf inside (not inside)))))
        (setf j i)))
    inside))

(defun edge-crosses-interior-p (xlo xhi ylo yhi x1 y1 x2 y2)
  (cond
    ((= x1 x2)  ; vertical edge
     (and (> x1 xlo) (< x1 xhi)  ; strictly between left and right
          (let ((ya (min y1 y2))
                (yb (max y1 y2)))
            (and (> yb ylo) (< ya yhi)))))  ; overlaps y range
    ((= y1 y2)  ; horizontal edge
     (and (> y1 ylo) (< y1 yhi)  ; strictly between bottom and top
          (let ((xa (min x1 x2))
                (xb (max x1 x2)))
            (and (> xb xlo) (< xa xhi)))))  ; overlaps x range
    (t nil)))

(defun rect-inside-polygon-p (xlo xhi ylo yhi poly)
  (and (point-inside-p xlo ylo poly)
       (point-inside-p xlo yhi poly)
       (point-inside-p xhi ylo poly)
       (point-inside-p xhi yhi poly)
       (let ((n (length poly))
             (j (1- (length poly))))
         (loop for i from 0 below n do
           (let* ((p-j (aref poly j))
                  (p-i (aref poly i))
                  (x1 (first p-j)) (y1 (second p-j))
                  (x2 (first p-i)) (y2 (second p-i)))
             (when (edge-crosses-interior-p xlo xhi ylo yhi x1 y1 x2 y2)
               (return-from rect-inside-polygon-p nil))
             (setf j i)))
         t)))

(defun max-rectangle-inside (pts poly)
  (let ((best 0)
        (n (length pts)))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n do
        (let* ((p1 (aref pts i)) (p2 (aref pts j))
               (x1 (first p1)) (y1 (second p1))
               (x2 (first p2)) (y2 (second p2)))
          (when (and (/= x1 x2) (/= y1 y2))
            (let* ((xmin (min x1 x2)) (xmax (max x1 x2))
                   (ymin (min y1 y2)) (ymax (max y1 y2)))
              (when (rect-inside-polygon-p xmin xmax ymin ymax poly)
                (let ((area (* (1+ (- xmax xmin)) (1+ (- ymax ymin)))))
                  (when (> area best) (setf best area)))))))))
    best))

(defun main ()
  (let* ((lines (read-lines "input.txt"))
         (pts (load-points lines))
         (poly pts)
         (t0 (get-internal-real-time))
         (p1 (max-rectangle-any pts))
         (p2 (max-rectangle-inside pts poly))
         (t1 (get-internal-real-time))
         (elapsed (elapsed-ms t0 t1)))
    (format t "max_rect_area=~A max_green_rect_area=~A elapsed_ms=~,3f~%" p1 p2 elapsed)))

(main)
