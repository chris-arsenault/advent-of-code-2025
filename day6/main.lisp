#!/usr/bin/env sbcl --script

(defun read-file-string (path)
  (with-open-file (in path :direction :input :element-type 'character)
    (let* ((len (file-length in))
           (data (make-string len)))
      (read-sequence data in)
      data)))

(defun load-grid (text)
  (let* ((lines (with-input-from-string (in text)
                  (loop for line = (read-line in nil nil)
                        while line collect line)))
         (width (apply #'max (mapcar #'length lines))))
    (mapcar (lambda (s)
              (let ((pad (- width (length s))))
                (if (> pad 0) (concatenate 'string s (make-string pad :initial-element #\Space)) s)))
            lines)))

(defun split-blocks (grid)
  (let* ((height (length grid))
         (width (length (first grid)))
         (empty-col (make-array width :element-type 'boolean)))
    (loop for c from 0 below width do
      (setf (aref empty-col c)
            (loop for r from 0 below height
                  always (char= (char (nth r grid) c) #\Space))))
    (let ((blocks '())
          (c 0))
      (loop while (< c width) do
        (loop while (and (< c width) (aref empty-col c)) do (incf c))
        (when (< c width)
          (let ((start c))
            (loop while (and (< c width) (not (aref empty-col c))) do (incf c))
            (push (list start c) blocks))))
      (nreverse blocks))))

(defun problem-operator (row start end)
  (loop for i from start below end
        for ch = (char row i)
        when (or (char= ch #\+) (char= ch #\*))
          do (return ch)))

(defun eval-numbers (nums op)
  (if (char= op #\+)
      (reduce #'+ nums)
      (reduce #'* nums :initial-value 1)))

(defun part1 (grid blocks)
  (let* ((op-row (car (last grid)))
         (rows (butlast grid)))
    (loop for (start end) in blocks sum
          (let* ((op (problem-operator op-row start end))
                 (nums '()))
            (dolist (row rows)
              (let* ((slice (string-trim '(#\Space) (subseq row start end))))
                (when (> (length slice) 0)
                  (push (parse-integer slice) nums))))
            (eval-numbers (nreverse nums) op)))))

(defun part2 (grid blocks)
  (let* ((height (1- (length grid))) ; exclude operator row
         (op-row (car (last grid))))
    (loop for (start end) in blocks sum
          (let ((op (problem-operator op-row start end))
                (nums '()))
            (loop for c downfrom (1- end) to start do
              (let ((digits '()))
                (loop for r from 0 below height do
                  (let ((ch (char (nth r grid) c)))
                    (when (digit-char-p ch)
                      (push ch digits))))
                (when digits
                  (push (parse-integer (coerce (nreverse digits) 'string)) nums))))
            (eval-numbers (nreverse nums) op)))))

(defun main ()
  (let* ((start (get-internal-real-time))
         (text (read-file-string "input.txt"))
         (grid (load-grid text))
         (blocks (split-blocks grid))
         (p1 (part1 grid blocks))
         (p2 (part2 grid blocks))
         (elapsed-ms (* 1000.0 (/ (- (get-internal-real-time) start)
                                  internal-time-units-per-second))))
    (format t "grand_total=~A quantum_total=~A elapsed_ms=~,3F~%" p1 p2 elapsed-ms)))

(main)
