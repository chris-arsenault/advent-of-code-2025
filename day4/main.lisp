#!/usr/bin/env sbcl --script

(defun read-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line collect line)))

(defun elapsed-ms (start end)
  (* 1000.0 (/ (- end start) internal-time-units-per-second)))

(defparameter *neighbors*
  '((-1 . -1) (-1 . 0) (-1 . 1)
    (0 . -1) (0 . 1)
    (1 . -1) (1 . 0) (1 . 1)))

(defun parse-grid (lines)
  (let ((rolls '()))
    (loop for r from 0
          for line in lines do
            (loop for c from 0 below (length line) do
              (when (char= (char line c) #\@)
                (push (cons r c) rolls))))
    rolls))

(defun rolls-set (rolls)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (p rolls)
      (setf (gethash p ht) t))
    ht))

(defun neighbor-counts (rolls)
  (let* ((set (rolls-set rolls))
         (counts (make-hash-table :test 'equal)))
    (dolist (pos rolls)
      (destructuring-bind (r . c) pos
        (let ((cnt 0))
          (dolist (d *neighbors*)
            (let ((p (cons (+ r (car d)) (+ c (cdr d)))))
              (when (gethash p set) (incf cnt))))
          (setf (gethash pos counts) cnt))))
    counts))

(defun part1 (rolls)
  (let ((counts (neighbor-counts rolls))
        (total 0))
    (maphash (lambda (_ cnt)
               (declare (ignore _))
               (when (< cnt 4) (incf total)))
             counts)
    total))

(defun part2 (rolls)
  (let* ((set (rolls-set rolls))
         (counts (neighbor-counts rolls))
         (removed (make-hash-table :test 'equal))
         (queue (make-array (length rolls) :adjustable t :fill-pointer 0)))
    (maphash (lambda (pos cnt)
               (when (< cnt 4)
                 (vector-push-extend pos queue)))
             counts)
    (loop while (> (fill-pointer queue) 0) do
      (let ((pos (vector-pop queue)))
        (unless (gethash pos removed)
          (setf (gethash pos removed) t)
          (remhash pos set)
          (destructuring-bind (r . c) pos
            (dolist (d *neighbors*)
              (let ((nbr (cons (+ r (car d)) (+ c (cdr d)))))
                (when (gethash nbr set)
                  (decf (gethash nbr counts))
                  (when (< (gethash nbr counts) 4)
                    (vector-push-extend nbr queue)))))))))
    (hash-table-count removed)))

(defun main ()
  (let* ((lines (read-lines "input.txt"))
         (rolls (parse-grid lines))
         (t0 (get-internal-real-time))
         (p1 (part1 rolls))
         (p2 (part2 rolls))
         (t1 (get-internal-real-time))
         (elapsed (elapsed-ms t0 t1)))
    (format t "accessible=~A removable_total=~A elapsed_ms=~,3f~%" p1 p2 elapsed)))

(main)
