#!/usr/bin/env sbcl --script

(defun read-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line collect line)))

(defun elapsed-ms (start end)
  (* 1000.0 (/ (- end start) internal-time-units-per-second)))

(defun load-grid (lines)
  (let ((grid '())
        (sr -1)
        (sc -1))
    (loop for r from 0
          for line in lines do
            (push line grid)
            (let ((pos (position #\S line)))
              (when pos
                (setf sr r
                      sc pos))))
    (when (< sr 0) (error "missing S"))
    (values (nreverse grid) sr sc)))

(defun part1 (grid sr sc)
  (let* ((height (length grid))
         (width (length (first grid)))
         (active (list sc))
         (splits 0))
    (loop for r from sr below height do
      (let ((next '())
            (seen (make-hash-table))
            (queue-front (copy-list active))
            (queue-back '()))
        (loop
          (when (and (null queue-front) (null queue-back))
            (return))
          (when (null queue-front)
            (setf queue-front (nreverse queue-back))
            (setf queue-back '()))
          (let ((c (pop queue-front)))
            (unless (gethash c seen)
              (setf (gethash c seen) t)
              (let ((cell (char (nth r grid) c)))
                (if (char= cell #\^)
                    (progn
                      (incf splits)
                      (when (> c 0) (push (1- c) queue-back))
                      (when (< (1+ c) width) (push (1+ c) queue-back)))
                    (push c next))))))
        (setf active next)
        (when (null active) (return))))
    splits))

(defun part2 (grid sr sc)
  (let* ((height (length grid))
         (width (length (first grid)))
         (active (make-hash-table)))
    (setf (gethash sc active) 1)
    (loop for r from sr below height do
      (let ((next (make-hash-table)))
        (maphash
         (lambda (c count)
           (let ((cell (char (nth r grid) c)))
             (if (char= cell #\^)
                 (progn
                   (when (> c 0)
                     (incf (gethash (1- c) next 0) count))
                   (when (< (1+ c) width)
                     (incf (gethash (1+ c) next 0) count)))
                 (incf (gethash c next 0) count))))
         active)
        (setf active next)
        (when (= (hash-table-count active) 0) (return))))
    (let ((total 0))
      (maphash (lambda (_ v) (incf total v)) active)
      total)))

(defun main ()
  (let ((lines (read-lines "input.txt")))
    (multiple-value-bind (grid sr sc) (load-grid lines)
      (let* ((t0 (get-internal-run-time))
             (p1 (part1 grid sr sc))
             (p2 (part2 grid sr sc))
             (t1 (get-internal-run-time))
             (elapsed (elapsed-ms t0 t1)))
        (format t "splits=~A timelines=~A elapsed_ms=~,3f~%" p1 p2 elapsed)))))

(main)
