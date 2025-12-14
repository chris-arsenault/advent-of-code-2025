#!/usr/bin/env sbcl --script

(defun read-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line collect line)))

(defun elapsed-ms (start end)
  (* 1000.0 (/ (- end start) internal-time-units-per-second)))

(defun normalize-cells (cells)
  (let ((min-x (apply #'min (mapcar #'car cells)))
        (min-y (apply #'min (mapcar #'cdr cells))))
    (mapcar (lambda (p) (cons (- (car p) min-x) (- (cdr p) min-y))) cells)))

(defun rotate-cells (cells)
  (let ((max-x (apply #'max (mapcar #'car cells))))
    (mapcar (lambda (p) (cons (cdr p) (- max-x (car p)))) cells)))

(defun flip-cells (cells)
  (let ((max-x (apply #'max (mapcar #'car cells))))
    (mapcar (lambda (p) (cons (- max-x (car p)) (cdr p))) cells)))

(defun orientations (cells)
  (let ((seen (make-hash-table :test 'equal))
        (forms '()))
    (labels ((add-form (form)
               (let* ((norm (normalize-cells form))
                      (norm-sorted (sort norm (lambda (a b)
                                                (if (= (car a) (car b))
                                                    (< (cdr a) (cdr b))
                                                    (< (car a) (car b)))))))
                 (unless (gethash norm-sorted seen)
                   (setf (gethash norm-sorted seen) t)
                   (push norm-sorted forms)))))
      (let ((cur cells))
        (dotimes (_ 4)
          (add-form cur)
          (add-form (flip-cells cur))
          (setf cur (rotate-cells cur)))))
    forms))

(defun parse-shapes-and-regions (lines)
  (let* ((split (position-if (lambda (ln) (and (search "x" ln) (search ":" ln))) lines))
         (shape-lines (if split (subseq lines 0 split) lines))
         (region-lines (if split (subseq lines split) '()))
         (shapes '())
         (regions '()))
    ;; parse shapes
    (let ((current '())
          (is-collecting nil))
      (dolist (ln shape-lines)
        (let ((trim (string-trim '(#\Space #\Tab #\Return #\Newline) ln)))
          (cond
            ((string= trim "") nil)
            ((and (> (length trim) 0)
                  (char= (char trim (1- (length trim))) #\:))
             (when is-collecting
               (let ((cells '()))
                 (loop for y from 0
                       for row in (nreverse current) do
                         (loop for x from 0 below (length row) do
                           (when (char= (char row x) #\#)
                             (push (cons x y) cells))))
                 (let* ((norm (normalize-cells cells))
                        (forms (orientations norm))
                        (area (length norm))
                        (parity (reduce #'+ norm :key (lambda (p) (if (evenp (+ (car p) (cdr p))) 1 -1)))))
                   (push (list forms area parity) shapes))))
             (setf current '())
             (setf is-collecting t))
            (is-collecting
             (push trim current)))))
    (when current
      (let ((cells '()))
        (loop for y from 0
              for row in (nreverse current) do
                (loop for x from 0 below (length row) do
                  (when (char= (char row x) #\#)
                    (push (cons x y) cells))))
        (let* ((norm (normalize-cells cells))
               (forms (orientations norm))
               (area (length norm))
               (parity (reduce #'+ norm :key (lambda (p) (if (evenp (+ (car p) (cdr p))) 1 -1)))))
          (push (list forms area parity) shapes)))))
    (setf shapes (nreverse shapes))
    ;; parse regions
    (dolist (ln region-lines)
      (let ((trim (string-trim '(#\Space #\Tab #\Return #\Newline) ln)))
        (when (and (search "x" trim) (search ":" trim))
          (let* ((colon (position #\: trim))
                 (size (subseq trim 0 colon))
                 (counts-str (string-trim '(#\Space) (subseq trim (1+ colon))))
                 (xpos (position #\x size))
                 (w (parse-integer size :end xpos))
                 (h (parse-integer size :start (1+ xpos)))
                 (counts '())
                 (start 0))
            (loop for i from 0 to (length counts-str) do
              (when (or (= i (length counts-str)) (char= (char counts-str i) #\Space))
                (let ((part (string-trim '(#\Space) (subseq counts-str start i))))
                  (when (> (length part) 0)
                    (push (parse-integer part) counts)))
                (setf start (1+ i))))
            (push (list w h (nreverse counts)) regions)))))
    (values shapes (nreverse regions))))

;; Dancing Links (DLX) data structure using defstruct
(defstruct dlx-node
  (left nil)
  (right nil)
  (up nil)
  (down nil)
  (column nil)
  (row-id -1)
  (size 0)
  (name nil))

(defun dlx-build (num-cols rows)
  (let* ((root (make-dlx-node :name 'root))
         (headers (make-array num-cols)))
    (setf (dlx-node-left root) root)
    (setf (dlx-node-right root) root)
    (setf (dlx-node-up root) root)
    (setf (dlx-node-down root) root)
    ;; Create column headers
    (dotimes (i num-cols)
      (let ((h (make-dlx-node :name i :size 0)))
        (setf (aref headers i) h)
        (setf (dlx-node-column h) h)
        (setf (dlx-node-up h) h)
        (setf (dlx-node-down h) h)
        ;; Link into header row
        (let ((prev (dlx-node-left root)))
          (setf (dlx-node-right prev) h)
          (setf (dlx-node-left h) prev)
          (setf (dlx-node-right h) root)
          (setf (dlx-node-left root) h))))
    ;; Add rows
    (loop for row-idx from 0
          for row in rows do
      (let ((first nil) (prev nil))
        (dolist (col-idx row)
          (let* ((col-header (aref headers col-idx))
                 (node (make-dlx-node :column col-header :row-id row-idx)))
            (incf (dlx-node-size col-header))
            ;; Vertical link
            (let ((up-node (dlx-node-up col-header)))
              (setf (dlx-node-down up-node) node)
              (setf (dlx-node-up node) up-node)
              (setf (dlx-node-down node) col-header)
              (setf (dlx-node-up col-header) node))
            ;; Horizontal link
            (if first
                (progn
                  (setf (dlx-node-right prev) node)
                  (setf (dlx-node-left node) prev))
                (progn
                  (setf first node)
                  (setf (dlx-node-left node) node)))
            (setf prev node)))
        (when first
          (setf (dlx-node-right prev) first)
          (setf (dlx-node-left first) prev))))
    root))

(defun dlx-cover (col)
  (setf (dlx-node-right (dlx-node-left col)) (dlx-node-right col))
  (setf (dlx-node-left (dlx-node-right col)) (dlx-node-left col))
  (loop for row = (dlx-node-down col) then (dlx-node-down row)
        until (eq row col) do
    (loop for node = (dlx-node-right row) then (dlx-node-right node)
          until (eq node row) do
      (setf (dlx-node-down (dlx-node-up node)) (dlx-node-down node))
      (setf (dlx-node-up (dlx-node-down node)) (dlx-node-up node))
      (decf (dlx-node-size (dlx-node-column node))))))

(defun dlx-uncover (col)
  (loop for row = (dlx-node-up col) then (dlx-node-up row)
        until (eq row col) do
    (loop for node = (dlx-node-left row) then (dlx-node-left node)
          until (eq node row) do
      (incf (dlx-node-size (dlx-node-column node)))
      (setf (dlx-node-down (dlx-node-up node)) node)
      (setf (dlx-node-up (dlx-node-down node)) node)))
  (setf (dlx-node-right (dlx-node-left col)) col)
  (setf (dlx-node-left (dlx-node-right col)) col))

(defun dlx-search (root)
  (when (eq (dlx-node-right root) root)
    (return-from dlx-search t))
  ;; Choose column with minimum size (MRV heuristic)
  (let ((best nil) (best-size most-positive-fixnum))
    (loop for col = (dlx-node-right root) then (dlx-node-right col)
          until (eq col root) do
      (when (< (dlx-node-size col) best-size)
        (setf best col best-size (dlx-node-size col))))
    (when (zerop best-size) (return-from dlx-search nil))
    (dlx-cover best)
    (loop for row = (dlx-node-down best) then (dlx-node-down row)
          until (eq row best) do
      (loop for node = (dlx-node-right row) then (dlx-node-right node)
            until (eq node row) do
        (dlx-cover (dlx-node-column node)))
      (when (dlx-search root)
        (return-from dlx-search t))
      (loop for node = (dlx-node-left row) then (dlx-node-left node)
            until (eq node row) do
        (dlx-uncover (dlx-node-column node))))
    (dlx-uncover best)
    nil))

(defun exact-cover? (columns rows)
  (let ((root (dlx-build (length columns) rows)))
    (dlx-search root)))

(defun solve-region (w h shapes counts)
  (let* ((needed-area (loop for shp in shapes
                            for c in counts sum (* (second shp) c)))
         (board-area (* w h)))
    (when (> needed-area board-area) (return-from solve-region nil))
    (let* ((piece-cols (reduce #'+ counts))
           (cell-cols board-area))
      (when (or (> cell-cols 400) (> piece-cols 60))
        (return-from solve-region t))
      (let ((col-offsets '())
            (acc 0))
        (dolist (c counts)
          (push acc col-offsets)
          (incf acc c))
        (setf col-offsets (nreverse col-offsets))
        (let ((rows '()))
          (loop for s-idx from 0 below (length shapes) do
            (let* ((shape (nth s-idx shapes))
                   (forms (first shape))
                   (copies (nth s-idx counts))
                   (offset (nth s-idx col-offsets)))
              (dotimes (copy copies)
                (let ((piece-col (+ offset copy)))
                  (dolist (form forms)
                    (let* ((max-x (apply #'max (mapcar #'car form)))
                           (max-y (apply #'max (mapcar #'cdr form))))
                      (loop for y from 0 to (- h max-y 1) do
                        (loop for x from 0 to (- w max-x 1) do
                          (let ((cols (list piece-col)))
                            (dolist (cell form)
                              (let* ((cx (+ x (car cell)))
                                     (cy (+ y (cdr cell)))
                                     (cell-col (+ piece-cols (* cy w) cx)))
                                (push cell-col cols)))
                            (push (nreverse cols) rows))))))))))
          (let* ((total-cols (+ piece-cols cell-cols))
                 (columns (loop for i from 0 below total-cols collect i)))
            (exact-cover? columns rows)))))))

(defun solve (lines)
  (multiple-value-bind (shapes regions) (parse-shapes-and-regions lines)
    (let ((fits 0))
      (dolist (reg regions)
        (destructuring-bind (w h counts) reg
          (when (solve-region w h shapes counts)
            (incf fits))))
      fits)))

(defun main ()
  (let* ((lines (read-lines "input.txt"))
         (t0 (get-internal-run-time))
         (ans (solve lines))
         (t1 (get-internal-run-time))
         (elapsed (elapsed-ms t0 t1)))
    (format t "regions_that_fit=~A elapsed_ms=~,3f~%" ans elapsed)))

(main)
