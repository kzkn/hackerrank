(defparameter *test-input* "0 0
b---d
-d--d
--dd-
--d--
----d")

(defun distance (p1 p2)
  (flet ((%distance (a b) (abs (- a b))))
    (let ((distance-x (%distance (first p1) (first p2)))
          (distance-y (%distance (second p1) (second p2))))
      (sqrt (+ (expt distance-x 2) (expt distance-y 2))))))

(defun position-most-nearest-dirty (position dirties)
  (first (sort (copy-list dirties)
               #'<
               :key (lambda (dirty-pos)
                      (distance position dirty-pos)))))

(defun collect-dirty-positions (area)
  (loop for s in area
        for y from 0
        append (loop for c across s
                     for x from 0
                     when (char= c #\d)
                     collect `(,x ,y))))

(defun read-bot-position ()
  (let* ((xy (read-line))
         (space (position #\space xy)))
    `(,(parse-integer xy :start (1+ space))
       ,(parse-integer xy :end space))))

(defun step-to (from to)
  (let ((x1 (first from)) (y1 (second from))
        (x2 (first to))   (y2 (second to)))
    (format t "~A~%"
            (cond ((< y1 y2) 'down)
                  ((> y1 y2) 'up)
                  ((< x1 x2) 'right)
                  ((> x1 x2) 'left)))))

(defun main ()
  (let* ((position (read-bot-position))
         (dirties (collect-dirty-positions
                   (loop for s = (read-line *standard-input* nil nil)
                      while s
                      collect s)))
         (nearest (position-most-nearest-dirty position dirties)))
    (if (equal nearest position)
        (format t "CLEAN~%")
        (step-to position nearest))))

(main)
