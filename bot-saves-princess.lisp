(defun find-princess ()
  (loop for y = 0 then (1+ y)
        for s = (read-line) then (read-line)
        do (let ((x (position #\p s)))
             (when x
               (return-from find-princess `(,x ,y)))))
  (error "no-princess"))

(defun move (direction count)
  (loop repeat count do (print direction)))

(defun main ()
  (let* ((n (parse-integer (read-line)))
         (princess-position (find-princess))
         (bot-position `(,(floor (/ n 2)) ,(floor (/ n 2)))))
    (destructuring-bind (px py bx by) (append princess-position bot-position)
      ;; up or down
      (cond ((> py by)
             (move 'down (- py by)))
            ((< py by)
             (move 'up (- by py))))
      ;; left or right
      (cond ((> px bx)
             (move 'right (- px bx)))
            ((< px bx)
             (move 'left (- bx px)))))))

(main)
