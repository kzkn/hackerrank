(defparameter *test-input* "5
2 2
-----
-----
--m--
-----
-p---")

(defun read-bot-position ()
  (let* ((xy (read-line))
         (space (position #\space xy)))
    `(,(parse-integer xy :start (1+ space))
      ,(parse-integer xy :end space))))

(defun find-princess ()
  (loop for y = 0 then (1+ y)
        for s = (read-line) then (read-line)
        do (let ((x (position #\p s)))
             (when x
               (return-from find-princess `(,x ,y)))))
  (error "no-princess"))

(defun main ()
  (read-line) ;; skip first line
  (let ((bot-position (read-bot-position))
        (princess-position (find-princess)))
    (destructuring-bind (px py bx by) (append princess-position bot-position)
      (format t "~@[~A~%~]"
              (cond ((< py by) 'up)
                    ((> py by) 'down)
                    ((< px bx) 'left)
                    ((> px bx) 'right)
                    (t nil))))))

(main)
