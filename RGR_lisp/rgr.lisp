(defun fi (i)
  (labels ((f (k)
             (cond ((= k 1) 1d0)
                   ((= k 11) 1d0)
                   ((and (>= k 2) (<= k 10)) (* (f (1- k)) (* (log k) 8d0)))
                   ((and (>= k 12) (<= k 20)) (* (f (1- k)) (/ (log k) 8d0)))
                   (t (error "i out of range: ~A" k)))))
    (f i)))

(defun build-table ()
  (append
   (loop for i from 2 to 10 collect (list i (fi i)))
   (loop for i from 12 to 20 collect (list i (fi i)))))

(defun show-table ()
  (format t "~%  i | Fi(i)~%--------------------~%")
  (dolist (row (build-table))
    (format t "~3D | ~,8,2E~%" (first row) (second row))))

(defun run-tests ()
  (format t "~%Tests:~%")
  (format t "F1 = 1: ~A~%" (= (fi 1) 1d0))
  (format t "F11 = 1: ~A~%" (= (fi 11) 1d0))

  (let ((up    (loop for i from 2 to 10 collect (fi i)))
        (down  (loop for i from 12 to 20 collect (fi i))))
    (format t "2..10 increasing: ~A~%"
            (every #'< up (rest up)))
    (format t "12..20 decreasing: ~A~%"
            (every #'> down (rest down)))))

