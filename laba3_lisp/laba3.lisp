(defun list-min (lst)
  (cond
    ((null lst) (error "list-min: empty list"))
    ((null (cdr lst)) (car lst))
    (t (let ((tail-min (list-min (cdr lst))))
         (if (< (car lst) tail-min) (car lst) tail-min)))))

(defun remove-first (x lst)
  (cond
    ((null lst) nil)
    ((eql x (car lst)) (cdr lst))
    (t (cons (car lst) (remove-first x (cdr lst))))))

(defun selection-sort-func (lst)
  (if (null lst)
      nil
      (let* ((m (list-min lst))
             (rest (remove-first m lst)))
        (cons m (selection-sort-func rest)))))

(defun selection-sort-imp (lst)
  (let ((head (copy-list lst)))
    (let ((i head))
      (loop while i do
            (let ((min-cell i)
                  (j (cdr i)))
              (loop while j do
                    (when (< (car j) (car min-cell))
                      (setf min-cell j))
                    (setf j (cdr j)))
              (rotatef (car i) (car min-cell))
              (setf i (cdr i)))))
    head))

(defun check-sort (name fn input expected)
  (format t "~:[FAILED~;passed~]  ~a~%"
          (equal (funcall fn input) expected)
          name))

(defun test-selection-sort-func ()
  (check-sort "func test 1" #'selection-sort-func '() '())
  (check-sort "func test 2" #'selection-sort-func '(3 1 2) '(1 2 3))
  (check-sort "func test 3" #'selection-sort-func '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "func test 4" #'selection-sort-func '(2 2 1 3 1) '(1 1 2 2 3))
  (check-sort "func test 5" #'selection-sort-func '(-2 0 -5 3) '(-5 -2 0 3)))

(defun test-selection-sort-imp ()
  (check-sort "imp test 1" #'selection-sort-imp '() '())
  (check-sort "imp test 2" #'selection-sort-imp '(3 1 2) '(1 2 3))
  (check-sort "imp test 3" #'selection-sort-imp '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "imp test 4" #'selection-sort-imp '(2 2 1 3 1) '(1 1 2 2 3))
  (check-sort "imp test 5" #'selection-sort-imp '(-2 0 -5 3) '(-5 -2 0 3)))
