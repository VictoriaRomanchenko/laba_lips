(defun singletonize (lst)
  (if (null lst) nil (cons (list (car lst)) (singletonize (cdr lst)))))

(defun merge-lists-spinning-pairs (xs ys)
  (cond
    ((null xs) (singletonize ys))
    ((null ys) (singletonize xs))
    (t (cons (list (car xs) (car ys))
             (merge-lists-spinning-pairs (cdr ys) (cdr xs))))))
(defun check-mlsp (name input-x input-y expected)
  (format t "~:[FAILED~;passed~]  merge-lists-spinning-pairs ~a~%"
          (equal (merge-lists-spinning-pairs input-x input-y) expected)
          name))

(defun test-merge-lists-spinning-pairs ()
  (check-mlsp "test 1"
              '(1 2 3 4 5) '(a b c d)
              '((1 A) (B 2) (3 C) (D 4) (5)))
  (check-mlsp "test 2"
              '(x y) '(1 2 3 4)
              '((X 1) (2 Y) (3) (4)))
  (check-mlsp "test 3"
              '() '(p q r)
              '((P) (Q) (R))))

(defun member-p (x lst)
  (and lst (or (eql x (car lst)) (member-p x (cdr lst)))))

(defun list-set-intersect-p (a b)
  (cond
    ((null a) nil)
    ((member-p (car a) b) t)
    (t (list-set-intersect-p (cdr a) b))))

(defun check-intersect (name a b expected)
  (format t "~:[FAILED~;passed~]  list-set-intersect-p ~a~%"
          (eq (list-set-intersect-p a b) expected)
          name))

(defun test-list-set-intersect-p ()
  (check-intersect "test 1" '(1 2 3) '(4 5 6) nil)
  (check-intersect "test 2" '(1 2 3) '(3 4 5) t)
  (check-intersect "test 3" '(a a b) '(c b d) t))

