(defun selection-sort-func (lst &key (key #'identity) (test #'<))
  (labels
      ((decorate (xs)                     
         (mapcar (lambda (x) (cons (funcall key x) x)) xs))
       (undecorate (pairs)                
         (mapcar #'cdr pairs))
       (min-pair (pairs)                  
         (reduce (lambda (a b)
                   (if (funcall test (car a) (car b)) a b))
                 pairs))
       (remove-first-pair (target pairs)
         (let ((seen nil))
           (mapcan (lambda (p)
                     (if (and (not seen) (equal p target))
                         (progn (setf seen t) nil)
                         (list p)))
                   pairs)))
       (sort-pairs (pairs)
         (if (endp pairs)
             nil
             (let* ((m (min-pair pairs))
                    (rest (remove-first-pair m pairs)))
               (cons m (sort-pairs rest))))))
    (if (endp lst) nil (undecorate (sort-pairs (decorate lst))))))

(defun duplicate-elements-fn (n &key duplicate-p)
  (let ((pred (or duplicate-p (constantly t))))
    (lambda (x)
      (if (funcall pred x)
          (make-list n :initial-element x)
          (list x)))))

(defun check-sort (name fn input expected)
  (format t "~:[FAILED~;passed~]  ~a~%"
          (equal (funcall fn input) expected)
          name))

(defun test-selection-sort-func ()
  (check-sort "func test 1 (empty)"
              #'selection-sort-func
              '()
              '())
  (check-sort "func test 2 (nums)"
              #'selection-sort-func
              '(3 1 2)
              '(1 2 3))
  (check-sort "func test 3 (dups+neg)"
              #'selection-sort-func
              '(2 -1 2 0 -1)
              '(-1 -1 0 2 2))
  (check-sort "func test 4 (:key length)"
              (lambda (xs) (selection-sort-func xs :key #'length))
              '("bb" "a" "cccc" "aa")
              '("a" "bb" "aa" "cccc"))
  (check-sort "func test 5 (:test >)"
              (lambda (xs) (selection-sort-func xs :test #'>))
              '(5 1 4 2 3)
              '(5 4 3 2 1)))

(defun check-dup (name n &key duplicate-p input expected)
  (format t "~:[FAILED~;passed~]  ~a~%"
          (equal (mapcan (duplicate-elements-fn n :duplicate-p duplicate-p) input)
                 expected)
          name))

(defun test-duplicate-elements-fn ()
  (check-dup "dup test 1 (all x2)"
             2 :input '(1 2 3) :expected '(1 1 2 2 3 3))
  (check-dup "dup test 2 (even x2)"
             2 :duplicate-p #'evenp
             :input '(1 2 3 4 5)
             :expected '(1 2 2 3 4 4 5))
  (check-dup "dup test 3 (n=1 identity)"
             1 :duplicate-p #'oddp
             :input '(1 2 3 4)
             :expected '(1 2 3 4))
  (check-dup "dup test 4 (>2 x3)"
             3 :duplicate-p (lambda (x) (> x 2))
             :input '(1 2 3 4 2)
             :expected '(1 2 3 3 3 4 4 4 2))
  (check-dup "dup test 5 (symbols a,c x2)"
             2 :duplicate-p (lambda (x) (member x '(a c) :test #'eq))
             :input '(a b c d)
             :expected '(a a b c c d)))

