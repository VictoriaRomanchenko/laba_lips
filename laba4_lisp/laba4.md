<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Романченко Вікторія Олександрівна</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання

Завдання складається з двох частин:

1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
   роботи 3 з такими змінами:

   - використати функції вищого порядку для роботи з послідовностями (де/якщо
     це доречно, в разі, якщо функції вищого порядку не були використані при
     реалізації л.р. No3);
   - додати до інтерфейсу функції (та використання в реалізації) два ключових
     параметра: key та test , що працюють аналогічно до того, як працюють
     параметри з такими назвами в функціях, що працюють з послідовностями (р.
     12). При цьому key має виконатись мінімальну кількість разів.

2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
   варіантом (див. п 4.1.2). Використання псевдофункцій не забороняється, але, за
   можливості, має бути зменшене до необхідного мінімуму.

## Варіант першої частини 1

Алгоритм сортування вибором за незменшенням.

## Лістинг реалізації першої частини завдання

```lisp
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
```

### Тестові набори та утиліти першої частини

```lisp
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
```

### Тестування першої частини

```lisp
CL-USER> (test-duplicate-elements-fn)
passed  dup test 1 (all x2)
passed  dup test 2 (even x2)
passed  dup test 3 (n=1 identity)
passed  dup test 4 (>2 x3)
passed  dup test 5 (symbols a,c x2)
NIL
```

## Варіант другої частини 9

Написати функцію duplicate-elements-fn , яка має один основний параметр n та один
ключовий параметр — функцію duplicate-p . duplicate-elements-fn має повернути
функцію, яка при застосуванні в якості першого аргументу mapcan робить наступне:
кожен елемент списка-аргумента mapcan , для якого функція duplicate-p повертає
значення t (або не nil ), дублюється n разів. Якщо користувач не передав функцію
duplicate-p у duplicate-elements-fn , тоді дублюються всі елементи вхідного списку.

## Лістинг реалізації другої частини завдання

```lisp
(defun duplicate-elements-fn (n &key duplicate-p)
  (let ((pred (or duplicate-p (constantly t))))
    (lambda (x)
      (if (funcall pred x)
          (make-list n :initial-element x)
          (list x)))))
```

### Тестові набори та утиліти другої частини

```lisp
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
```

### Тестування другої частини

```lisp
CL-USER> (test-selection-sort-func)
passed  func test 1 (empty)
passed  func test 2 (nums)
passed  func test 3 (dups+neg)
FAILED  func test 4 (:key length)
passed  func test 5 (:test >)
NIL
```
