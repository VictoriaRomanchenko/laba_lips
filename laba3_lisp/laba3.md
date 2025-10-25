<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Функціональний і імперативний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Романченко Вікторія Олександрівна КВ-22</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання

Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.

1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
   конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
   списку. Не допускається використання: псевдо-функцій, деструктивних операцій,
   циклів . Також реалізована функція не має бути функціоналом (тобто приймати на
   вхід функції в якості аргументів).

2. Імперативний варіант реалізації має базуватись на використанні циклів і
   деструктивних функцій (псевдофункцій). Не допускається використання функцій
   вищого порядку або функцій для роботи зі списками/послідовностями, що
   використовуються як функції вищого порядку. Тим не менш, оригінальний список
   цей варіант реалізації також не має змінювати, тому перед виконанням
   деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
   Також реалізована функція не має бути функціоналом (тобто приймати на вхід
   функції в якості аргументів).

## Варіант 1

Алгоритм сортування вибором за незменшенням.

## Лістинг функції з використанням конструктивного підходу

```lisp
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
```

### Тестові набори та утиліти

```lisp
(defun test-selection-sort-func ()
  (check-sort "func test 1" #'selection-sort-func '() '())
  (check-sort "func test 2" #'selection-sort-func '(3 1 2) '(1 2 3))
  (check-sort "func test 3" #'selection-sort-func '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "func test 4" #'selection-sort-func '(2 2 1 3 1) '(1 1 2 2 3))
  (check-sort "func test 5" #'selection-sort-func '(-2 0 -5 3) '(-5 -2 0 3)))
```

### Тестування

```lisp
CL-USER> (test-selection-sort-func)
passed  func test 1
passed  func test 2
passed  func test 3
passed  func test 4
passed  func test 5
NIL
```

## Лістинг функції з використанням деструктивного підходу

```lisp
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
```

### Тестові набори та утиліти

```lisp
(defun test-selection-sort-imp ()
  (check-sort "imp test 1" #'selection-sort-imp '() '())
  (check-sort "imp test 2" #'selection-sort-imp '(3 1 2) '(1 2 3))
  (check-sort "imp test 3" #'selection-sort-imp '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "imp test 4" #'selection-sort-imp '(2 2 1 3 1) '(1 1 2 2 3))
  (check-sort "imp test 5" #'selection-sort-imp '(-2 0 -5 3) '(-5 -2 0 3)))
```

### Тестування

```lisp
CL-USER> (test-selection-sort-imp)
passed  imp test 1
passed  imp test 2
passed  imp test 3
passed  imp test 4
passed  imp test 5
NIL
```
