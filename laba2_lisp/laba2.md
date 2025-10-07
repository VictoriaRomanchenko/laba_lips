<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Романченко Вікторія Олександрівна КВ-22</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання

Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за
можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно
реалізувати, задаються варіантом (п. 2.1.1). Вимоги до функцій:

1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового
   списку, а не зміни наявного (вхідного).

2. Не допускається використання функцій вищого порядку чи стандартних функцій
   для роботи зі списками, що не наведені в четвертому розділі навчального
   посібника.

3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції
   в якості аргументів.

4. Не допускається використання псевдофункцій (деструктивного підходу).

5. Не допускається використання циклів.

Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (див. п. 2.3).

## Варіант 6

Написати функцію merge-lists-spinning-pairs , яка групує відповідні елементи
двох списків, почергово змінюючи їх взаємне розташування в групі.

Написати предикат list-set-intersect-p , який визначає чи перетинаються дві
множини, задані списками атомів, чи ні.

## Лістинг функції merge-lists-spinning-pairs

```lisp
(defun merge-lists-spinning-pairs (xs ys)
  (merge-lists-spinning-pairs-aux xs ys nil))

(defun merge-lists-spinning-pairs-aux (xs ys flip)
  (cond
    ((and (null xs) (null ys)) nil)
    ((and xs ys)
     (cons (if flip
               (list (car ys) (car xs))
               (list (car xs) (car ys)))
           (merge-lists-spinning-pairs-aux (cdr xs) (cdr ys) (not flip))))
    (xs  (cons (list (car xs))
               (merge-lists-spinning-pairs-aux (cdr xs) nil flip)))
    (ys  (cons (list (car ys))
               (merge-lists-spinning-pairs-aux nil (cdr ys) flip)))))
```

### Тестові набори та утиліти

```lisp
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

```

## Лістинг функції list-set-intersect-p

```lisp
(defun member-p (x lst)
  (and lst (or (eql x (car lst)) (member-p x (cdr lst)))))

(defun list-set-intersect-p (a b)
  (cond
    ((null a) nil)
    ((member-p (car a) b) t)
    (t (list-set-intersect-p (cdr a) b))))
```

### Тестові набори та утиліти

```lisp
(defun check-intersect (name a b expected)
  (format t "~:[FAILED~;passed~]  list-set-intersect-p ~a~%"
          (eq (list-set-intersect-p a b) expected)
          name))

(defun test-list-set-intersect-p ()
  (check-intersect "test 1" '(1 2 3) '(4 5 6) nil)
  (check-intersect "test 2" '(1 2 3) '(3 4 5) t)
  (check-intersect "test 3" '(a a b) '(c b d) t))

```

### Тестування

```lisp
CL-USER> (test-list-set-intersect-p)
passed  list-set-intersect-p test 1
passed  list-set-intersect-p test 2
passed  list-set-intersect-p test 3
NIL
CL-USER> (test-merge-lists-spinning-pairs)
passed  merge-lists-spinning-pairs test 1
passed  merge-lists-spinning-pairs test 2
passed  merge-lists-spinning-pairs test 3
NIL

```
