<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Романченко Вікторія Олександрівна КВ-22<p>
<p align="right"><b>Рік</b>: 2025<p>

## Загальне завдання

В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.

1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.

5. Написати функції для конвертування записів у інший тип (в залежності від варіанту):

   1. структури у геш-таблиці
   2. геш-таблиці у асоціативні списки
   3. асоціативні списки у геш-таблиці

6. Написати функцію(-ї) для "красивого" виводу записів таблиці.

## Варіанти завдань 9

База даних: Наукові статті

Тип записів: Асоціативний список

Таблиці: 1. Спеціальності 2. Наукові статті

## Лістинг реалізації

```lisp
(defpackage :lab5
  (:use :cl)
  (:export :check-lab5))

(in-package :lab5)

(defparameter *path-to-files* "D:/LISP/portacle/projects/laba5/")

(defparameter *articles-schema*
  '((:id . :integer) (:title . :string) (:year . :integer) (:specialty-id . :integer) (:journal . :string)))

(defparameter *specialties-schema*
  '((:id . :integer) (:name . :string) (:field . :string)))

(defun get-schema (type)
  (case type
    (:articles *articles-schema*)
    (:specialties *specialties-schema*)))

(defun trim (s) (string-trim '(#\Space #\Tab #\Newline #\Return) s))

(defun parse-val (val type)
  (let ((s (trim val)))
    (if (eq type :integer)
        (parse-integer s :junk-allowed t)
        s)))

(defun split-csv (line)
  (loop for i = 0 then (1+ j)
        as j = (position #\, line :start i)
        collect (subseq line i j)
        while j))

(defun read-csv (filename type)
  (let ((full-path (concatenate 'string *path-to-files* filename))
        (schema (get-schema type)))
    (unless (probe-file full-path)
      (format t "ERROR: File not found: ~A~%" full-path)
      (return-from read-csv nil))
    (with-open-file (s full-path :if-does-not-exist nil)
      (when s
        (read-line s nil)
        (loop for line = (read-line s nil)
              while line
              when (> (length (trim line)) 0)
              collect (loop for v in (split-csv line)
                            for (k . t-type) in schema
                            collect (cons k (parse-val v t-type))))))))

(defun write-csv (filename records type)
  (let ((full-path (concatenate 'string *path-to-files* filename))
        (schema (get-schema type)))
    (with-open-file (s full-path :direction :output :if-exists :supersede)
      (format s "~{~A~^,~}~%" (mapcar #'car schema))
      (dolist (r records)
        (format s "~{~A~^,~}~%"
                (loop for (k . t-type) in schema collect (cdr (assoc k r)))))
      (format s "~%"))))

(defun select (filename type)
  (lambda (&rest args)
    (let ((data (read-csv filename type))
          (filters (loop for (k v) on args by #'cddr collect (list k v))))
      (if filters
          (remove-if-not
           (lambda (row)
             (every (lambda (f) (equalp (cdr (assoc (car f) row)) (cadr f))) filters))
           data)
          data))))


(defun print-table (records)
  (if (null records)
      (format t "Table is empty.~%")
      (let ((headers (mapcar #'car (first records))))

        (format t "~%")
        (dolist (h headers)
          (format t "| ~20a " h))
        (format t "|~%")

        (format t "~A~%" (make-string (* (length headers) 23) :initial-element #\-))

        (dolist (rec records)
          (dolist (h headers)
            (format t "| ~20a " (cdr (assoc h rec))))
          (format t "|~%")))))
```

### Тестові набори та утиліти

```lisp
(defun alist-to-hash (alist)
  (let ((h (make-hash-table :test 'equal)))
    (dolist (p alist) (setf (gethash (car p) h) (cdr p)))
    h))

(defun check-lab5 ()
  (format t "~%--- CHECKING PATH ---~%")
  (format t "Current path: ~A~%" *path-to-files*)

  (format t "~%--- TEST 1: All Articles (Table View) ---~%")
  (print-table (funcall (select "articles.csv" :articles)))

  (format t "~%--- TEST 2: Articles 2023 ---~%")
  (print-table (funcall (select "articles.csv" :articles) :year 2023))

  (format t "~%--- TEST 3: Specialties (ID 2) ---~%")
  (print-table (funcall (select "specialties.csv" :specialties) :id 2))

  (format t "~%--- TEST 4: Hash Table ---~%")
  (let ((res (funcall (select "articles.csv" :articles))))
    (when res
      (let ((h (alist-to-hash (first res))))
        (format t "Title from Hash: ~A~%" (gethash :title h)))))

  (format t "~%--- TEST 5: Writing ---~%")
  (write-csv "output.csv" (funcall (select "articles.csv" :articles) :year 2023) :articles)
  (format t "File saved to: ~Aoutput.csv~%" *path-to-files*))
```

### Тестування

```
CL-USER> (lab5:check-lab5)

--- CHECKING PATH ---
Current path: D:/LISP/portacle/projects/laba5/

--- TEST 1: All Articles (Table View) ---

| ID                   | TITLE                | YEAR                 | SPECIALTY-ID         | JOURNAL              |
-------------------------------------------------------------------------------------------------------------------
| 10                   | Introduction to Lisp | 2022                 | 1                    | Code World           |
| 20                   | Quantum Mechanics    | 2023                 | 2                    | Physics Today        |
| 30                   | Advanced Algorithms  | 2022                 | 1                    | CS Weekly            |
| 40                   | Black Holes          | 2023                 | 2                    | Science Journal      |

--- TEST 2: Articles 2023 ---

| ID                   | TITLE                | YEAR                 | SPECIALTY-ID         | JOURNAL              |
-------------------------------------------------------------------------------------------------------------------
| 20                   | Quantum Mechanics    | 2023                 | 2                    | Physics Today        |
| 40                   | Black Holes          | 2023                 | 2                    | Science Journal      |

--- TEST 3: Specialties (ID 2) ---

| ID                   | NAME                 | FIELD                |
---------------------------------------------------------------------
| 2                    | Physics              | Science              |

--- TEST 4: Hash Table ---
Title from Hash: Introduction to Lisp

--- TEST 5: Writing ---
File saved to: D:/LISP/portacle/projects/laba5/output.csv
NIL
```
