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

