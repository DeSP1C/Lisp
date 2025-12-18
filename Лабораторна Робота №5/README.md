<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Литвин Максим Ігорович КВ-23</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
<!-- Зазначається загальне завдання на лабораторну роботу -->
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури та/або утиліти для створення записів з таблиць (в залежності
від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів. Значення колонок мають
бути розібрані відповідно до типу даних у них. Наприклад, рядок — це просто
рядок; числові колонки необхідно розібрати як цілі числа або числа з рухомою
крапкою.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці

геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці

6. Написати функцію(-ї) для "красивого" виводу записів таблиці (pretty-print).
## Варіант 12
<!-- Зазначається завдання за варіантом: база даних та тип записів з таблиць -->
- База даних: Космічні апарати
- Тип записів: Асоціативний список
- Таблиці:
- 1. Компанії
- 2. Космічні апарати
- База даних космічних апаратів для зв'язку, дослідження, тощо.

## Лістинг реалізації завдання
```lisp
(defparameter *company-slots* '(:id :name :country :foundation-year :description))
(defparameter *spacecraft-slots* '(:id :name :purpose :company-id :launch-year :mass :description))

(defun make-alist-record (headers values)
  (mapcar (lambda (header value)
            (cons header value))
      headers values))

(defun split-string (string separator)
  (let ((start 0)
        (result nil))
    (loop for end = (position separator string :start start)
          while end
          do (push (subseq string start end) result)
            (setf start (1+ end)))
    (push (subseq string start) result)
    (nreverse result)))

(defun parse-value (slot-key value)
  (let ((clean-value (string-trim '(#\Space #\Tab #\Newline #\Return #\") value)))
    (case slot-key
      ((:id :company-id :foundation-year :launch-year)
       (if (string= clean-value "")
           nil
           (handler-case
               (parse-integer clean-value :junk-allowed nil)
             (error (c)
               (error "ERROR! Slot '~a' expected an integer, received: '~a'. Details: '~a'" slot-key clean-value c)))))
      ((:mass)
       (if (string= clean-value "")
           nil
           (handler-case
               (read-from-string clean-value)
             (error (c)
               (error "ERROR! Slot '~a' expected a float, received: '~a'. Details: '~a'" slot-key clean-value c)))))
      (t
       (if (string= clean-value "") nil clean-value)))))

(defun read-csv-as-alists (filepath)
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (let* ((header-line (read-line stream nil))
           (headers (mapcar (lambda (header)
                              (intern (string-upcase
                                        (substitute #\- #\Space
                                            (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                         header)))
                                      :keyword))
                        (split-string header-line #\,)))
           (rows (loop for line = (read-line stream nil)
                       while line
                       collect (split-string line #\,))))
      (mapcar (lambda (row)
                (let ((alist (make-alist-record headers row)))
                  (mapcar (lambda (pair)
                            (cons (car pair)
                                  (parse-value (car pair) (cdr pair))))
                      alist)))
          rows))))

(defun record-matches-p (record-value filter-value)
  (cond
   ((and (numberp record-value) (numberp filter-value))
     (= record-value filter-value))
   ((and (stringp record-value) (stringp filter-value))
     (string-equal record-value filter-value))
   ((null record-value)
     (null filter-value))
   (t
     (equal (princ-to-string record-value) (princ-to-string filter-value)))))

(defun select (filepath)
  (let ((data (read-csv-as-alists filepath)))
    (lambda (&rest filters &key &allow-other-keys)
      (if filters
          (remove-if-not
              (lambda (item-alist)
                (loop for (filter-key filter-val)
                        on filters by #'cddr
                        always (let* ((record-pair (assoc filter-key item-alist))
                                      (record-value (cdr record-pair)))
                                 (if record-pair
                                     (record-matches-p record-value filter-val)
                                     nil))))
              data)
          data))))

(defun write-selection-to-csv (filepath selection slots &key (overwrite t))
  (with-open-file (stream filepath
                          :direction :output
                          :if-exists (if overwrite :supersede :append)
                          :if-does-not-exist :create)
    (format stream "~{~a~^,~}~%" (mapcar (lambda (s) (string-upcase (symbol-name s))) slots))

    (dolist (alist selection)
      (let ((values (mapcar
                        (lambda (slot)
                          (let ((value (cdr (assoc slot alist))))
                            (cond
                             ((null value) "")
                             ((stringp value) (format nil "~a" value))
                             (t (princ-to-string value)))))
                        slots)))
        (format stream "~{~a~^,~}~%" values)))))

(defun alist-to-hash-table (alist)
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) hash) (cdr pair)))
    hash))

```
### Тестові набори та утиліти
```lisp
;;; Лістинг реалізації утилітних тестових функцій та тестових наборів
(defun print-alist-table (alists slot-names)
  (when (null alists)
        (format t "No records found.~%")
        (return-from print-alist-table))

  (let* ((headers (mapcar (lambda (slot) (string-upcase (symbol-name slot))) slot-names))
         (rows (mapcar (lambda (alist)
                         (mapcar (lambda (slot)
                                   (let ((value (cdr (assoc slot alist))))
                                     (if value (write-to-string value) "")))
                             slot-names))
                   alists))
         (col-widths (loop for i from 0 below (length slot-names)
                           collect (max (length (nth i headers))
                                     (apply #'max
                                       (mapcar (lambda (row) (length (nth i row))) rows))))))
    (loop for header in headers
          for width in col-widths
          do (format t "| ~v@a " width header))
    (format t "|~%")

    (loop for width in col-widths
          do (format t "+~a" (make-string (+ width 2) :initial-element #\-)))
    (format t "+~%")

    (dolist (row rows)
      (loop for cell in row
            for width in col-widths
            do (format t "| ~v@a " width cell))
      (format t "|~%"))))

(defun print-databases (companies-lambda spacecrafts-lambda)
  (flet ((print-detailed-summary (label selection-lambda slots)
                                 (format t "~%~a:~%" label)
                                 (let ((all-items (funcall selection-lambda)))
                                   (print-alist-table all-items slots)
                                   (loop for item in all-items
                                         for i from 1 do
                                           (format t "~%  Record ~a (ALIST):~%" i)
                                           (dolist (slot slots)
                                             (let ((value (cdr (assoc slot item))))
                                               (format t "    ~a = ~a (~a)~%" slot value (type-of value))))))))

    (print-detailed-summary "Companies" companies-lambda *company-slots*)
    (print-detailed-summary "Spacecrafts" spacecrafts-lambda *spacecraft-slots*)))

(defun run-all-filter-tests (companies-lambda spacecrafts-lambda)
  (flet ((run-filter-test (label selection-lambda slots &rest filters)
             (format t "~%~a:~%" label)
             (let ((result (apply selection-lambda filters)))
               (print-alist-table result slots))))

    (run-filter-test "Spacecrafts with company ID=1" spacecrafts-lambda *spacecraft-slots* :COMPANY-ID 1)
    (run-filter-test "Companies from USA" companies-lambda *company-slots* :COUNTRY "USA")
    (run-filter-test "Spacecrafts with mass equal to 150000.0" spacecrafts-lambda *spacecraft-slots* :MASS 150000.0)
    (run-filter-test "Companies founded in 2000 in USA" companies-lambda *company-slots* :FOUNDATION-YEAR 2000 :COUNTRY "USA")
    (run-filter-test "Spacecrafts used for exploration" spacecrafts-lambda *spacecraft-slots* :PURPOSE "Exploration")))

(defun run-all-tests ()
  (let ((companies-lambda (select "companies.csv"))
        (spacecrafts-lambda (select "spacecrafts.csv")))

    (print-databases companies-lambda spacecrafts-lambda)
    (run-all-filter-tests companies-lambda spacecrafts-lambda)

    (format t "~%Converting association list to hash table:~%")
    (let* ((first-company (first (funcall companies-lambda)))
           (hash-table (alist-to-hash-table first-company)))
      (format t "  ALIST: ~a~%" first-company)
      (format t "  HASH-TABLE:~%")
      (maphash (lambda (k v) (format t "    (~a . ~s)~%" k v)) hash-table)
      (format t "~%"))

    (let ((selected-spacecrafts (funcall spacecrafts-lambda :LAUNCH-YEAR 2019)))
      (write-selection-to-csv "selected_spacecrafts.csv" selected-spacecrafts *spacecraft-slots* :overwrite t)
      (format t "Records (spacecrafts launched in 2019) successfully written to file: 'selected_spacecrafts.csv'.~%"))))
```
### companies.csv
```
ID,Name,Country,Foundation Year,Description
1,SpaceX,USA,2002,"Private space company focused on Mars"
2,ESA,France,1975,"European Space Agency for space research"
3,NASA,USA,1958,"A US government agency"
4,JAXA,Japan,2003,"Japan Aerospace Exploration Agency"
5,ISRO,India,1969,"Indian Space Research Organisation"
6,Blue Origin,USA,2000,"Private space company focused on reusable rockets"
```

### spacecrafts.csv
```
ID,Name,Purpose,Company ID,Launch Year,Mass,Description
101,Starlink-1,Communication,1,2019,260,"Satellite internet constellation"
102,Huygens,Exploration,2,2005,318.1,"Lander to Titan"
103,Dragon,Cargo,1,2010,7400,"Reusable cargo spacecraft"
104,Hubble,Exploration,3,1990,11110.0,"Space Telescope"
105,Crew Dragon,Crew,1,2020,13000,"Crew transport vehicle"
106,Starship,Crew/Cargo,1,2024,150000,"Fully reusable super heavy-lift launch system"
107,Hayabusa2,Exploration,4,2014,600.0,"Asteroid sample return mission"
108,Chandrayaan-3,Exploration,5,2023,3900.0,"Moon lander and rover mission"
```

### Тестування
```lisp
;;; Виклик і результат виконання тестів
CL-USER> (run-all-tests)

Spacecrafts with company ID=1:
|  ID |          NAME |         PURPOSE | COMPANY-ID | LAUNCH-YEAR |   MASS |                                     DESCRIPTION |
+-----+---------------+-----------------+------------+-------------+--------+-------------------------------------------------+
| 101 |  "Starlink-1" | "Communication" |          1 |        2019 |    260 |              "Satellite internet constellation" |
| 103 |      "Dragon" |         "Cargo" |          1 |        2010 |   7400 |                     "Reusable cargo spacecraft" |
| 105 | "Crew Dragon" |          "Crew" |          1 |        2020 |  13000 |                        "Crew transport vehicle" |
| 106 |    "Starship" |    "Crew/Cargo" |          1 |        2024 | 150000 | "Fully reusable super heavy-lift launch system" |

Companies from USA:
| ID |          NAME | COUNTRY | FOUNDATION-YEAR |                                         DESCRIPTION |
+----+---------------+---------+-----------------+-----------------------------------------------------+
|  1 |      "SpaceX" |   "USA" |            2002 |             "Private space company focused on Mars" |
|  3 |        "NASA" |   "USA" |            1958 |                            "A US government agency" |
|  6 | "Blue Origin" |   "USA" |            2000 | "Private space company focused on reusable rockets" |

Spacecrafts with mass equal to 150000.0:
|  ID |       NAME |      PURPOSE | COMPANY-ID | LAUNCH-YEAR |   MASS |                                     DESCRIPTION |
+-----+------------+--------------+------------+-------------+--------+-------------------------------------------------+
| 106 | "Starship" | "Crew/Cargo" |          1 |        2024 | 150000 | "Fully reusable super heavy-lift launch system" |

Companies founded in 2000 in USA:
| ID |          NAME | COUNTRY | FOUNDATION-YEAR |                                         DESCRIPTION |
+----+---------------+---------+-----------------+-----------------------------------------------------+
|  6 | "Blue Origin" |   "USA" |            2000 | "Private space company focused on reusable rockets" |

Spacecrafts used for exploration:
|  ID |            NAME |       PURPOSE | COMPANY-ID | LAUNCH-YEAR |    MASS |                      DESCRIPTION |
+-----+-----------------+---------------+------------+-------------+---------+----------------------------------+
| 102 |       "Huygens" | "Exploration" |          2 |        2005 |   318.1 |                "Lander to Titan" |
| 104 |        "Hubble" | "Exploration" |          3 |        1990 | 11110.0 |                "Space Telescope" |
| 107 |     "Hayabusa2" | "Exploration" |          4 |        2014 |   600.0 | "Asteroid sample return mission" |
| 108 | "Chandrayaan-3" | "Exploration" |          5 |        2023 |  3900.0 |  "Moon lander and rover mission" |

Converting association list to hash table:
  ALIST: ((ID . 1) (NAME . SpaceX) (COUNTRY . USA) (FOUNDATION-YEAR . 2002)
          (DESCRIPTION . Private space company focused on Mars))
  HASH-TABLE:
    (ID . 1)
    (NAME . "SpaceX")
    (COUNTRY . "USA")
    (FOUNDATION-YEAR . 2002)
    (DESCRIPTION . "Private space company focused on Mars")

Records (spacecrafts launched in 2019) successfully written to file: 'selected_spacecrafts.csv'.
```