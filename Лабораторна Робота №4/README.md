<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Литвин Максим Ігорович КВ-23</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де/якщо
це доречно, в разі, якщо функції вищого порядку не були використані при
реалізації л.р. №3);
додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями (р.
12). При цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдофункцій не забороняється, але, за
можливості, має бути зменшене до необхідного мінімуму.

## Варіант першої частини 12
Алгоритм сортування обміном №1 (без оптимізацій) за незменшенням.

## Лістинг реалізації першої частини завдання
```lisp
(defun bubble-sort-func (lst &key (key #'identity) (test #'<))
  (let ((lst-with-keys (mapcar (lambda (x) (cons x (funcall key x))) lst)))
    (labels ((bubble-pass-inner (lst i r)
                                (cond
                                 ((>= i r) lst)
                                 ((null (cdr lst)) lst)
                                 (t (let ((a (car lst))
                                          (b (cadr lst)))
                                      (if (funcall test (cdr b) (cdr a))
                                          (cons b (bubble-pass-inner (cons a (cddr lst)) (1+ i) r))
                                          (cons a (bubble-pass-inner (cdr lst) (1+ i) r)))))))
             (bubble-sort-rec (lst r)
                              (if (<= r 0)
                                  lst
                                  (bubble-sort-rec (bubble-pass-inner lst 0 r) (1- r)))))
      (mapcar #'car (bubble-sort-rec lst-with-keys (1- (length lst-with-keys)))))))
```

### Тестові набори та утиліти першої частини
```lisp
(defun check-bubble-sort-func (name input expected &key (key #'identity) (test #'<))
  (format t "~:[FAILED~;passed~] ~a~%"
    (equal (bubble-sort-func input :key key :test test) expected)
    name))

(defun test-bubble-sort-func ()
  (check-bubble-sort-func "test 1" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-bubble-sort-func "test 2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-bubble-sort-func "test 3" '() '())
  (check-bubble-sort-func "test 4" '(0) '(0))
  (check-bubble-sort-func "test 5" '(-3 0 5 -1 2) '(-3 -1 0 2 5))
  (check-bubble-sort-func "test 6" '(3 1 4 1 5 9 2 6 5 3 5) '(1 1 2 3 3 4 5 5 5 6 9))
  (check-bubble-sort-func "test 7" '(1 4 2 5 3) '(5 4 3 2 1) :test #'>)
  (check-bubble-sort-func "test 8" '(-10 7 -3 2 0) '(0 2 -3 7 -10) :key #'abs)
  (check-bubble-sort-func "test 9" '((a . 3) (b . 1) (c . 2)) '((b . 1) (c . 2) (a . 3)) :key #'cdr :test #'<))
```

### Тестування першої частини
```lisp
CL-USER> (test-bubble-sort-func)  
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
passed test 6
passed test 7
passed test 8
passed test 9
NIL
```

## Варіант другої частини 12
Написати функцію replacer , яка має два основні параметри what і to та два
ключові параметри — test та count . replacer має повернути функцію, яка при
застосуванні в якості першого аргументу reduce робить наступне: при обході списку з
кінця, кожен елемент списка-аргумента reduce , для якого функція test , викликана з
цим елементом та значенням what , повертає значення t (або не nil ), заміняється
на значення to . Якщо count передане у функцію, заміна виконується count разів.
Якщо count не передане тоді обмежень на кількість разів заміни немає. test має
значення за замовчуванням #'eql . Обмеження, які накладаються на використання
функції-результату replacer при передачі у reduce визначаються розробником (тобто,
наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів
функції reduce from-end та initial-value ).
```lisp
CL-USER> (reduce (replacer 1 2)
                 '(1 1 1 4)
                 :from-end ...
                 :initial-value ...)
(2 2 2 4)
CL-USER> (reduce (replacer 1 2 :count 2)
                 '(1 1 1 4)
                 :from-end ...
                 :initial-value ...)
(1 2 2 4)
```

## Лістинг реалізації другої частини завдання
```lisp
(defun replacer (what to &key (test #'eql) count)
  (let ((replacements-done 0))
    (lambda (element accumulator)
      (if (and (funcall test element what)
               (or (null count)
                   (< replacements-done count)))
          (progn
           (incf replacements-done)
           (cons to accumulator))
          (cons element accumulator)))))
```

### Тестові набори та утиліти другої частини
```lisp
(defun check-replacer (name what to input expected &key (test #'eql) count)
  (format t "~:[FAILED~;passed~] ~a~%"
    (equal (reduce (replacer what to :test test :count count)
               input
             :from-end t
             :initial-value '())
           expected)
    name))

(defun test-replacer ()
  (check-replacer "test 1" 1 2 '(1 1 1 4) '(2 2 2 4))
  (check-replacer "test 2" 1 2 '(1 1 1 4) '(1 2 2 4) :count 2)
  (check-replacer "test 3" 3 99 '(1 2 3 4 3) '(1 2 99 4 99) :test #'=)
  (check-replacer "test 4" 1 2 '() '())
  (check-replacer "test 5" 7 0 '(1 2 3) '(1 2 3))
  (check-replacer "test 6" 1 9 '(1 2 1 3 1) '(1 2 9 3 9) :count 2)
  (check-replacer "test 7" "foo" "bar" '("foo" "baz" "foo") '("bar" "baz" "bar") :test #'equal)
  (check-replacer "test 8" '(a . 1) '(x . 0)
                       '((a . 1) (b . 2) (a . 1))
                       '((x . 0) (b . 2) (x . 0)) :test #'equal))
```

### Тестування другої частини
```lisp
CL-USER> (test-replacer)
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
passed test 6
passed test 7
passed test 8
NIL
```