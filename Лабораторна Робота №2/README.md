<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Литвин Максим Ігорович КВ-23</p>
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

Додатковий бал за лабораторну роботу можна отримати в разі виконання всіх наступних
умов:
робота виконана до дедлайну (включно з датою дедлайну)
крім основних реалізацій функцій за варіантом, також реалізовано додатковий
варіант однієї чи обох функцій, який працюватиме швидше за основну реалізацію,
не порушуючи при цьому перші три вимоги до основної реалізації (вимоги 4 і 5
можуть бути порушені), за виключенням того, що в разі необхідності можна також
використати стандартну функцію copy-list

## Варіант 12
1. Написати функцію remove-thirds-and-reverse , яка видаляє зі списку кожен третій
елемент і обертає результат у зворотному порядку:
```Lisp
CL-USER> (remove-thirds-and-reverse '(a b c d e f g))
(G E D B A)
```
2. Написати функцію list-set-difference-3 , яка визначає різницю трьох множин,
заданих списками атомів:
```Lisp
CL-USER> (list-set-difference '(1 2 3 4) '(4 5 6) '(2 5 7))
(1 3) ; порядок може відрізнятись
```

## Лістинг функції remove-thirds-and-reverse
```lisp
(defun remove-thirds-and-reverse (lst &optional (res '()))
  (if (null lst)
      res
      (let ((a (car lst))
            (b (cadr lst)))
        (remove-thirds-and-reverse (cdddr lst)
                                   (if b
                                       (cons b (cons a res))
                                       (cons a res))))))
```
### Тестові набори та утиліти
```lisp
(defun check-remove-thirds-and-reverse (name input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
    (equal (remove-thirds-and-reverse input) expected)
    name))

(defun test-remove-thirds-and-reverse ()
  (check-remove-thirds-and-reverse "test 1" '(a b c d e f g) '(g e d b a))
  (check-remove-thirds-and-reverse "test 2" '(1 2 3 4 5 6) '(5 4 2 1))
  (check-remove-thirds-and-reverse "test 3" '(x y) '(y x))
  (check-remove-thirds-and-reverse "test 4" '() '())
  (check-remove-thirds-and-reverse "test 5" '(p q r) '(q p))
  (check-remove-thirds-and-reverse "test 6" '(a b c d e) '(e d b a))
  (check-remove-thirds-and-reverse "test 7" '(1 2 3 4 5 6 7 8 9) '(8 7 5 4 2 1))
  (check-remove-thirds-and-reverse "test 8" '(1) '(1)))
```
### Тестування
```lisp
CL-USER> (remove-thirds-and-reverse)
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
## Лістинг функції list-set-difference-3
```lisp
(defun my-member (item lst)
  (cond
    ((null lst) nil)
    ((equal item (car lst)) t)
    (t (my-member item (cdr lst)))))

(defun list-set-difference-3 (set-a set-b set-c)
  (cond
    ((null set-a) nil)
    ((or (my-member (car set-a) set-b) (my-member (car set-a) set-c))
     (list-set-difference-3 (cdr set-a) set-b set-c))
    (t (cons (car set-a)
           (list-set-difference-3 (cdr set-a) set-b set-c)))))
```
### Тестові набори та утиліти
```lisp
(defun check-list-set-difference-3 (name set-a set-b set-c expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (list-set-difference-3 set-a set-b set-c) expected)
          name))

(defun test-list-set-difference-3 ()
  (check-list-set-difference-3 "test 1" '(1 2 3 4) '(4 5 6) '(2 5 7) '(1 3))
  (check-list-set-difference-3 "test 2" '(a b c d) '(b) '(d) '(a c))
  (check-list-set-difference-3 "test 3" '(1 2 3 4 5) '(2 4) '(5) '(1 3))
  (check-list-set-difference-3 "test 4" '() '(1 2) '(3 4) '())
  (check-list-set-difference-3 "test 5" '(p q r s) '(q r) '(s) '(p))
  (check-list-set-difference-3 "test 6" '(x y z) '() '() '(x y z))
  (check-list-set-difference-3 "test 7" '(1 2 3 4 5 6) '(1 3 5) '(2 4 6) '()))
```
### Тестування
```lisp
CL-USER> (test-list-set-difference-3)
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
passed test 6
passed test 7
NIL
```