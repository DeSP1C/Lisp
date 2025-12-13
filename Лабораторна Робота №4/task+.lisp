(defun bubble-sort-func (lst &key (key #'identity) (test #'<))
  (let ((lst-with-keys (mapcar (lambda (x) (cons x (funcall key x))) lst))) ;; Створення списку точкових пар (елемент . значення ключа),
                                                                            ;; для кожного елементу вхідного списку викликається лямбда, яка виконує (cons x (funcall key x)).
                                                                            ;; Наприклад: для списку '(3 1 4)) та функції #'identity = ((3 . 3) (1 . 1) (4 . 4))
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
      (mapcar #'car (bubble-sort-rec lst-with-keys (1- (length lst-with-keys))))))) ;; Після завершення сортування маємо список з відсортованих точкових пар,
                                                                                    ;; тому тут відбувається витягування елементів з відкиданням ключів,
                                                                                    ;; щоб в результаті було ((1 . 1) (3 . 3) (4 . 4)) => (1 3 4)

(defun replacer (what to &key (test #'eql) count) ;; Це функція вищого порядку, яка повертає нову функцію
  (let ((replacements-done 0)) ;; Лексична змінна, яка "замикається" всередині лямбди (див. розділ 6.5 Замикання)
    (lambda (element accumulator) ;; Анонімна функція для reduce, яка приймає на вхід елемент та акумулятор
      (if (and (funcall test element what) ;; Перевірка, чи елемент співпадає з what (шуканий)
               (or (null count) ;; Перевірка, чи задано ліміт замін (див. розділ 12. Послідовності)
                   (< replacements-done count))) ;; Перевірка, чи ліміт ще не досягнуто
          (progn ;; Гілка TRUE: Виконує вирази та повертає результат останнього
           (incf replacements-done) ;; Інкремент для лічильника замін
           (cons to accumulator)) ;; Повертаємо акумулятор із заміненим елементом
          (cons element accumulator))))) ;; Гілка FALSE: Елемент не потрібно змінювати, тому додаємо в акумулятор і повертаємо як є