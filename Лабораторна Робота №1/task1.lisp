(setq mylist (list 1 'a (cons 2 '(3)) '() (list 'end 'list)))

;; Отримайте голову списку
(car mylist)

;; Отримайте хвіст списку
(cdr mylist)

;; Отримайте третій елемент списку
;; 1 спосіб
(car (cddr mylist)) 

;; 2 спосіб
(nth 2 mylist)

;; Отримайте останній елемент списку
(car (last mylist))

;; Використання atom
;; 1 спосіб
(atom 10)

;; 2 спосіб
(atom 'a)

;; 3 спосіб
(atom 'nil)

;; Використання listp
;; 1 спосіб
(listp mylist)

;; 2 спосіб
(listp '())

;; 3 спосіб
(listp 'a)

;;Інші предикати

;;Рівність структур
(equal '('A 'B) '('A 'B))

;;Чи це число?
(numberp '123)

;;Чи це символ?
(symbolp 'a)

;;Чи це нуль?
(null '())

;;З'єднання списку з не пустим підсписком
(append mylist (nth 2 mylist))