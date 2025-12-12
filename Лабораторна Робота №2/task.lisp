(defun remove-thirds-and-reverse (lst &optional (res '()))
  (if (null lst)
      res
      (let ((a (car lst))
            (b (cadr lst)))
        (remove-thirds-and-reverse (cdddr lst)
                                   (if b
                                       (cons b (cons a res))
                                       (cons a res))))))

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