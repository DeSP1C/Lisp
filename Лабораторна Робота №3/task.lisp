(defun bubble-pass-inner (lst i r)
  (cond
   ((>= i r) lst)
   ((null (cdr lst)) lst)
   (t (let ((a (car lst))
            (b (cadr lst)))
        (if (> a b)
            (cons b (bubble-pass-inner (cons a (cddr lst)) (1+ i) r))
            (cons a (bubble-pass-inner (cdr lst) (1+ i) r)))))))

(defun bubble-sort-rec (lst r)
  (if (<= r 0)
      lst
      (bubble-sort-rec (bubble-pass-inner lst 0 r) (1- r))))

(defun bubble-sort-func (lst)
  (let ((n (length lst)))
    (if (< n 2)
        lst
        (bubble-sort-rec lst (1- n)))))

(defun check-bubble-sort-func (name input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
    (equal (bubble-sort-func input) expected)
    name))

(defun test-bubble-sort-func ()
  (check-bubble-sort-func "test 1" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-bubble-sort-func "test 2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-bubble-sort-func "test 3" '() '())
  (check-bubble-sort-func "test 4" '(0) '(0))
  (check-bubble-sort-func "test 5" '(-3 0 5 -1 2) '(-3 -1 0 2 5))
  (check-bubble-sort-func "test 6" '(3 1 4 1 5 9 2 6 5 3 5) '(1 1 2 3 3 4 5 5 5 6 9)))

(defun bubble-sort-imper (lst)
  (let* ((tmp-lst (copy-list lst))
         (n (length tmp-lst)))
    (when (> n 1)
          (loop for R from (1- n) downto 1 do
                  (let ((curr tmp-lst))
                    (loop for i from 0 below R do
                            (let ((next (cdr curr)))
                              (when (> (car curr) (car next))
                                    (let ((tmp (car curr)))
                                      (setf (car curr) (car next) (car next) tmp)))
                              (setf curr next))))))
    tmp-lst))

(defun check-bubble-sort-imper (name input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
    (equal (bubble-sort-imper input) expected)
    name))

(defun test-bubble-sort-imper ()
  (check-bubble-sort-imper "test 1" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-bubble-sort-imper "test 2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-bubble-sort-imper "test 3" '() '())
  (check-bubble-sort-imper "test 4" '(0) '(0))
  (check-bubble-sort-imper "test 5" '(-3 0 5 -1 2) '(-3 -1 0 2 5))
  (check-bubble-sort-imper "test 6" '(3 1 4 1 5 9 2 6 5 3 5) '(1 1 2 3 3 4 5 5 5 6 9)))

(format t "~% ----FUNC----~%~%")
(test-bubble-sort-func)
(format t "~% ----IMPER----~%~%")
  (test-bubble-sort-imper)