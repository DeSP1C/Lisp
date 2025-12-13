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