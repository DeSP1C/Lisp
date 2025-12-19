(defun compute-f-seq ()
  (let ((f (make-array 25 :element-type 'double-float :initial-element 0d0)))
    (setf (aref f 1) 1d0)

    (loop for i from 2 to 10 do
      (let ((prev (aref f (1- i))))
        (setf (aref f i)
              (+ (sin prev)
                 (cos (* 2d0 prev))))))

    (setf (aref f 11) 1d0)

    (loop for i from 12 to 24 do
      (setf (aref f i)
            (+ (aref f (1- i))
               (sqrt (float i 1d0)))))

    f))

(defun print-f-seq (&optional (f (compute-f-seq)))
  (format t "~& i        Fi~%----------------------~%")
  (loop for i from 1 to 24 do
    (format t "~2d  ~,12f~%" i (aref f i)))
  f)

(defun approx= (a b &optional (eps 1d-12))
  (< (abs (- a b)) eps))

(defun run-tests ()
  (let ((f (compute-f-seq)))
    (assert (approx= (aref f 1) 1d0))
    (assert (approx= (aref f 11) 1d0))
    (assert (approx= (aref f 2) 0.4253241482607541d0 1d-12))
    (assert (approx= (aref f 10) 0.27531074681339274d0 1d-12))
    (assert (approx= (aref f 24) 55.84887729851865d0 1d-12))

    (format t "~&All tests passed.~%")
    t))

(print-f-seq)