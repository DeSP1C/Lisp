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

(run-all-tests)