(in-package :dbd-oracle-test)


(defun run (connection sql &rest params)
  "Run SELECT query and return list of tuples, a list of plists."
  (let* ((query (dbi:prepare connection sql))
         (result (apply #'dbi:execute query params)))
    (dbi:fetch-all result)))

(defun execute-command (connection sql &rest params)
  (let ((query (dbi:prepare connection sql)))
    (apply #'dbi:execute query params)
    t))

(defun create-table (connection table-name fields-list-clause &key (if-exists-delete t))
  (let ((create-sql (format nil "CREATE TABLE ~a ~a" table-name fields-list-clause))
        (delete-sql (format nil "DELETE FROM ~a" table-name)))
    (handler-case
        (execute-command connection create-sql)
      (dbi:<dbi-database-error> (e)
        (if (and (= (slot-value e 'dbi.error::error-code) dbd.oracle::+ora-name-used+)
                 if-exists-delete)
            (execute-command connection delete-sql)
            (error e))))))

(defun get-first (rows column-key)
  (getf (first rows) column-key))

(defun almost= (a b &optional (delta 1d-6))
   (< (abs (- a b)) delta))


(defun make-random-string (max-length characters &key (min-length 1))
  (let* ((size (+ min-length (random (- max-length min-length -1))))
         (number-of-choices (length characters))
         (str (make-string size)))
    (dotimes (k size)
      (setf (aref str k) (elt characters (random number-of-choices))))
    str))
