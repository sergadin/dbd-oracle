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

(defun get-first (rows column-key)
  (getf (first rows) column-key))

(defun almost= (a b)
   (< (abs (- a b)) 0.000001))
