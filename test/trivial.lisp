(in-package :dbd-oracle-test)

;;; First, check the ability to connect...

(addtest (root)
  just-connect-disconnect
  (ensure (let ((connection (dbi:connect :oracle
                                         :database-name connect-string
                                         :username user-name
                                         :password password
                                         :encoding :utf-8)))
            (dbi:disconnect connection)
            t)
          :report "Connection to the database."))

;;;
;;; Non-parametric selects
;;;
;;; create table realval (r real, n number, ns number(*, 5), nps number(10, 2), i integer);
;;; insert into realval values (17.17, 17.17, 17.17, 17.17, 17);
;;;


(deftestsuite non-parametric-selects (autoconnect)
  ()
  (:documentation "Tests for selecting data of basic types."))


(addtest (non-parametric-selects)
  integer-values
  ;;
  (ensure-same (get-first (run connection "SELECT i AS val FROM realval") :val) 17)
  (ensure-same (get-first (run connection "SELECT 17 AS val FROM dual") :val) 17 :test #'almost=)

  ;; Negative numbers
  (ensure-same (get-first (run connection "SELECT -17 AS val FROM dual") :val) -17 :test #'=)
  (ensure-same (get-first (run connection "SELECT 0 AS val FROM dual") :val) 0 :test #'=)

  ;; Multiple rows
  (ensure-same (length (run connection "SELECT 17 AS val FROM dual UNION SELECT 18 from dual")) 2)
  t)


(addtest (non-parametric-selects)
  floating-point-values
  (ensure-same (get-first (run connection "SELECT r AS val FROM realval") :val) 17.17 :test #'almost=)
  (ensure-same (get-first (run connection "SELECT ns AS val FROM realval") :val) 17.17 :test #'almost=)
  (ensure-same (get-first (run connection "SELECT nps AS val FROM realval") :val) 17.17 :test #'almost=)
  (ensure-same (get-first (run connection "SELECT n AS val FROM realval") :val) 17.17 :test #'almost=)

  (ensure-same (get-first (run connection "SELECT -17.17 AS val FROM dual") :val) -17.17 :test #'almost=)
  (ensure-same (get-first (run connection "SELECT sin(17) AS val FROM realval") :val) -.96139749d0 :test #'almost=)
  t)


(addtest (non-parametric-selects)
  strings-and-nulls

  ;; basic string
  (ensure-same (get-first (run connection "SELECT 'asdF' AS val FROM dual") :val) "asdF" :test #'string=)

  ;; UTF string (Cyrillic)
  ;; Be sure that NLS_LANG variable is set properly, e.g. NLS_LANG=American_America.UTF8
  ;; Testing procedures assume UTF-8 encoding (see call to dbi:connect)
  (ensure-same (get-first (run connection "SELECT 'Фыва' AS val FROM dual") :val) "Фыва" :test #'string=)

  ;; NULL value
  (ensure-same (get-first (run connection "SELECT NULL AS val FROM dual") :val) nil)
  t)


;;;
;;; Selects with input parameters (variables binding)
;;;

(deftestsuite parametric-selects (autoconnect)
  ()
  (:documentation "Queries that accept parameters."))

(addtest (parametric-selects)
  numeric-values
  (ensure-null (run connection "SELECT * FROM realval WHERE i < ?" 3))
  (ensure-same (get-first (run connection "SELECT i AS val FROM realval WHERE i > ?" 3) :val) 17 :test #'=)
  (ensure-same (get-first (run connection "SELECT i AS val FROM realval WHERE i > ?" 3.0d0) :val) 17 :test #'=)
  (ensure-same (get-first (run connection "SELECT i AS val FROM realval WHERE r > ?" 3.0d0) :val) 17 :test #'=)
  ;; Bind two parameters
  (ensure-same (get-first (run connection "SELECT i AS val FROM realval WHERE r > ? and i > ?" 3.0d0 14) :val) 17 :test #'=)
  t)

(addtest (parametric-selects)
  string-values
  (ensure-same (get-first (run connection "SELECT -1 AS val FROM dual WHERE 'asdf' = ?" "asdf") :val) -1 :test #'=)
  (ensure-same (get-first (run connection "SELECT -1 AS val FROM dual WHERE 'Фыва' = ?" "Фыва") :val) -1 :test #'=)
  ;; Two strings
  (ensure-same (get-first (run connection "SELECT -1 AS val FROM dual WHERE 'asdf' = ? and ? like 'as%'" "asdf" "as-prefixed-string") :val) -1 :test #'=)
  t)

;;;
;;; Transactions and DML commands
;;;


(deftestsuite basic-dml (autoconnect)
  ()
  (:documentation "Tests for update/insert/delete operations."))


(addtest (basic-dml)
  update-transactions
  ;; check that record exists
  (ensure-same (get-first (run connection "SELECT n AS val FROM realval") :val) 17.17 :test #'almost=)
  ;; update, check result, and rollback the transaction
  (execute-command connection "UPDATE realval SET n = ? WHERE n > ?" 100 3)
  (ensure-same (get-first (run connection "SELECT n AS val FROM realval") :val) 100 :test #'almost= :report "Value was not updated")
  (dbi:rollback connection)
  ;; validate that changes are rolled back
  (ensure-same (get-first (run connection "SELECT n AS val FROM realval") :val) 17.17 :test #'almost= :report "ROLLBACK does not changed updated variable to its previous value")

  ;;; Repeat the procedure with switched arguments to UPDATE. No updates should happen.
  (execute-command connection "UPDATE realval SET n = ? WHERE n > ?" 3 100)
  (ensure-same (get-first (run connection "SELECT n AS val FROM realval") :val) 17.17 :test #'almost= :report "Value was updated")
  (dbi:rollback connection)
  t)

(addtest (basic-dml)
  insert-transactions
  (ensure-null (run connection "SELECT n AS val FROM realval WHERE n >= 100"))
  (execute-command connection "INSERT INTO realval (n, r) VALUES ( ?, ?)" 100 -100)
  (ensure (run connection "SELECT n AS val FROM realval WHERE n >= 100"))
  (dbi:rollback connection)
  t)

(addtest (basic-dml)
  delete-transactions
  (ensure-same (get-first (run connection "SELECT n AS val FROM realval") :val) 17.17 :test #'almost=)
  (execute-command connection "DELETE FROM realval WHERE r >= ? AND r <= ?" -100 100)
  (ensure-null (run connection "SELECT n AS val FROM realval WHERE n >= 100"))
  (dbi:rollback connection)
  t)
