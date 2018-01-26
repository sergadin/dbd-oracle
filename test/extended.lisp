(in-package :dbd-oracle-test)

(deftestsuite extended-functions (autoconnect)
  ()
  (:documentation "Tests for selecting data of basic types."))


(addtest (extended-functions)
  prepare-execute*
  ;;
  (execute-command connection "CREATE TABLE x (k INTEGER, r NUMBER)")
  ;; (execute-command connection "DELETE FROM x")

  (with-reusable-query (query connection "INSERT INTO x (k, r) VALUES ( ?, ?)")
    (let ((precise-answer (/ (* pi pi) 6d0))
          (limit 20000))
      (loop for k from 1 to limit
         do (dbi:execute query k (/ 1.0d0 (* k k))))
      (ensure-same
       (get-first (run connection "SELECT SUM(r) AS approx FROM x") :approx)
       precise-answer
       :test #'(lambda (a b) (almost= a b 0.001))
       :report "Euler's series sum_k 1/k^2 did not converge to pi^2/6 after ~D summations..."
       :arguments (limit))))

  (execute-command connection "DROP TABLE x")
  t)


(addtest (extended-functions)
  concurrent-connections
  (let ((create-sql "CREATE TABLE concurrent_access (k INTEGER)")
        (insert-sql "INSERT INTO concurrent_access (k) VALUES ( ?) ")
        (count-sql "SELECT COUNT(*) AS cnt FROM concurrent_access")
        (drop-sql "DROP TABLE concurrent_access"))
    (handler-case
        (execute-command connection create-sql)
      (dbi:<dbi-database-error> (e)
        (when (= (slot-value e 'dbi.error::error-code) dbd.oracle::+ora-name-used+)
          (execute-command connection "DELETE FROM concurrent_access"))))
    (execute-command connection insert-sql 7)
    ;; Check that other connection can not access uncommitted changes
    (dbi:with-connection (conn2 :oracle
                                :database-name connect-string
                                :username user-name
                                :password password)
      (ensure-same (get-first (run conn2 count-sql) :cnt) 0
                   :test #'= :report "Access to data from other connection")
      (dbi:commit conn2))
    ;; Again (we issued COMMIT in other connection)
    (dbi:with-connection (conn2 :oracle
                                :database-name connect-string
                                :username user-name
                                :password password)
      (ensure-same (get-first (run conn2 count-sql) :cnt) 0
                   :test #'= :report "Access to data from other connection"))
    ;; OK, do commit in connection
    (dbi:commit connection)
    ;; ... and check that data become accessable
    (dbi:with-connection (conn2 :oracle
                                :database-name connect-string
                                :username user-name
                                :password password)
      (ensure-same (get-first (run conn2 count-sql) :cnt) 1
                   :test #'= :report "Committed data is unaccessible"))
    ;; Drop the table
    (execute-command connection drop-sql)
    t))
