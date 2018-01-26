(in-package :dbd-oracle-test)

(deftestsuite blobs-and-clobs (autoconnect)
  ()
  (:documentation "Tests for BLOBs and CLOBs fields.")
  (:function
   (test-for-length
    (length characters)
    (let ((table-fields "(k INTEGER, data CLOB)")
          (insert-sql "INSERT INTO clob_table (k, data) VALUES ( ?, ?) ")
          (validate-length-sql "SELECT SUM(LENGTH(data)) AS len FROM clob_table")
          (substr-sql "SELECT DBMS_LOB.SUBSTR(data, ?, ?) AS fragment FROM clob_table WHERE k = ?")
          (drop-sql "DROP TABLE clob_table")
          (random-string (make-random-string length characters :min-length length))
          (number-of-subseq-tests 100))
      ;; Create table and populate it with two ASCII-strings
      (create-table connection "clob_table" table-fields)
      (execute-command connection insert-sql 1 random-string)
      (execute-command connection insert-sql 2 random-string)

      ;; Check that total length in the table is as expected
      (ensure-same (get-first (run connection validate-length-sql) :len) (* 2 length)
                   :test #'= :report "Total length of stored CLOBs differs from the expected")

      ;; Check that the string itself is the same with some probability.
      ;; The test extracts random substrings of length not exceeding maximal varchar2 size.
      (do* ((attempt 0 (1+ attempt))
            (string-id 1)
            (offset 1 (1+ (random (1- length)))) ; ORACLE uses 1-based indexes
            (amount 1 (1+ (random (min (- length offset)
                                       dbd.oracle::+maximal-varchar2-length+)))))
           ((>= attempt number-of-subseq-tests))
        (ensure-same (get-first (run connection substr-sql amount offset string-id) :fragment)
                     (subseq random-string (1- offset) (+ offset -1 amount))
                     :test #'string=
                     :report "Substring extracted back from the CLOB does not match the stored value"))

      ;; Drop the table
      (execute-command connection drop-sql)
      t))))


(addtest (blobs-and-clobs)
  short-ascii-strings
  (test-for-length 500 "ACGT"))

(addtest (blobs-and-clobs)
  short-non-ascii-strings
  (test-for-length 500 "фыва"))

(addtest (blobs-and-clobs)
  close-to-2k-limit-non-ascii-strings
  (test-for-length 1900 "фыва"))

(addtest (blobs-and-clobs)
  long-strings
  (test-for-length 20000 "ACGT"))

(addtest (blobs-and-clobs)
  long-non-ascii-strings
  (test-for-length 20000 "фыва"))

(addtest (blobs-and-clobs)
  fairy-long-non-ascii-strings
  (test-for-length 123000 "фыва"))
