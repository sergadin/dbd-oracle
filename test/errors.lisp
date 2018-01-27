(in-package :dbd-oracle-test)

;;;
;;; Check that some errors (syntax, missing tables etc) are detected and signaled.
;;;


(deftestsuite expected-errors (autoconnect)
  ()
  (:documentation "Tests for ability to handle error.")
  (:function
   (ora-code (e ora-code)
             #'(lambda ()
                 (= (slot-value e 'dbi.error::error-code) ora-code)))))


(addtest (expected-errors)
  syntax-and-data-errors
  ;;
  (ensure-error (run connection "SELECT i AS val FROM"))
  (ensure-error (run connection "SELECT something_unexpected FROM dual"))
  (ensure-error (run connection "SELECT something_unexpected FROM realval"))
  (ensure-error (run connection "SELECT x FROM non_existant_table"))

  (ensure-error (run connection "SELECT COUNT(*) AS cnt FROM realval WHERE r < ?"))
  t)

(addtest (expected-errors)
  ora-codes
  ;; table does not exist
  (ensure-condition (dbi.error:<dbi-database-error>
                     :validate (ora-code condition dbd.oracle::+ora-no-such-table+))
    (run connection "SELECT COUNT(*) from nonexistent_table"))

  ;; Create table with an unique constrained column
  (create-table connection "ee_unique" "(k INTEGER NOT NULL UNIQUE)")
  (ensure (execute-command connection "INSERT INTO ee_unique (k) VALUES ( ?)" 123))
  ;; Unique constraint violation
  (ensure-condition (dbi.error:<dbi-database-error>
                     :validate (ora-code condition dbd.oracle::+ora-unique-constraint+))
    (execute-command connection "INSERT INTO ee_unique (k) VALUES ( ?)" 123))

  ;; Unbound variable
  (ensure-condition (dbi.error:<dbi-database-error>
                     :validate (ora-code condition dbd.oracle::+ora-not-bound+))
    (execute-command connection "INSERT INTO ee_unique (k) VALUES ( ?)"))

  ;; Name used
  (ensure-condition (dbi.error:<dbi-database-error>
                     :validate (ora-code condition dbd.oracle::+ora-name-used+))
    (execute-command connection "CREATE TABLE ee_unique (k INTEGER NOT NULL UNIQUE)"))
  (ensure (execute-command connection "DROP TABLE ee_unique"))
  t)
