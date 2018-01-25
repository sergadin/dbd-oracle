(in-package :dbd-oracle-test)

;;;
;;; Check that some errors (syntax, missing tables etc) are detected and signaled.
;;;


(deftestsuite expected-errors (autoconnect)
  ()
  (:documentation "Tests for ability to handle error."))


(addtest (expected-errors)
  syntax-and-data-errors
  ;;
  (ensure-error (run connection "SELECT i AS val FROM"))
  (ensure-error (run connection "SELECT something_unexpected FROM dual"))
  (ensure-error (run connection "SELECT something_unexpected FROM realval"))
  (ensure-error (run connection "SELECT x FROM non_existant_table"))

  (ensure-error (run connection "SELECT COUNT(*) AS cnt FROM realval WHERE r < ?"))
  t)
