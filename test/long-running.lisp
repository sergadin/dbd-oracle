(in-package :dbd-oracle-test)

;;;
;;; Check long-running operations
;;;


(deftestsuite long-running (autoconnect)
  ()
  (:documentation "Tests for long-running operations."))


(addtest (long-running)
  thousands-of-fetchall-selects
  ;;
  (dotimes (k 22000)
    (ensure (run connection "SELECT COUNT(*) FROM realval")
            :report "SELECT failed at iteration ~A"
            :arguments (k)))
  t)
