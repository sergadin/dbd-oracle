(in-package :dbd-oracle-test)

(deftestsuite extended-functions (autoconnect)
  ()
  (:documentation "Tests for selecting data of basic types."))


(addtest (extended-functions)
  parse-bind-bind
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
