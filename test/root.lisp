
(in-package :dbd-oracle-test)

(deftestsuite root ()
  ((user-name "scott")
   (password "tiger")
   (connect-string (let ((host "127.0.0.1")
                         (port "1521")
                         (oracle-sid (or (dbd.oracle::getenv "ORACLE_SID") "orcl")))
                     (format nil "~A:~A/~A" host port oracle-sid))))
  (:dynamic-variables
   (dbd.oracle:*foreign-library-search-paths* '(#p"/opt/oracle/")))
  (:documentation
   "Root unit test suite."))


(deftestsuite autoconnect (root)
  (connection)
  ;; additioal path to search oracle libraries
  (:run-setup :once-per-suite)
  (:setup
   (setf connection (dbi:connect :oracle
                                 :database-name connect-string
                                 :username user-name
                                 :password password
                                 :encoding :utf-8)))
  (:teardown
   (when connection
     (dbi:disconnect connection)
     (setf connection nil)))
  (:documentation
   "Root unit test suite."))


(defun run-all-tests ()
  (lift:run-tests :suite 'root :break-on-errors? nil))
