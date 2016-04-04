(in-package :cl-user)

(defpackage dbd.oracle-test
  (:use :cl
        :dbd.oracle))
(in-package dbd.oracle-test)

(defun trivial-test ()
    (let* ((dbd.oracle:*foreign-library-search-paths* '(#p"/opt/oracle/"))
           (connection (dbi:connect :oracle
                                    :database-name "127.0.0.1:1521/orcl"
                                    :username "scott"
                                    :password "tiger"
                                    :encoding :utf-8)))
      (flet ((execute-command (sql &rest params)
               (let ((query (dbi:prepare connection sql)))
                 (apply #'dbi:execute query params)
                 t)))
        (execute-command "CREATE TABLE testdbi (k INTEGER, s VARCHAR2(512))")
        (dbi:with-transaction connection
          (execute-command "INSERT INTO testdbi (k, s) VALUES( ?, ?)" 1 "abc")
          (execute-command "INSERT INTO testdbi (k, s) VALUES( ?, ?)" 2 "xyz")
          (let* ((stmt "SELECT sysdate, k, s FROM testdbi WHERE k > ?")
                 (query (dbi:prepare connection stmt))
                 (result (dbi:execute query 0)))
            (loop for row = (dbi:fetch result)
               while row
               do (print row)))
          (execute-command "DELETE FROM testdbi WHERE k < ?" 3)
          (format t "~&~D rows deleted.~%" (dbi:row-count connection)))
        (execute-command "DROP TABLE testdbi")
        (dbi:disconnect connection))))

