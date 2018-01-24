(in-package :cl-user)
(defpackage dbd-oracle-asd
  (:use :cl :asdf))
(in-package :dbd-oracle-asd)

(defsystem dbd-oracle
  :name "DBD-Oracle"
  :author "Sergey Afonin <serg@msu.ru>"
  :maintainer "Sergey Afonin <serg@msu.ru>"
  :licence "Lessor Lisp General Public License"
  :description "ORACLE database driver for CL-DBI."
  :long-description "A CL-DBI interface for ORACLE database based on
  OCI bindings provided by CLSQL library."
  :version "1.0"
  :depends-on (:dbi :cffi-uffi-compat :cffi)
  :pathname "src"
  :components ((:file "package")
               (:file "oracle-constants")
               (:file "oracle-api")
               (:file "oracle-loader")
               (:file "foreign-resources")
               (:file "dbd-oracle"))

  :in-order-to ((test-op (load-op dbd-oracle-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (symbol-name '#:run-tests) :lift)
                             :config :generic)))

(defmethod operation-done-p
    ((o test-op) (c (eql (find-system 'dbd-oracle))))
  (values nil))
