#|
  CL-DBI Database driver for ORACLE.

  Author: Sergey Afonin (serg@msu.ru)
|#

(in-package :cl-user)
(defpackage dbd-oracle-test-asd
  (:use :cl :asdf))
(in-package :dbd-oracle-test-asd)

(defsystem dbd-oracle-test
  :name "Tests for DBD-Oracle"
  :author "Sergey Afonin <serg@msu.ru>"
  :maintainer "Sergey Afonin <serg@msu.ru>"
  :licence "Lessor Lisp General Public License"
  :depends-on (:dbd-oracle)
  :pathname "test"
  :components ((:file "trivial")))

