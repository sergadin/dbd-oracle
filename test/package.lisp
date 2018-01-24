(in-package :cl-user)

(defpackage :dbd-oracle-test
  (:use :cl :lift :dbd.oracle)
  (:export #:run-all-tests)
  (:documentation
   "This package contains unit tests of dbd-oracle."))
