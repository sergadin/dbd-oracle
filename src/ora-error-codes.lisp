;;;; *************************************************************************
;;;; This file defines common ORA-XXXXX error codes.
;;;; *************************************************************************

(in-package #:dbd.oracle)

(defconstant +ora-unique-constraint+       1 "Unique constraint violated")
(defconstant +ora-no-such-table+         942 "Table or view does not exist ")
(defconstant +ora-name-used+             955 "Name is already used by an existing object")
(defconstant +ora-not-bound+            1008 "Not all variables are bound")
(defconstant +ora-no-data-found+        1403 "No data found")
(defconstant +ora-snapshot-too-old+     1555 "Snapshot too old (Rollback has been overwritten)")

(defconstant +ora-missing-r-parenthesis+ 907  "Missing right parenthesis")

(defconstant +ora-end-of-channel+       3113 "End-of-file on communication channel (Network connection lost)")
(defconstant +ora-not-connected+        3114 "Not connected to ORACLE")
(defconstant +ora-tns-name+            12154 "TNS:could not resolve service name")

(defconstant +ora-invalid-password+     1017 "Invalid Username/Password")
(defconstant +ora-no-privileges+        1031 "Insufficient privileges")
