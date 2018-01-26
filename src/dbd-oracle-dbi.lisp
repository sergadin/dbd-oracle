;;;
;;; DBI related methods.
;;;

(in-package :dbd.oracle)

(cl-syntax:use-syntax :annot)

@export
(defclass <dbd-oracle> (<dbi-driver>) ())

@export
(defclass <dbd-oracle-connection> (<dbi-connection>)
  ((%format-placeholders
    :type boolean
    :initarg :format-placeholders)
   (%rows-affected
    :type integer
    :documentation "Number of rows affected by the last INSERT/UPDATE/DELETE statement.")))


(defclass <dbd-oracle-query> (<dbi-query>)
  ((%cursor)
   (reusablep :initform nil :accessor query-reusablep)))


(defun questions->placeholders (query-string)
  "Convert the query from CL-DBI placeholder syntax into ORACLE-friendly colon notation."
  (loop
     with result = ""
     for last-pos = 0 then (+ pos 2)
     for pos = (search " ?" query-string :start2 last-pos)
     for count from 1
     do
       (setf result (concatenate
                     'string result
                     (subseq query-string last-pos pos)
                     (when pos (format nil " :~D" count))))
     while pos
     finally (return result)))

(defmethod prepare ((conn <dbd-oracle-connection>) (sql string) &key)
  (make-instance '<dbd-oracle-query>
                 :sql sql
                 :connection conn
                 :prepared (prepare-statement (connection-handle conn)
                                              (if (slot-value conn '%format-placeholders)
                                                  (questions->placeholders sql)
                                                  sql))))


(defmethod execute-using-connection ((conn <dbd-oracle-connection>) (query <dbd-oracle-query>) params)
  (let ((database (connection-handle conn))
        (prepared (query-prepared query))
        (result-types :as-is) ; convert integers and floats into corresponding LISP values
        (field-names t))
    ;;(reset-statement prepared)
    (let ((count 0))
      (dolist (param params)
        (bind-or-valuate-parameter prepared (incf count) param)))
    (setf (slot-value query '%cursor)
          (sql-prepared-stmt-exec prepared database result-types field-names))
    ;; Copy number of updated rows from the statement to the connection instance
    ;; because row-count operates on connection level.
    (when (slot-boundp prepared 'rows-affected)
      (setf (slot-value conn '%rows-affected)
            (slot-value prepared 'rows-affected)))
    query))


(defmethod fetch-using-connection ((conn <dbd-oracle-connection>) (query <dbd-oracle-query>))
  (let ((database (connection-handle conn))
        (cursor (slot-value query '%cursor))
        (include-field-names t))
    (car (process-cursor cursor database include-field-names :fetch-size 1))))


(defmethod make-connection ((driver <dbd-oracle>) &key database-name username password
                                                    (format-placeholders t)
                                                    encoding)
  "Connect to ORACLE database specified by the connect-string
DATABASE-NAME using USERNAME/PASSWORD. Encoding is passed to CFFI
string conversion functions.

Example of DATABASE-NAME:
  127.0.0.1:1521/orcl
"
  (unless *oracle-library-loaded*
    (oracle-load-foreign))
  (make-instance '<dbd-oracle-connection>
     :database-name database-name
     :auto-commit nil
     :handle (oracle-connect database-name username password
                             :encoding (or encoding (nls-lang-encoding) :utf-8))
     :format-placeholders format-placeholders))


(defmethod disconnect ((conn <dbd-oracle-connection>))
  (let ((database (connection-handle conn)))
    (osucc (oci-logoff (deref-vp (svchp database))
                       (deref-vp (errhp database))))
    (osucc (oci-handle-free (deref-vp (envhp database)) +oci-htype-env+))
    ;; Note: It's neither required nor allowed to explicitly deallocate the
    ;; ERRHP handle here, since it's owned by the ENVHP deallocated above,
    ;; and was therefore automatically deallocated at the same time.
    t))


(defmethod begin-transaction ((conn <dbd-oracle-connection>))
  (let ((database (connection-handle conn))
        (timeout-sec 1))
    (declare (ignore database timeout-sec))
    ;;
    ;; OCILogon creates a session that starts transaction automatically
    ;;
    #+(or)(osucc (oci-trans-start (deref-vp (svchp database))
                            (deref-vp (errhp database))
                            timeout-sec
                            +oci-trans-new+))
    t))

(defmethod commit ((conn <dbd-oracle-connection>))
  (let ((database (connection-handle conn)))
    (with-slots (svchp errhp) database
      (osucc (oci-trans-commit (deref-vp svchp)
                               (deref-vp errhp)
                               0)))))

(defmethod rollback ((conn <dbd-oracle-connection>))
  (let ((database (connection-handle conn)))
    (osucc (oci-trans-rollback (deref-vp (svchp database))
                               (deref-vp (errhp database))
                               0))))

(defmethod row-count ((conn <dbd-oracle-connection>))
  (when (slot-boundp conn '%rows-affected)
    (slot-value conn '%rows-affected)))


(defmethod release-resources ((query <dbd-oracle-query>))
  (release-resources (query-prepared query)))
