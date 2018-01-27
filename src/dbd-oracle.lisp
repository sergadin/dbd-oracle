;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; This file is a MODIFIED version of CLSQL oracle-sql.lisp.
;;;; Major modifications are:
;;;; - implementation of DBI interfaces;
;;;; - query preparation and parameters bindings via OCI calls.
;;;;
;;;; CLSSQL original banner follows.
;;;;

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          oracle-sql.lisp
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************



(in-package :dbd.oracle)


;;;; arbitrary parameters, tunable for performance or other reasons

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +errbuf-len+ 512
    "the number of characters that we allocate for an error message buffer")
  (defconstant +n-buf-rows+ 200
    "the number of table rows that we buffer at once when reading a table.
CMUCL has a compiled-in limit on how much C data can be allocated
(through malloc() and friends) at any given time, typically 8 Mb.
Setting this constant to a moderate value should make it less
likely that we'll have to worry about the CMUCL limit."))


(uffi:def-type vp-type :pointer-void)
(uffi:def-type vpp-type (* :pointer-void))

(defun uffi-foreign-string-length (c-stmt-string)
  (cffi:foreign-funcall "strlen" :string c-stmt-string :int))


(defmacro deref-vp (foreign-object)
  `(the vp-type (uffi:deref-pointer (the vpp-type ,foreign-object) :pointer-void)))

(uffi:def-pointer-var +unsigned-char-null-pointer+
  (uffi:make-null-pointer :unsigned-char))
(uffi:def-pointer-var +unsigned-short-null-pointer+
  (uffi:make-null-pointer :unsigned-short))
(uffi:def-pointer-var +unsigned-int-null-pointer+
  (uffi:make-null-pointer :unsigned-int))

;; constants - from OCI?

(defconstant +var-not-in-list+       1007)
(defconstant +no-data-found+         1403)
(defconstant +null-value-returned+   1405)
(defconstant +field-truncated+       1406)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant SQLT-NUMBER 2)
  (defconstant SQLT-INT 3)
  (defconstant SQLT-FLT 4)
  (defconstant SQLT-STR 5)
  (defconstant SQLT-LNG 8)
  (defconstant SQLT-DATE 12)
  (defconstant SQLT-BIN 23)
  (defconstant SQLT-CLOB 112)
  (defconstant SQLT-BLOB 113))

;; New versions, e.g. 12c, supports varchar2 strings of length 4000. Smaller limit may
;; cause performance degradation, due to CLOB processing overhead.
(defconstant +maximal-varchar2-length+ 2000
  "All strings exceedinng that length are bound as CLOBs, not VARCHAR2 data types.")

;;; Note that despite the suggestive class name (and the way that the
;;; *DEFAULT-DATABASE* variable holds an object of this class), a DB
;;; object is not actually a database but is instead a connection to a
;;; database. Thus, there's no obstacle to having any number of DB
;;; objects referring to the same database.

(uffi:def-type pointer-pointer-void (* :pointer-void))

(defclass <database> ()
  ((encoding
    :initform :utf-8
    :initarg :encoding
    :accessor encoding)))

(defclass <oracle-database> (<database>)
  ((envhp
    :reader envhp
    :initarg :envhp
    :type pointer-pointer-void
    :documentation
    "OCI environment handle")
   (errhp
    :reader errhp
    :initarg :errhp
    :type pointer-pointer-void
    :documentation
    "OCI error handle")
   (svchp
    :reader svchp
    :initarg :svchp
    :type pointer-pointer-void
    :documentation
    "OCI service context handle")
   (data-source-name
    :initarg :dsn
    :initform nil
    :documentation
    "optional data source name (used only for debugging/printing)")
   (user
    :initarg :user
    :reader user
    :type string
    :documentation
    "the \"user\" value given when data source connection was made")
   (date-format
    :initarg :date-format
    :reader date-format
    :initform "YYYY-MM-DD HH24:MI:SS\".0\"")
   (date-format-length
    :type number
    :documentation
    "Each database connection can be configured with its own date
output format.  In order to extract date strings from output buffers
holding multiple date strings in fixed-width fields, we need to know
the length of that format.")
   (server-version
    :type (or null string)
    :initarg :server-version
    :reader server-version
    :documentation
    "Version string of Oracle server.")
   (major-server-version
    :type (or null fixnum)
    :initarg :major-server-version
    :reader major-server-version
    :documentation
    "The major version number of the Oracle server, should be 8, 9, or 10")))

;;; Handle a non-successful result from an OCI function.
(defun handle-oci-result (result database nulls-ok)
  (case result
    (#.+oci-success+
     +oci-success+)
    (#.+oci-error+
     (handle-oci-error :database database :nulls-ok nulls-ok))
    (#.+oci-no-data+
     (error '<dbi-database-error> :message "OCI No Data Found" :error-code result))
    (#.+oci-success-with-info+
     (error '<dbi-database-error> :message "internal error: unexpected +oci-success-with-info" :error-code result))
    (#.+oci-invalid-handle+
     (error '<dbi-database-error> :message "OCI Invalid Handle" :error-code result))
    (#.+oci-need-data+
     (error '<dbi-database-error> :message "OCI Need Data" :error-code result))
    (#.+oci-still-executing+
     (error '<dbi-database-error> :message "OCI Still Executing" :error-code result))
    (#.+oci-continue+
     (error '<dbi-database-error> :message "OCI Continue" :error-code result))
    (1804
     (error '<dbi-database-error> :message "Check ORACLE_HOME and NLS settings." :error-code result))
    (t
     (error '<dbi-database-error>
            :message (format nil "OCI unknown error, code=~A" result)
            :error-code result))))

;;; Handle the messy case of return code=+oci-error+, querying the
;;; system for subcodes and reporting them as appropriate. ERRHP and
;;; NULLS-OK are as in the OERR function.

(defun handle-oci-error (&key database nulls-ok)
  (cond
    (database
     (with-slots (errhp) database
       (let ((errcode (uffi:allocate-foreign-object 'sb4))
             (errbuf (uffi:allocate-foreign-string #.+errbuf-len+)))
         ;; ensure errbuf empty string
         (setf (uffi:deref-array errbuf '(:array :unsigned-char) 0)
               (uffi:ensure-char-storable (code-char 0)))
         (setf (uffi:deref-pointer errcode 'sb4) 0)

         (uffi:with-cstring (sqlstate nil)
           (oci-error-get (deref-vp errhp) 1
                          sqlstate
                          errcode
                          (uffi:char-array-to-pointer errbuf)
                          +errbuf-len+ +oci-htype-error+))
         (let ((subcode (uffi:deref-pointer errcode 'sb4))
               (errstr (uffi:convert-from-foreign-string
                        errbuf
                        :encoding (when database (encoding database)))))
           (uffi:free-foreign-object errcode)
           (uffi:free-foreign-object errbuf)
           (unless (and nulls-ok (= subcode +null-value-returned+))
             (error '<dbi-database-error>
                    :error-code subcode
                    :message errstr))))))
    (nulls-ok
     (error '<dbi-database-error>
            :message "can't handle NULLS-OK without ERRHP"))
    (t
     (error '<dbi-database-error>
            :message "OCI Error (and no ERRHP available to find subcode)"))))

;;; Require an OCI success code.
;;;
;;; (The ordinary OCI error reporting mechanisms uses a fair amount of
;;; machinery (environments and other handles). In order to get to
;;; where we can use these mechanisms, we have to be able to allocate
;;; the machinery. The functions for allocating the machinery can
;;; return errors (e.g. out of memory) but shouldn't. Wrapping this function
;;; around function calls to such have-to-succeed functions enforces
;;; this condition.)

(defun osucc (code)
  (declare (type fixnum code))
  (unless (= code +oci-success+)
    (error '<dbi-database-error>
           :message (format nil "unexpected OCI failure, code=~S" code))))


;; Return the INDEXth string of the OCI array, represented as Lisp
;; SIMPLE-STRING. SIZE is the size of the fixed-width fields used by
;; Oracle to store strings within the array.

(uffi:def-type string-pointer (* :unsigned-char))

(defun deref-oci-string (arrayptr string-index size encoding)
  (declare (type string-pointer arrayptr))
  (declare (type (mod #.+n-buf-rows+) string-index))
  (declare (type (and unsigned-byte fixnum) size))
  (let ((str (uffi:convert-from-foreign-string
              (uffi:make-pointer
               (+ (uffi:pointer-address arrayptr) (* string-index size))
               :unsigned-char)
              :encoding encoding)))
    (if (string-equal str "NULL") nil str)))



(defun oracle-connect (data-source-name user password &key (encoding :utf-8))
  (let ((envhp (uffi:allocate-foreign-object :pointer-void))
        (errhp (uffi:allocate-foreign-object :pointer-void))
        (svchp (uffi:allocate-foreign-object :pointer-void))
        (srvhp (uffi:allocate-foreign-object :pointer-void)))
    ;; Requests to allocate environments and handles should never
    ;; fail in normal operation, and they're done too early to
    ;; handle errors very gracefully (since they're part of the
    ;; error-handling mechanism themselves) so we just assert they
    ;; work.

    (setf (deref-vp envhp) +null-void-pointer+)

    (progn
      (oci-initialize +oci-object+ +null-void-pointer+ +null-void-pointer+
                      +null-void-pointer+ +null-void-pointer-pointer+)
      (ignore-errors (oci-handle-alloc +null-void-pointer+ envhp
                                       +oci-htype-env+ 0
                                       +null-void-pointer-pointer+)) ;no testing return
      (oci-env-init envhp +oci-default+ 0 +null-void-pointer-pointer+))

    (oci-handle-alloc (deref-vp envhp) errhp
                      +oci-htype-error+ 0 +null-void-pointer-pointer+)
    (oci-handle-alloc (deref-vp envhp) srvhp
                      +oci-htype-server+ 0 +null-void-pointer-pointer+)

    (let ((db (make-instance '<oracle-database>
                             :envhp envhp
                             :errhp errhp
                             :svchp svchp
                             :dsn data-source-name
                             :user user
                             :encoding encoding)))
      (uffi:with-foreign-strings ((c-user user)
                                  (c-password password)
                                  (c-data-source-name data-source-name))
        (oci-logon (deref-vp envhp)
                   (deref-vp errhp)
                   svchp
                   c-user (length user)
                   c-password (length password)
                   c-data-source-name (length data-source-name)
                   :database db))
      ;; :date-format-length (1+ (length date-format)))))
      (database-execute-command
       (format nil "ALTER SESSION SET NLS_DATE_FORMAT='~A'" (date-format db)) db)
      (let ((server-version
             (caar (database-query
                    "SELECT BANNER FROM V$VERSION WHERE BANNER LIKE '%Oracle%'" db nil nil))))
        (setf (slot-value db 'server-version) server-version
              (slot-value db 'major-server-version) (major-client-version-from-string
                                                     server-version)))
      db)))


(defun major-client-version-from-string (str)
  (cond
    ((search " 12c " str)
     12)
    ((search " 11g " str)
     11)
    ((search " 10g " str)
     10)
    ((search "Oracle9i " str)
     9)
    ((search "Oracle8" str)
     8)))

(defun major-server-version-from-string (str)
  (when (> (length str) 2)
    (loop for ver from 8 to 12
       when (string= (format nil "~D." ver) (subseq str 0 (if (<= ver 9) 2 3)))
       do (return ver))))



(defstruct (cd (:constructor make-cd)
               (:print-function print-cd))
  "a column descriptor: metadata about the data in a table"

  ;; name of this column
  (name (error "missing NAME") :type simple-string :read-only t)
  ;; the size in bytes of a single element
  (sizeof (error "missing SIZE") :type fixnum :read-only t)
  ;; an array of +N-BUF-ROWS+ elements in C representation
  (buffer (error "Missing BUFFER")
          :type foreign-resource
          :read-only t)
  ;; an array of +N-BUF-ROWS+ OCI return codes in C representation.
  ;; (There must be one return code for every element of every
  ;; row in order to be able to represent nullness.)
  (retcodes (error "Missing RETCODES")
            :type foreign-resource
            :read-only t)
  (indicators (error "Missing INDICATORS")
              :type foreign-resource
              :read-only t)
  ;; the OCI code for the data type of a single element
  (oci-data-type (error "missing OCI-DATA-TYPE")
                 :type fixnum
                 :read-only t)
  (result-type (error "missing RESULT-TYPE")
               :read-only t))


(defun print-cd (cd stream depth)
  (declare (ignore depth))
  (print-unreadable-object (cd stream :type t)
    (format stream
            ":NAME ~S :OCI-DATA-TYPE ~S :OCI-DATA-SIZE ~S"
            (cd-name cd)
            (cd-oci-data-type cd)
            (cd-sizeof cd))))

(defun print-query-cursor (qc stream depth)
  (declare (ignore depth))
  (print-unreadable-object (qc stream :type t :identity t)
    (prin1 (qc-db qc) stream)))




;; Return one row of the table referred to by QC, represented as a
;; list; or if there are no more rows, signal an error if EOF-ERRORP,
;; or return EOF-VALUE otherwise.

;; KLUDGE: This CASE statement is a strong sign that the code would be
;; cleaner if CD were made into an abstract class, we made variant
;; classes for CD-for-column-of-strings, CD-for-column-of-floats,
;; etc., and defined virtual functions to handle operations like
;; get-an-element-from-column. (For a small special purpose module
;; like this, would arguably be overkill, so I'm not going to do it
;; now, but if this code ends up getting more complicated in
;; maintenance, it would become a really good idea.)

;; Arguably this would be a good place to signal END-OF-FILE, but
;; since the ANSI spec specifically says that END-OF-FILE means a
;; STREAM which has no more data, and QC is not a STREAM, we signal
;; DBI-ERROR instead.

(uffi:def-type short-array (* :short))
(uffi:def-type int-array (* :int))
(uffi:def-type double-array (* :double))
(uffi:def-type int-pointer (* :int))
(uffi:def-type double-pointer (* :double))

;;; the result of a database query: a cursor through a table
(defstruct (oracle-result-set (:print-function print-query-cursor)
                              (:conc-name qc-)
                              (:constructor %make-query-cursor))
  (db (error "missing DB")   ; db conn. this table is associated with
    :type <oracle-database>
    :read-only t)
  (stmthp (error "missing STMTHP")      ; the statement handle used to create
;;  :type alien                 ; this table. owned by the QUERY-CURSOR
    :read-only t)                       ; object, deallocated on CLOSE-QUERY
  (cds) ;  (error "missing CDS")            ; column descriptors
;    :type (simple-array cd 1)
                                        ;    :read-only t)
  (n-from-oci
   0                         ; buffered rows: number of rows recv'd
   :type (integer 0 #.+n-buf-rows+))   ; from the database on the last read
  (n-to-dbi
   0                           ; number of buffered rows returned, i.e.
   :type (integer 0 #.+n-buf-rows+))   ; the index, within the buffered rows,
                                        ; of the next row which hasn't already
                                        ; been returned
  (total-n-from-oci
   0                   ; total number of bytes recv'd from OCI
   :type unsigned-byte)                ; in all reads
  (oci-end-seen-p nil))                 ; Have we seen the end of OCI
                                        ; data, i.e. OCI returning
                                        ; less data than we requested?
                                        ; OCI doesn't seem to like us
                                        ; to try to read more data
                                        ; from it after that..


(defun fetch-row (qc &optional (eof-errorp t) eof-value encoding)
  (declare (optimize (speed 3)))
  (cond ((zerop (qc-n-from-oci qc))
         (if eof-errorp
             (error 'sql-database-error :message
                    (format nil "no more rows available in ~S" qc))
           eof-value))
        ((>= (qc-n-to-dbi qc)
             (qc-n-from-oci qc))
         (refill-qc-buffers qc)
         (fetch-row qc nil eof-value encoding))
        (t
         (let ((cds (qc-cds qc))
               (reversed-result nil)
               (irow (qc-n-to-dbi qc)))
           (dotimes (icd (length cds))
             (let* ((cd (aref cds icd))
                    (b (foreign-resource-buffer (cd-buffer cd)))
                    (value
                     (let* ((arb (foreign-resource-buffer (cd-indicators cd)))
                            (indicator (uffi:deref-array arb '(:array :short) irow)))
                       (declare (type short-array arb))
                       (unless (= indicator -1)
                         (ecase (cd-oci-data-type cd)
                           (#.SQLT-STR
                            (deref-oci-string b irow (cd-sizeof cd) encoding))
                           (#.SQLT-FLT
                            (locally
                                (declare (type double-array b))
                              (uffi:deref-array b '(:array :double) irow)))
                           (#.SQLT-INT
                            (ecase (cd-sizeof cd)
                              (4
                               (locally
                                   (declare (type int-array b))
                                 (uffi:deref-array b '(:array :int) irow)))))
                           (#.SQLT-DATE
                            (deref-oci-string b irow (cd-sizeof cd) encoding)))))))
               (when (and (eq :string (cd-result-type cd))
                          value
                          (not (stringp value)))
                   (setq value (write-to-string value)))
               (push value reversed-result)))
           (incf (qc-n-to-dbi qc))
           (nreverse reversed-result)))))

(defun refill-qc-buffers (qc)
  (with-slots (errhp) (qc-db qc)
    (setf (qc-n-to-dbi qc) 0)
    (cond ((qc-oci-end-seen-p qc)
           (setf (qc-n-from-oci qc) 0))
          (t
           (let ((oci-code (%oci-stmt-fetch
                            (deref-vp (qc-stmthp qc))
                            (deref-vp errhp)
                            +n-buf-rows+
                            +oci-fetch-next+ +oci-default+)))
             (ecase oci-code
               (#.+oci-success+ (values))
               (#.+oci-no-data+ (setf (qc-oci-end-seen-p qc) t)
                                (values))
               (#.+oci-error+ (handle-oci-error :database (qc-db qc)
                                                :nulls-ok t))))
           (uffi:with-foreign-object (rowcount 'ub4)
             (oci-attr-get (deref-vp (qc-stmthp qc))
                           +oci-htype-stmt+
                           rowcount
                           +unsigned-int-null-pointer+
                           +oci-attr-row-count+
                           (deref-vp errhp))
             (setf (qc-n-from-oci qc)
                   (- (uffi:deref-pointer rowcount 'ub4)
                      (qc-total-n-from-oci qc)))
             (when (< (qc-n-from-oci qc) +n-buf-rows+)
               (setf (qc-oci-end-seen-p qc) t))
             (setf (qc-total-n-from-oci qc)
                   (uffi:deref-pointer rowcount 'ub4)))))
    (values)))



;; the guts of the SQL function
;;
;; (like the SQL function, but with the QUERY argument hardwired to T, so
;; that the return value is always a cursor instead of a list)

;; Is this a SELECT statement?  SELECT statements are handled
;; specially by OCIStmtExecute().  (Non-SELECT statements absolutely
;; require a nonzero iteration count, while the ordinary choice for a
;; SELECT statement is a zero iteration count.

;; SELECT statements are the only statements which return tables.  We
;; don't free STMTHP in this case, but instead give it to the new
;; QUERY-CURSOR, and the new QUERY-CURSOR becomes responsible for
;; freeing the STMTHP when it is no longer needed.

(defclass <binding-info> ()
  ((user-binding-type :initarg :user-binding-type)
   (sqlt :initarg :sqlt)
   (c-value :initarg :c-value)
   (c-value-size :initarg :c-value-size))
  (:documentation "Description of bound value."))

(defun make-bi (user-binding-type sqlt c-value c-value-size)
  "Make an instance of `<binding-info>'."
  (make-instance '<binding-info> :user-binding-type user-binding-type :sqlt sqlt :c-value c-value :c-value-size c-value-size))

(defclass <oracle-stmt> ()
  ((database :initarg :database :reader database)
   (oci-stmthp :initarg :oci-stmthp :reader oci-stmthp) ; statement handle pointer
   (bindings :initform nil)
   (reusablep :initform nil :initarg :reusablep)
   (binding-types :initarg :binding-types :reader binding-types)
   (stmt-type :type integer :initarg :stmt-type)
   (selectp :initarg :selectp :reader selectp)
   (field-names :initarg :field-names :accessor stmt-field-names)
   (result-types :initarg :result-types :reader result-types)
   (rows-affected :reader rows-affected))
  (:documentation "Representation of prepared SQL statement."))

(defun prepare-statement (database sql-stmt &key reusablep)
  (with-slots (envhp svchp errhp) database
    (uffi:with-foreign-strings ((c-stmt-string sql-stmt))
      (let ((stmthp (uffi:allocate-foreign-object :pointer-void))
            selectp
            types result-types field-names)

        (uffi:with-foreign-object (stmttype :unsigned-short)
          (oci-handle-alloc (deref-vp envhp)
                            stmthp
                            +oci-htype-stmt+ 0 +null-void-pointer-pointer+)
          (oci-stmt-prepare (deref-vp stmthp)
                            (deref-vp errhp)
                            c-stmt-string
                            (uffi-foreign-string-length c-stmt-string)
                            +oci-ntv-syntax+ +oci-default+ :database database)
          (oci-attr-get (deref-vp stmthp)
                        +oci-htype-stmt+
                        stmttype
                        +unsigned-int-null-pointer+
                        +oci-attr-stmt-type+
                        (deref-vp errhp)
                        :database database)

          (setq selectp (= (uffi:deref-pointer stmttype :unsigned-short) +oci-stmt-select+))

          (make-instance '<oracle-stmt>
                         :database database
                         :oci-stmthp stmthp
                         :selectp selectp
                         :stmt-type (uffi:deref-pointer stmttype :unsigned-short)
                         :binding-types types
                         :result-types result-types
                         :field-names field-names
                         :reusablep reusablep))))))

(defun guess-object-type (value)
  (cond
    ((integerp value) :integer)
    ((floatp value) :float)
    ((stringp value)
     (if (< (length value) +maximal-varchar2-length+) :string :clob))
    (t
     (error '<dbi-programming-error>
            :message (format nil "Unsupported type of bind parameter: ~A." (type-of value))))))

(defun clsql-type->oracle-sqlt (type)
  (flet ((in (arg &rest items) (member arg items)))
    (cond
      ((in type :int :integer :short :bigint) SQLT-INT)
      ((in type :float :double :number) SQLT-FLT)
      ((in type :clob) SQLT-LNG)
      ((in type :blob) SQLT-BIN)
      ((or (in type :string)
           (and (consp type) (in (car type) :char :varchar))) SQLT-STR)
      (t
       (error '<dbi-programming-error>
              :message
              (format nil "Unknown type ~A." type))))))

(defun valuate-bound-parameter (stmt position value &key binding-type)
  "Copy new value to a bound parameter, i.e. to a uffi-allocated memory."
  (declare (ignore binding-type))
  (with-slots (bindings binding-types) stmt
    (with-slots (user-binding-type sqlt c-value c-value-size)
        (or (cdr (assoc position bindings))
            (error '<dbi-programming-error>
                   :message (format nil "Parameter :~A was not bound" position)
                   :error-code nil))
      (case user-binding-type
        ((:string :clob :blob)
         (error '<dbi-notsupported-error>
                :message "Unable to change the value of a bound variable-length parameter."
                :error-code -1))
        ((:int :integer :short :bigint)
         (setf (uffi:deref-pointer c-value :int) value))
        ((:float :double :numeric)
         (setf (uffi:deref-pointer c-value :double) value))
        (t
         (error '<dbi-programming-error> :message "Unsupported bind parameter type." :error-code nil)))
      t)))

(defun bind-parameter (stmt position value &key binding-type)
  (check-type stmt <oracle-stmt>)
  (with-slots (database id oci-stmthp bindings binding-types field-names result-types) stmt
    (with-slots (envhp svchp errhp) database
      (let* ((bindp (uffi:allocate-foreign-object :pointer-void))
             c-value c-value-size
             (user-binding-type (or binding-type
                                    (and binding-types (elt binding-types (1- position)))
                                    (guess-object-type value)))  ; user-suplied type of value
             (sqlt (clsql-type->oracle-sqlt user-binding-type))) ; Oracle's type for DBI binding-type
        (case user-binding-type
          ((:string)
           (setf c-value (uffi:convert-to-foreign-string value)
                 c-value-size (1+ (uffi-foreign-string-length c-value))))
          ((:clob) ; just as string, but do not count zero symbol
           (setf c-value (uffi:convert-to-foreign-string value)
                 c-value-size (uffi-foreign-string-length c-value)))
          (:blob
           (setf c-value-size (length value)
                 c-value (uffi:allocate-foreign-object :unsigned-char c-value-size))
           (dotimes (i c-value-size)
             (setf (uffi:deref-array c-value '(:array :unsigned-char) i) (elt value i))))
          ((:int :integer :short :bigint)
           (setf c-value (uffi:allocate-foreign-object :int)
                 c-value-size 4) ; sizeof(int)
           (setf (uffi:deref-pointer c-value :int) value))
          ((:float :double :numeric)
           (setf c-value (uffi:allocate-foreign-object :double)
                 c-value-size 8) ; sizeof(double)
           (setf (uffi:deref-pointer c-value :double) value))
          (t
           (error '<dbi-database-error>
                  :message
                  (format nil "Unsupported bind parameter type."))))

        (setf (deref-vp bindp) +null-void-pointer+)
        (oci-bind-by-pos (deref-vp oci-stmthp)
                         bindp
                         (deref-vp errhp)
                         position
                         c-value
                         c-value-size
                         sqlt ;;-- type of value
                         +null-void-pointer+ ;; indp
                         +unsigned-short-null-pointer+ ;; alenp
                         +unsigned-short-null-pointer+ ;; rcodep
                         0 ;; maxarr_len
                         +unsigned-int-null-pointer+ ;; curelep
                         +oci-default+ :database database)

        (push (cons position (make-bi user-binding-type sqlt c-value c-value-size)) bindings))
      t)))

(defun bind-or-valuate-parameter (stmt position value &key binding-type)
  (check-type stmt <oracle-stmt>)
  (with-slots (bindings) stmt
    (if (assoc position bindings)
        (if (slot-value stmt 'reusablep)
            (valuate-bound-parameter stmt position value :binding-type binding-type)
            (error '<dbi-programming-error>
                   :message "Statement is not reusable, please call PREPARE aagain."
                   :error-code -1))
        (bind-parameter stmt position value :binding-type binding-type))))


(defun sql-prepared-stmt-exec (stmt database result-types field-names)
  "Return cursor if statement is a select query and NIL otherwise."
  (with-slots (oci-stmthp selectp rows-affected stmt-type reusablep) stmt
    (with-slots (envhp svchp errhp) database
      (unwind-protect
           (let ((iters (if selectp 0 1)))
             (oci-stmt-execute (deref-vp svchp)
                               (deref-vp oci-stmthp)
                               (deref-vp errhp)
                               iters 0 +null-void-pointer+ +null-void-pointer+ +oci-default+
                               :database database)
             ;; save number of rows affected by an insert/update/delete statement
             (when (member stmt-type (list +oci-stmt-update+ +oci-stmt-delete+ +oci-stmt-insert+))
               (uffi:with-foreign-object (rows-affected 'ub4)
                 (oci-attr-get (deref-vp oci-stmthp)
                               +oci-htype-stmt+
                               rows-affected
                               +unsigned-int-null-pointer+
                               +oci-attr-row-count+
                               (deref-vp errhp)
                               :database database)
                 (setf (slot-value stmt 'rows-affected)
                       (uffi:deref-pointer rows-affected 'ub4)))))
        ;; free resources unless a query, or a reusable statement
        (unless (or selectp reusablep)
          (release-resources stmt)
          #+nil(oci-handle-free (deref-vp oci-stmthp) +oci-htype-stmt+)
          #+nil(uffi:free-foreign-object oci-stmthp)))
      (cond
        (selectp
         (make-query-cursor database oci-stmthp result-types field-names))
        (t
         nil)))))


;; Return a QUERY-CURSOR representing the table returned from the OCI
;; operation done through STMTHP.  TYPES is the argument of the same
;; name from the external SQL function, controlling type conversion
;; of the returned arguments.

(defun make-query-cursor (db stmthp result-types field-names)
  (let ((qc (%make-query-cursor :db db
                                :stmthp stmthp
                                :cds (make-query-cursor-cds db stmthp
                                                            result-types
                                                            field-names))))
    (refill-qc-buffers qc)
    qc))


;; the hairy part of MAKE-QUERY-CURSOR: Ask OCI for information
;; about table columns, translate the information into a Lisp
;; vector of column descriptors, and return it.

;; Allegro defines several flavors of type conversion, but this
;; implementation only supports the :AUTO flavor.

;; A note of explanation: OCI's internal number format uses 21
;; bytes (42 decimal digits). 2 separate (?) one-byte fields,
;; scale and precision, are used to deduce the nature of these
;; 21 bytes. See pp. 3-10, 3-26, and 6-13 of OCI documentation
;; for more details.

;; Mac OS X Note: According to table 6-8 in the Oracle 9i OCI
;; documentation, PRECISION may actually be an sb2 instead of a
;; single byte if performing an "implicit describe".  Using a
;; signed short instead of an unsigned byte fixes a Mac OS X bug
;; where PRECISION is always zero. -- JJB 20040713

;; When calling OCI C code to handle the conversion, we have
;; only two numeric types available to pass the return value:
;; double-float and signed-long. It would be possible to
;; bypass the OCI conversion functions and write Lisp code
;; which reads the 21-byte field directly and decodes
;; it. However this is left as an exercise for the reader. :-)

;; The following table describes the mapping, based on the implicit
;; assumption that C's "signed long" type is a 32-bit integer.
;;
;;   Internal Values                     SQL Type        C Return Type
;;   ===============                     ========        =============
;;   Precision > 0        SCALE = -127   FLOAT       --> double-float
;;   Precision > 0 && <=9 SCALE = 0      INTEGER     --> signed-long
;;   Precision = 0 || > 9 SCALE = 0      BIG INTEGER --> double-float
;;   Precision > 0        SCALE > 0      DECIMAL     --> double-float

;; (OCI uses 1-based indexing here.)

;; KLUDGE: This should work for all other data types except those
;; which don't actually fit in their fixed-width field (BLOBs and the
;; like). As Winton says, we (Cadabra) don't need to worry much about
;; those, since we can't reason with them, so we don't use them. But
;; for a more general application it'd be good to have a more
;; selective and rigorously correct test here for whether we can
;; actually handle the given DEREF-DTYPE value. -- WHN 20000106

;; Note: The OCI documentation doesn't seem to say whether the COLNAME
;; value returned here is a newly-allocated copy which we're
;; responsible for freeing, or a pointer into some system copy which
;; will be freed when the system itself is shut down.  But judging
;; from the way that the result is used in the cdemodsa.c example
;; program, it looks like the latter: we should make our own copy of
;; the value, but not try to free it.

;; WORKAROUND: OCI seems to return ub2 values for the
;; +oci-attr-data-size+ attribute even though its documentation claims
;; that it returns a ub4, and even though the associated "sizep" value
;; is 4, not 2.  In order to make the code here work reliably, without
;; having to patch it later if OCI is ever fixed to match its
;; documentation, we pre-zero COLSIZE before making the call into OCI.

;; To exercise the weird OCI behavior (thereby blowing up the code
;; below, beware!) try setting this value into COLSIZE, calling OCI,
;; then looking at the value in COLSIZE.  (setf colsize #x12345678)
;; debugging only

;; Mac OS X Note: This workaround fails on a bigendian platform so
;; I've changed the data type of COLNAME to :unsigned-short as per
;; the Oracle 9i OCI documentation. -- JJB 20040713

(uffi:def-type byte-pointer (* :byte))
(uffi:def-type void-pointer-pointer (* :void-pointer))

(defun make-query-cursor-cds (database stmthp result-types field-names)
  (declare (optimize #+:debug (safety 3) #-debug (speed 3))
           (type <oracle-database> database)
           (type pointer-pointer-void stmthp))
  (with-slots (errhp) database
    (uffi:with-foreign-objects ((dtype-foreign :unsigned-short)
                                (parmdp :pointer-void)
                                (precision :short)
                                (scale :byte)
                                (colname '(* :unsigned-char))
                                (colnamelen 'ub4)
                                (colsize 'ub2)
                                (defnp ':pointer-void))
      (let ((buffer nil)
            (sizeof nil))
        (do ((icolumn 0 (1+ icolumn))
             (cds-as-reversed-list nil))
            ((not (eql (oci-param-get (deref-vp stmthp)
                                      +oci-htype-stmt+
                                      (deref-vp errhp)
                                      parmdp
                                      (1+ icolumn) :database database)
                       +oci-success+))
             (coerce (reverse cds-as-reversed-list) 'simple-vector))
          ;; Decode type of ICOLUMNth column into a type we're prepared to
          ;; handle in Lisp.
          (oci-attr-get (deref-vp parmdp)
                        +oci-dtype-param+
                        dtype-foreign
                        +unsigned-int-null-pointer+
                        +oci-attr-data-type+
                        (deref-vp errhp))
          (let ((dtype (uffi:deref-pointer dtype-foreign :unsigned-short)))
            (declare (fixnum dtype))
            (case dtype
              (#.SQLT-DATE
               (setf buffer (acquire-foreign-resource :unsigned-char
                                                      (* 32 +n-buf-rows+)))
               (setf sizeof 32 dtype #.SQLT-STR))
              (#.SQLT-NUMBER
               (oci-attr-get (deref-vp parmdp)
                             +oci-dtype-param+
                             precision
                             +unsigned-int-null-pointer+
                             +oci-attr-precision+
                             (deref-vp errhp))
               (oci-attr-get (deref-vp parmdp)
                             +oci-dtype-param+
                             scale
                             +unsigned-int-null-pointer+
                             +oci-attr-scale+
                             (deref-vp errhp))
               (let ((*scale (uffi:deref-pointer scale :byte))
                     (*precision (uffi:deref-pointer precision :short)))
                 ;; (format t "scale=~d, precision=~d~%" *scale *precision)

                 ;; NUMBER fields, specified by scale=-127 and precision=0, can store
                 ;; any numeric values, including real values. For this reason such
                 ;; fields should be treated as floating point.
                 (cond
                  ((or ; (and (minusp *scale) (zerop *precision))
                       (and (zerop *scale) (plusp *precision)))
                   (setf buffer (acquire-foreign-resource :int +n-buf-rows+)
                         sizeof 4                       ;; sizeof(int)
                         dtype #.SQLT-INT))
                  (t
                   (setf buffer (acquire-foreign-resource :double +n-buf-rows+)
                         sizeof 8                   ;; sizeof(double)
                         dtype #.SQLT-FLT)))))
              ;; Default to SQL-STR
              (t
               (setf (uffi:deref-pointer colsize :unsigned-short) 0)
               (setf dtype #.SQLT-STR)
               (oci-attr-get (deref-vp parmdp)
                             +oci-dtype-param+
                             colsize
                             +unsigned-int-null-pointer+
                             +oci-attr-data-size+
                             (deref-vp errhp))
               (let ((colsize-including-null (1+ (uffi:deref-pointer colsize :unsigned-short))))
                 (setf buffer (acquire-foreign-resource
                               :unsigned-char (* +n-buf-rows+ colsize-including-null)))
                 (setf sizeof colsize-including-null))))
            (let ((retcodes (acquire-foreign-resource :unsigned-short +n-buf-rows+))
                  (indicators (acquire-foreign-resource :short +n-buf-rows+))
                  (colname-string ""))
              (when field-names
                (oci-attr-get (deref-vp parmdp)
                              +oci-dtype-param+
                              colname
                              colnamelen
                              +oci-attr-name+
                              (deref-vp errhp))
                (setq colname-string (uffi:convert-from-foreign-string
                                      (uffi:deref-pointer colname '(* :unsigned-char))
                                      :length (uffi:deref-pointer colnamelen 'ub4)
                                      :encoding (encoding database))))
              (push (make-cd :name colname-string
                             :sizeof sizeof
                             :buffer buffer
                             :oci-data-type dtype
                             :retcodes retcodes
                             :indicators indicators
                             :result-type (cond
                                           ((consp result-types)
                                            (nth icolumn result-types))
                                           ((null result-types)
                                            :string)
                                           (t
                                            result-types)))
                    cds-as-reversed-list)
              (oci-define-by-pos (deref-vp stmthp)
                                 defnp
                                 (deref-vp errhp)
                                 (1+ icolumn) ; OCI 1-based indexing again
                                 (foreign-resource-buffer buffer)
                                 sizeof
                                 dtype
                                 (foreign-resource-buffer indicators)
                                 +unsigned-short-null-pointer+
                                 (foreign-resource-buffer retcodes)
                                 +oci-default+))))))))

;; Release the resources associated with a QUERY-CURSOR.

(defun close-query (qc)
  (oci-handle-free (deref-vp (qc-stmthp qc)) +oci-htype-stmt+)
  (uffi:free-foreign-object (qc-stmthp qc))
  (let ((cds (qc-cds qc)))
    (dotimes (i (length cds))
      (release-cd-resources (aref cds i))))
  (values))


;; Release the resources associated with a column description.

(defun release-cd-resources (cd)
  (free-foreign-resource (cd-buffer cd))
  (free-foreign-resource (cd-retcodes cd))
  (free-foreign-resource (cd-indicators cd))
  (values))



;;; Do the database operation described in SQL-STMT-STRING on database
;;; DB and, if the command is a SELECT, return a representation of the
;;; resulting table. The representation of the table is controlled by the
;;; QUERY argument:
;;;   * If QUERY is NIL, the table is returned as a list of rows, with
;;;     each row represented by a list.
;;;   * If QUERY is non-NIL, the result is returned as a QUERY-CURSOR
;;;     suitable for FETCH-ROW and CLOSE-QUERY
;;; The TYPES argument controls the type conversion method used
;;; to construct the table. The Allegro version supports several possible
;;; values for this argument, but we only support :AUTO.
;;;
;;; Process-cursor is a helper function what does all the work.

(defun process-cursor (cursor database field-names &key (fetch-size 1))
  ;; (declare (type (or query-cursor null) cursor))
  (flet ((format-result (reversed-result)
           (loop for row in (nreverse reversed-result)
              collect (if field-names
                          (loop for cd across (qc-cds cursor)
                             and data in row
                             nconc (list (intern (cd-name cd) :keyword)
                                         data))
                          row))))
    (if (null cursor) ; No table was returned.
        (values)
        (let ((reversed-result nil)
              (eof-value :eof))
          (dotimes (count fetch-size)
            (let ((row (fetch-row cursor nil eof-value (encoding database))))
              (when (eq row eof-value)
                (close-query cursor)
                (return))
              (push row reversed-result)))
          (format-result reversed-result)))))


(defun get-affected-rows-count (stmt database)
  "Returns the number of rows processed by an INSERT/DELETE/UPDATE
statement or NIL, if the statement execuded something else."
  (with-slots (envhp svchp errhp) database
    (with-slots (oci-stmthp stmt-type) stmt
      (when (member stmt-type (list +oci-stmt-select+ +oci-stmt-update+ +oci-stmt-delete+ +oci-stmt-insert+))
        (uffi:with-foreign-object (rows-affected 'ub4)
          (oci-attr-get (deref-vp oci-stmthp)
                        +oci-htype-stmt+
                        rows-affected
                        +unsigned-int-null-pointer+
                        +oci-attr-row-count+
                        (deref-vp errhp)
                        :database database)
          (uffi:deref-pointer rows-affected 'ub4))))))

;; The following two functions are used in oracle-connect only.
(defun database-query (query-expression database result-types field-names)
  (declare (ignore field-names))
  (let* ((prepared (prepare-statement database query-expression))
         (cursor (sql-prepared-stmt-exec prepared database result-types nil)))
    (process-cursor cursor database nil)))

(defun database-execute-command (sql-expression database)
  (database-query sql-expression database nil nil)
  t)


(defun variable-length-type (type)
  (member type '(:string :clob :blob)))

(defgeneric release-resources (stmt)
  (:documentation "Release all uffi-allocated resources associated to the statement.")
  (:method ((stmt <oracle-stmt>))
    (with-slots (oci-stmthp selectp bindings database) stmt
      (with-slots (envhp svchp errhp) database
        (oci-handle-free (deref-vp oci-stmthp) +oci-htype-stmt+)
        (uffi:free-foreign-object oci-stmthp))
      ;; Deallocate memory used for passing bound parameters values
      (loop for (nil . bi) in bindings
         do (with-slots (user-binding-type c-value) bi
              ;; Both free version are essentially the same?
              (if (variable-length-type user-binding-type)
                  (uffi:free-cstring c-value)
                  (uffi:free-foreign-object c-value)))))
    t))
