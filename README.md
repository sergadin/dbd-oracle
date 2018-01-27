# dbd-oracle: Oracle database driver for CL-DBI

[![Build Status](https://travis-ci.org/sergadin/dbd-oracle.svg?branch=master)](https://travis-ci.org/sergadin/dbd-oracle)
[![Coverage Status](https://coveralls.io/repos/github/sergadin/dbd-oracle/badge.svg?branch=master)](https://coveralls.io/github/sergadin/dbd-oracle?branch=master)
[![Quicklisp](http://quickdocs.org/badge/dbd-oracle.svg)](http://quickdocs.org/dbd-oracle/)

This driver is based on OCI bindings developed for CLSQL.

## Usage

This library provides Oracle interface to CL-DBI, so any
CL-DBI-complaint program should work. Please, note the following
limitations.

### Connecting to ORACLE database

Connection can be estabslished by providing appropriate database name
to `dbi:connect` method. The value of `database-name` key argument is an
ORACLE connect string that might include hostname, listener port
number, and oracle SID or service name. For example,

```common-lisp
(defvar *connection*
  (dbi:connect :oracle
               :database-name "127.0.0.1:1521/orcl"
               :username "nobody"
               :password "1234"
               :encoding :utf-8))
```

Encoding parameter is used for conversion of character strings between
OCI library and Lisp. :utf-8 is the default encoding. Possible values
for encoding parameter is the same as for CFFI string conversion
functions. If :encoding keyword is not fiven, the library accesses
NLS_LANG enironment variable. Note, that Oracle encoding names
appearing in NLS_LANG may not match the names used in CFFI.


### Loading OCI library

The application makes an attempt to load Oracle OCI library during the
first call to `dbi:connect`. By default, the OCI library is searched in
ORACLE_HOME, ORACLE_HOME/lib, and ORACLE_HOME/bin directories, as well
as in the current directory. If ORACLE_HOME environment variable is
not set, then search paths can be provided by setting
`dbd.oracle:*foreign-library-search-paths*` variable. The value
assigned to this variable should be a list of pathnames. For example,

```common-lisp
  (let* ((dbd.oracle:*foreign-library-search-paths* '(#p"/opt/oracle/"))
         (connection (dbi:connect :oracle
                                  :database-name "localhost/orcl"
                                  :username "nobody"
                                  :password "1234")))
     (dbi:disconect connection))
```

You may need to create a symbolic link for `libclntsh.so` pointing to
an appropriate library, e.g. `libclntsh.so.12.1`.

### Writing queries: space before question

Queries are evaluated using CL-DBI interface. Parameters bindings is
not guaranteed to work properly due to incompatible syntax for
placeholders definitions used in CL-DBI and Oracle queries. Oracle
requires parameter name to start with a colon, while CL-DBI uses the
question mark `?` to define placeholders. Simple substitution of *all*
occurrences of " ?" (a space followed by a question mark) by
Oracle-friendly column expressions is performed for the entire SQL
expression.

```common-lisp
(defvar *connection*
  (dbi:connect :mysql
               :database-name "test"
               :username "nobody"
               :password "1234"))

(let* ((query (dbi:prepare *connection*
                           "SELECT * FROM somewhere WHERE flag = ? OR updated_at > ?"))
       (result (dbi:execute query 0 "2011-11-01")))
  (loop for row = (dbi:fetch result)
     while row
     ;; process "row".
       ))
```

The effectve query would be
`"SELECT * FROM somewhere WHERE flag = :1 OR updated_at > :2"`.
If a question mark appears inside a quoted string of the SQL query,
then the effective query could not be identical to the original
one. The rule of thumb is to avoid using French punctuation style in
literal constants inside the main body of SQL queries (bound strings
are processed as is), and to put space before every placeholder. Automatic
substitution of question marks may be disabled by setting
`format-placeholders` key parameter of `dbi:connect` to NIL.

## Support for CLOB fields

CLOB fields may be used in INSERT operations.

```lisp
(let ((long-string (make-string 22000 :initial-element #\a))
      (query (dbi:prepare "INSERT INTO tbl (clob_field) VALUES ( ?)")))
  (dbi:execute query long-string))
```

You can not SELECT the value back, as ORACLE strips CLOB values to a
predefined limit, which is 4000 for modern versions of ORACLE.

## Extensions to DBI interface

The library supports single parse, multiple bind/executes loop for a statement.

```lisp
(with-reusable-query (query connection "INSERT INTO tbl (k) VALUES ( ?)")
  (loop for k from 1 to 100
    do (dbi:execute query k)))
```

This scheme may be more efficient when evaluating large number of
queries. Macro `with-reusable-query` evaluates its body with `query`
being prepared SQL expressions, and ensures that all resources
allocated for the query will be released at the end.

## Testing

The library was manually tested using Oracle 11g server under following client configurations:

* 64-bit Clozure CL 1.9 under Windows 7 using 64-bit version of Oracle instant client 12.1.0.1
* 64-bit SBCL 1.3.2 under Ubuntu 14.04 64-bit using Oracle instant client 12.1.0.1
* 32-bit LispWorks Personal Edition on Mac OS X 10.10 using 32-bit version of Oracle instant client 11.2.0.4

Automated testing uses SBCL under Linux, and Oracle XE version
11.2.

Testig procedure requires that the databse contain a table

```SQL
CREATE TABLE realval (
    r REAL,
    n NUMBER,
    ns NUMBER(*, 5),
    nps NUMBER(10, 2),
    i INTEGER
);
INSERT INTO realval VALUES (17.17, 17.17, 17.17, 17.17, 17);
```

In order to run tests locally, you need to modify connection-related
parameters in `test/root.lisp`, or modify your environment in
accordance with the default values:

```lisp
(deftestsuite root ()
  ((user-name "scott")
   (password "tiger")
   (connect-string (let ((host "127.0.0.1")
                         (port "1521")
                         (oracle-sid (or (dbd.oracle::getenv "ORACLE_SID") "orcl")))
                     (format nil "~A:~A/~A" host port oracle-sid))))
  (:dynamic-variables
   (dbd.oracle:*foreign-library-search-paths* '(#p"/opt/oracle/")))
  ...)
```

Tests can be started using
```lisp
(asdf:test-system "dbd-oracle")
```

## License

Lisp Lesser GNU Public License
