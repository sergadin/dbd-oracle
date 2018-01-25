#!/bin/bash -ev

#
# Creatre Oracle user and populate his schema with data for testing
#

$ORACLE_HOME/bin/sqlplus / as sysdba <<EOF
CREATE USER scott IDENTIFIED BY tiger
/

GRANT connect, resource TO scott
/
EOF

$ORACLE_HOME/bin/sqlplus scott/tiger <<EOF
DROP TABLE realval
/

CREATE TABLE realval (
    r REAL,
    n NUMBER,
    ns NUMBER(*, 5),
    nps NUMBER(10, 2),
    i INTEGER
)
/

INSERT INTO realval VALUES (17.17, 17.17, 17.17, 17.17, 17)
/

COMMIT
/
EOF
