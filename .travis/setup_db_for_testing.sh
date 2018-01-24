#!/bin/sh -e

#
# Creatre Oracle user and populate his schema with data for testing
#

$ORACLE_HOME/bin/sqlplus / as sysdba <<EOF
create user scott identified by tiger
/

grant connect to scott
/

grant resource to scott
/

quit
/
EOF

$ORACLE_HOME/bin/sqlplus scott/tiger <<EOF
drop table realval
/

create table realval (
    r real,
    n number,
    ns number(*, 5),
    nps number(10, 2),
    i integer
)
/

insert into realval values (17.17, 17.17, 17.17, 17.17, 17)
/

commit
/

quit
/
EOF

exit 0
