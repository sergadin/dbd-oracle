#!/bin/bash

sudo mkdir /opt/oracle
cd $ORACLE_HOME/lib

ls -la

sudo ln -s libclntsh.so.11.2 libclntsh.so
sudo ln -s libocci.so.11.2 libocci.so
sudo ln -s liboci.so.11.2 liboci.so
for f in `ls ./*.so*`
do
   sudo ln -s $ORACLE_HOME/lib/$f /opt/oracle/$f
done
