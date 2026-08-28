#!/bin/bash

# Executable to build
PGM=TSQL001A

# Sources (.sqb). Main program FIRST, then subprograms.
SRC_MAIN=TSQL001AC
SRC_SUBS="TSQL001AC1"

# GixSQL Libraries
GIXSQL_HOME="/usr"
LOADLIB="$GIXSQL_HOME/lib"
export PATH=$PATH:$GIXSQL_HOME/bin

# Copy Libraries
COBCOPY="../cpy"
SQLCOPY="$GIXSQL_HOME/share/gixsql/copy"

# All sources: main first, then subprograms
SRC_ALL="$SRC_MAIN $SRC_SUBS"

# Remove old versions
rm -f ../bin/$PGM
for SRC in $SRC_ALL; do
  rm -f ../tcbl/$SRC.cbl
done

# GixSQL Prep and Bind
for SRC in $SRC_ALL; do
  gixpp -e -S -I $SQLCOPY -I $COBCOPY -i ../cbl/$SRC.sqb -o ../tcbl/$SRC.cbl
done

# Pause to check the results
read -p "Press any key to resume"

# Compile the program (main + subprograms into one executable)
TCBL_ALL=""
for SRC in $SRC_ALL; do
  TCBL_ALL="$TCBL_ALL ../tcbl/$SRC.cbl"
done

cobc -x $TCBL_ALL \
  -I $SQLCOPY \
  -I $COBCOPY \
  -L $LOADLIB \
  -l gixsql \
  -o ../bin/$PGM

# Check return code
if [ "$?" -eq 0 ]; then
  echo "SUCCESS: Compile Return code is ZERO."
else
  echo "FAIL: Compile Return code is NOT ZERO."
fi
