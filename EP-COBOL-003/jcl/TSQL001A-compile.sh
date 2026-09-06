#!/bin/bash

# Ejecutable a construir
PGM=TSQL001A

# Fuente principal: COBOL PURO (SD/SORT/RELEASE/RETURN).
# NO pasa por gixpp: el analizador lexico del precompilador se
# rompe con las sentencias nativas de ordenamiento de COBOL.
SRC_MAIN=TSQL001A

# Subprograma de BD: unico fuente con EXEC SQL -> pasa por gixpp.
SRC_DB=TSQL001AD

# GixSQL Libraries
GIXSQL_HOME="/usr"
LOADLIB="$GIXSQL_HOME/lib"
export PATH=$PATH:$GIXSQL_HOME/bin

# Copy Libraries
COBCOPY="../cpy"
SQLCOPY="$GIXSQL_HOME/share/gixsql/copy"

# Limpiar versiones anteriores
rm -f ../bin/$PGM ../tcbl/$SRC_MAIN.cbl ../tcbl/$SRC_DB.cbl

# 1) Precompilar SOLO el subprograma de BD (EXEC SQL -> CALLs GixSQL)
gixpp -e -S -I $SQLCOPY -I $COBCOPY -i ../cbl/$SRC_DB.sqb -o ../tcbl/$SRC_DB.cbl

# 2) El principal va tal cual: COBOL puro, solo lo procesa cobc
cp ../cbl/$SRC_MAIN.cbl ../tcbl/$SRC_MAIN.cbl

# Pausa para revisar el resultado del preprocesador
read -p "Press any key to resume"

# 3) Compilar principal + subprograma en un unico ejecutable
cobc -x ../tcbl/$SRC_MAIN.cbl ../tcbl/$SRC_DB.cbl \
  -I $SQLCOPY \
  -I $COBCOPY \
  -L $LOADLIB \
  -l gixsql \
  -o ../bin/$PGM

# Codigo de retorno
if [ "$?" -eq 0 ]; then
  echo "SUCCESS: Compile Return code is ZERO."
else
  echo "FAIL: Compile Return code is NOT ZERO."
fi
