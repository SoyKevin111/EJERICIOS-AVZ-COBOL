#!/bin/bash

# Program to run
PGM=TSQL001A

# GixSQL Load Libraries
LD_LIBRARY_PATH="/opt/gixsql/lib"


# Export the environment variables in the .env file
export $(grep -v '^#' ../.env | xargs)

# Zona horaria local: el contenedor corre en UTC y FUNCTION CURRENT-DATE
# (FECHA PROC) tomaba la fecha de UTC, adelantando un dia por la noche.
export TZ='America/Guayaquil'

# Run it
../bin/$PGM