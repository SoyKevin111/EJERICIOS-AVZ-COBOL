      *****************************************************************
      * CPY-MOV  Registro de movimiento que el subprograma de BD     *
      *          (TSQL001AD) entrega, de a un registro por CALL, al  *
      *          programa principal (TSQL001A).                      *
      *                                                             *
      * Convencion: prefijo MOV- + nombre de la columna de la tabla *
      * TABLA_MOVIMIENTOS (guiones en vez de guiones bajos).        *
      *   MOV-FECHA-MOV     <- FECHA_MOV                            *
      *   MOV-HORA-MOV      <- HORA_MOV                             *
      *   MOV-COD-CLIENTE   <- COD_CLIENTE                          *
      *   MOV-COD-MOTIVO    <- COD_MOTIVO                           *
      *   MOV-VALOR         <- VALOR                                *
      *   MOV-OFICINA       <- OFICINA                              *
      *****************************************************************
       01  MOV-REG.
           05 MOV-FECHA-MOV    PIC X(10).
           05 MOV-HORA-MOV     PIC X(08).
           05 MOV-COD-CLIENTE  PIC X(06).
           05 MOV-COD-MOTIVO   PIC X(02).
           05 MOV-VALOR        PIC S9(13)V99.
           05 MOV-OFICINA      PIC X(03).
