      *****************************************************************
      * CPY-PARAM  Tabla de parametrizacion contable en memoria.     *
      *            La llena el subprograma de BD (TSQL001AD) leyendo *
      *            TABLA_PARAMETROS y la consulta el principal con   *
      *            PERFORM ... por PAR-COD-MOTIVO.                    *
      *                                                             *
      * Convencion: prefijo PAR- + nombre de la columna de la tabla *
      * TABLA_PARAMETROS (guiones en vez de guiones bajos).         *
      *   PAR-COD-MOTIVO          <- COD_MOTIVO                     *
      *   PAR-TIPO-ENTRADA        <- TIPO_ENTRADA                   *
      *   PAR-CUENTA-CONTABLE     <- CUENTA_CONTABLE                *
      *   PAR-NOMBRE-CUENTA       <- NOMBRE_CUENTA                  *
      *   PAR-OFICINA-AFECTACION  <- OFICINA_AFECTACION             *
      *****************************************************************
       01  TB-PARAMETROS.
           05 PAR-ITEM OCCURS 100 TIMES INDEXED BY IDX-PAR.
              10 PAR-COD-MOTIVO         PIC X(02).
              10 PAR-TIPO-ENTRADA       PIC X(15).
              10 PAR-CUENTA-CONTABLE    PIC X(12).
              10 PAR-NOMBRE-CUENTA      PIC X(30).
              10 PAR-OFICINA-AFECTACION PIC X(03).
       01  WS-COUNT-PARAM     PIC 9(03) VALUE ZEROES.
