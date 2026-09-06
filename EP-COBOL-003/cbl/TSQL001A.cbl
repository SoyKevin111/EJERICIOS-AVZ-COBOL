       IDENTIFICATION DIVISION.

       PROGRAM-ID. TSQL001A.
      *----------------------------------------------------------------*
      * PROGRAMA PRINCIPAL  ---  COBOL PURO                            *
      *                                                              *
      * NO pasa por el precompilador SQL (gixpp).  Conserva intacto   *
      * el clasificador interno de COBOL:                             *
      *     SD SORT-FILE / SORT ... INPUT PROCEDURE OUTPUT PROCEDURE  *
      *     RELEASE REG-SORT / RETURN SORT-FILE                       *
      * Toda la Base de Datos vive en el subprograma TSQL001AD (.sqb),*
      * que entrega los registros de a uno por CALL.  Asi el          *
      * clasificador procesa registro por registro sin cargar todos   *
      * los movimientos en memoria.                                   *
      *----------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORTE  ASSIGN TO './REPORTE1'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-REPORTE.
           SELECT REPERROR ASSIGN TO './REPORTE2'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-ERROR.
           SELECT SORT-FILE ASSIGN TO SORTWK1.

       DATA DIVISION.
       FILE SECTION.
       FD  REPORTE.
       01  REG-REPORTE PIC X(133).

       FD  REPERROR.
       01  REG-ERROR PIC X(133).

       SD  SORT-FILE.
       01  REG-SORT.
           05 SRT-CUENTA       PIC X(12).
           05 SRT-OFICINA      PIC X(03).
           05 SRT-TIPO-ENTRADA PIC X(15).
           05 SRT-FECHA        PIC X(10).
           05 SRT-HORA         PIC X(08).
           05 SRT-CLIENTE      PIC X(06).
           05 SRT-MOTIVO       PIC X(02).
           05 SRT-VALOR        PIC S9(13)V99.
           05 SRT-NOMBRE-CTA   PIC X(30).

       WORKING-STORAGE SECTION.

      * ESTADOS DE ARCHIVO
       01  FS-REPORTE          PIC X(02) VALUE '00'.
       01  FS-ERROR            PIC X(02) VALUE '00'.

      * REGISTRO DE MOVIMIENTO QUE DEVUELVE EL SUBPROGRAMA DE BD
           COPY CPY-MOV.

      * TABLA DE PARAMETROS EN MEMORIA
           COPY CPY-PARAM.

      * CODIGO DE RETORNO SQL DEVUELTO POR EL SUBPROGRAMA
       01  WS-SQLCODE          PIC S9(09) COMP-5 VALUE ZEROES.

      * CONTADORES / ACUMULADORES DEL CUADRE
       01  WS-TOTAL-PARTIDAS   PIC S9(15)V99 VALUE ZEROES.
       01  WS-TOTAL-CONTRAPART PIC S9(15)V99 VALUE ZEROES.
       01  WS-DIFERENCIA       PIC S9(15)V99 VALUE ZEROES.
       01  WS-SUBTOT-CUENTA    PIC S9(15)V99 VALUE ZEROES.
       01  WS-CONT-LEIDOS      PIC 9(08) VALUE ZEROES.
       01  WS-CONT-VALIDOS     PIC 9(08) VALUE ZEROES.
       01  WS-CONT-ASIENTOS    PIC 9(08) VALUE ZEROES.
       01  WS-CONT-RECHAZADOS  PIC 9(08) VALUE ZEROES.
       01  WS-CONT-SIN-PARAM   PIC 9(08) VALUE ZEROES.
       01  WS-ESTADO-CUADRE    PIC X(11) VALUE SPACES.
       01  WS-CUENTA-ANTERIOR  PIC X(12) VALUE SPACES.
       01  WS-FIN-MOVIMIENTOS  PIC X(01) VALUE 'N'.

      * INDICES DE LA PARTIDA Y LA CONTRAPARTIDA DEL MOTIVO EN CURSO
       01  WS-IDX-PARTIDA     PIC 9(03) VALUE ZEROES.
       01  WS-IDX-CONTRA      PIC 9(03) VALUE ZEROES.
       01  WS-SUB            PIC 9(03) VALUE ZEROES.

      * EDICION PARA EL CUADRE
       01  WS-EDT-IMPORTE    PIC Z(14)9.99.
       01  WS-EDT-DIFEREN    PIC -(14)9.99.

      * ---- LISTADO DE ERRORES / INCONSISTENCIAS  (REPORTE2) ---------
       01  WS-TIPO-ERROR      PIC X(28) VALUE SPACES.
       01  WS-CAB-ERR.
           05 FILLER  PIC X(01) VALUE SPACES.
           05 FILLER  PIC X(12) VALUE 'FECHA'.
           05 FILLER  PIC X(10) VALUE 'HORA'.
           05 FILLER  PIC X(08) VALUE 'CLIENTE'.
           05 FILLER  PIC X(04) VALUE 'MOT'.
           05 FILLER  PIC X(05) VALUE 'OFI'.
           05 FILLER  PIC X(14) VALUE '         VALOR'.
           05 FILLER  PIC X(02) VALUE SPACES.
           05 FILLER  PIC X(13) VALUE 'TIPO DE ERROR'.
       01  WS-LINEA-ERROR.
           05 FILLER            PIC X(01) VALUE SPACES.
           05 WE-FECHA          PIC X(10).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 WE-HORA           PIC X(08).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 WE-CLIENTE        PIC X(06).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 WE-MOTIVO         PIC X(02).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 WE-OFICINA        PIC X(03).
           05 FILLER            PIC X(02) VALUE SPACES.
           05 WE-VALOR          PIC -(11)9.99.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 WE-TIPO-ERROR     PIC X(28).

      * ---- REPORTE CONTABLE PAGINADO  (REPORTE1) -------------------
       01  WS-SEP             PIC X(70) VALUE ALL '-'.
       01  WS-PAGINA         PIC 9(03) VALUE ZEROES.
       01  WS-PAGINA-ED      PIC ZZ9.
       01  WS-LINEAS         PIC 9(03) VALUE ZEROES.
       01  WS-MAX-LINEAS     PIC 9(03) VALUE 60.
       01  WS-FECHA-PROC     PIC X(10) VALUE SPACES.
       01  WS-FECHA-MOV-REP  PIC X(15) VALUE SPACES.
       01  WS-CURR           PIC X(21).
       01  WS-IMP-CTA        PIC Z(12)9.99.
       01  WS-IMP-PAR        PIC Z(12)9.99.
       01  WS-IMP-CON        PIC Z(12)9.99.

       01  WS-CAB-DET.
           05 FILLER  PIC X(15) VALUE 'TIPO'.
           05 FILLER  PIC X(12) VALUE 'FECHA'.
           05 FILLER  PIC X(10) VALUE 'HORA'.
           05 FILLER  PIC X(09) VALUE 'CLIENTE'.
           05 FILLER  PIC X(07) VALUE 'MOTIVO'.
           05 FILLER  PIC X(08) VALUE 'OFICINA'.
           05 FILLER  PIC X(13) VALUE '        VALOR'.

       01  WS-DET.
           05 WD-TIPO     PIC X(13).
           05 FILLER      PIC X(02) VALUE SPACES.
           05 WD-FECHA    PIC X(10).
           05 FILLER      PIC X(02) VALUE SPACES.
           05 WD-HORA     PIC X(08).
           05 FILLER      PIC X(02) VALUE SPACES.
           05 WD-CLIENTE  PIC X(06).
           05 FILLER      PIC X(03) VALUE SPACES.
           05 WD-MOTIVO   PIC X(02).
           05 FILLER      PIC X(05) VALUE SPACES.
           05 WD-OFICINA  PIC X(03).
           05 FILLER      PIC X(01) VALUE SPACES.
           05 WD-VALOR    PIC Z(12)9.99.

      * RESUMEN CONTABLE POR OFICINA (acotado: 1 fila por cuenta/ofi
      * parametrizada, maximo 100 -> tabla chica, no escala con los
      * movimientos)
       01  WS-RESUMEN.
           05 WR-ITEM OCCURS 100 TIMES INDEXED BY IDX-R.
              10 WR-OFICINA  PIC X(03).
              10 WR-CUENTA   PIC X(12).
              10 WR-NOMBRE   PIC X(30).
              10 WR-PARTIDA  PIC S9(15)V99.
              10 WR-CONTRA   PIC S9(15)V99.
       01  WS-COUNT-RESUMEN  PIC 9(03) VALUE ZEROES.
       01  WS-R-ENCONTRADO   PIC X(01) VALUE 'N'.
       01  WS-I             PIC 9(03) VALUE ZEROES.
       01  WS-J             PIC 9(03) VALUE ZEROES.
       01  WS-J1            PIC 9(03) VALUE ZEROES.
       01  WS-OFI-ANT        PIC X(03) VALUE SPACES.
       01  WS-SUB-OFI-PAR    PIC S9(15)V99 VALUE ZEROES.
       01  WS-SUB-OFI-CON    PIC S9(15)V99 VALUE ZEROES.
       01  WS-TMP-ITEM.
           05 WT-OFICINA  PIC X(03).
           05 WT-CUENTA   PIC X(12).
           05 WT-NOMBRE   PIC X(30).
           05 WT-PARTIDA  PIC S9(15)V99.
           05 WT-CONTRA   PIC S9(15)V99.

       PROCEDURE DIVISION.

      *================================================================*
       100-MAIN SECTION.
       100-MAIN-A.
           MOVE ZEROES TO WS-SQLCODE.
           CALL 'DB-CONECTAR' USING WS-SQLCODE.
           IF WS-SQLCODE NOT = 0
               DISPLAY 'ERROR DE CONEXION. SQLCODE=' WS-SQLCODE
               PERFORM 2000-MOSTRAR-CONTADORES
               PERFORM 100-EXIT
           END-IF.

           MOVE FUNCTION CURRENT-DATE TO WS-CURR.
           STRING WS-CURR(7:2) '/' WS-CURR(5:2) '/' WS-CURR(1:4)
               DELIMITED BY SIZE INTO WS-FECHA-PROC.

           OPEN OUTPUT REPORTE REPERROR.
           MOVE 'REPORTE DE ERRORES / INCONSISTENCIAS'
               TO REG-ERROR.
           WRITE REG-ERROR.
           MOVE WS-SEP TO REG-ERROR.
           WRITE REG-ERROR.
           MOVE WS-CAB-ERR TO REG-ERROR.
           WRITE REG-ERROR.
           MOVE WS-SEP TO REG-ERROR.
           WRITE REG-ERROR.

           PERFORM 500-CARGAR-PARAMETROS.

      *    El clasificador interno de COBOL:
      *      INPUT  PROCEDURE -> lee de la BD (1 x 1) y hace RELEASE
      *      OUTPUT PROCEDURE -> RETURN de los registros ya ordenados
           SORT SORT-FILE
               ON ASCENDING KEY SRT-CUENTA
               ON ASCENDING KEY SRT-OFICINA
               INPUT PROCEDURE IS 1000-PROCESAR-MOVIMIENTOS
               OUTPUT PROCEDURE IS 3000-GENERAR-REPORTE.

           CLOSE REPORTE REPERROR.

           PERFORM 2000-MOSTRAR-CONTADORES.
           PERFORM 100-EXIT.

      *================================================================*
       100-EXIT SECTION.
       100-EXIT-A.
           CALL 'DB-DESCONECTAR'.
           STOP RUN.

      *================================================================*
      * Pide al subprograma que llene la tabla de parametros.          *
      * (Son pocas filas y acotadas -> tabla en memoria, no desborda). *
      *================================================================*
       500-CARGAR-PARAMETROS SECTION.
       500-A.
           INITIALIZE TB-PARAMETROS.
           MOVE ZEROES TO WS-COUNT-PARAM.
           CALL 'DB-CARGAR-PARAMETROS'
               USING TB-PARAMETROS WS-COUNT-PARAM WS-SQLCODE.
           IF WS-SQLCODE < 0
               DISPLAY 'ERROR CARGANDO PARAMETROS. SQLCODE=' WS-SQLCODE
           END-IF.

      *================================================================*
      * INPUT PROCEDURE                                               *
      * Trae UN movimiento por CALL, lo valida en memoria y genera    *
      * SUS DOS afectaciones (PARTIDA + CONTRAPARTIDA) con RELEASE.    *
      * Nunca hay mas de un registro de BD vivo a la vez.             *
      *================================================================*
       1000-PROCESAR-MOVIMIENTOS SECTION.
       1000-LOOP.
           MOVE ZEROES TO WS-SQLCODE.
           PERFORM UNTIL WS-SQLCODE NOT = 0
               CALL 'DB-FETCH-MOV' USING MOV-REG WS-SQLCODE
               IF WS-SQLCODE = 0
                   ADD 1 TO WS-CONT-LEIDOS
                   PERFORM 1200-VALIDAR-Y-ENVIAR
               END-IF
           END-PERFORM.
           GO TO 1000-FIN.

      *----------------------------------------------------------------*
      * Validaciones (punto 5 del enunciado) y generacion del asiento *
      * por DOBLE PARTIDA (punto 4): cada movimiento contabilizado    *
      * produce un registro de PARTIDA y otro de CONTRAPARTIDA por el *
      * mismo valor.  Si falta cualquiera de los dos lados el         *
      * movimiento es INCONSISTENTE y no afecta el cuadre.            *
      *----------------------------------------------------------------*
       1200-VALIDAR-Y-ENVIAR.
      *    5.3 Valor del movimiento
           IF MOV-VALOR <= 0
               ADD 1 TO WS-CONT-RECHAZADOS
               MOVE 'VALOR INVALIDO' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'MOV RECHAZADO: VALOR <= 0'
               EXIT PARAGRAPH
           END-IF.

      *    5.4 Oficina del movimiento
           IF MOV-OFICINA = SPACES OR MOV-OFICINA = ZEROES
               ADD 1 TO WS-CONT-RECHAZADOS
               MOVE 'OFICINA INVALIDA' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'MOV RECHAZADO: OFICINA INVALIDA'
               EXIT PARAGRAPH
           END-IF.

      *    Ubicar la PARTIDA y la CONTRAPARTIDA del motivo
           MOVE ZEROES TO WS-IDX-PARTIDA WS-IDX-CONTRA.
           PERFORM VARYING IDX-PAR FROM 1 BY 1
                   UNTIL IDX-PAR > WS-COUNT-PARAM
               IF PAR-COD-MOTIVO(IDX-PAR) = MOV-COD-MOTIVO
                   IF PAR-TIPO-ENTRADA(IDX-PAR) = 'PARTIDA'
                       SET WS-IDX-PARTIDA TO IDX-PAR
                   ELSE
                       IF PAR-TIPO-ENTRADA(IDX-PAR) = 'CONTRAPARTIDA'
                           SET WS-IDX-CONTRA TO IDX-PAR
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

      *    5.1 Motivo sin ninguna parametrizacion
           IF WS-IDX-PARTIDA = 0 AND WS-IDX-CONTRA = 0
               ADD 1 TO WS-CONT-SIN-PARAM
               MOVE 'MOTIVO NO PARAMETRIZADO' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'MOV SIN PARAMETRIZACION. MOTIVO=' MOV-COD-MOTIVO
               EXIT PARAGRAPH
           END-IF.

      *    5.2 Parametrizacion incompleta -> movimiento inconsistente
           IF WS-IDX-PARTIDA = 0
               ADD 1 TO WS-CONT-RECHAZADOS
               MOVE 'PARTIDA NO ENCONTRADA' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'MOV INCONSISTENTE: FALTA PARTIDA. MOTIVO='
                       MOV-COD-MOTIVO
               EXIT PARAGRAPH
           END-IF.
           IF WS-IDX-CONTRA = 0
               ADD 1 TO WS-CONT-RECHAZADOS
               MOVE 'CONTRAPARTIDA NO ENCONTRADA' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'MOV INCONSISTENTE: FALTA CONTRAPARTIDA. MOTIVO='
                       MOV-COD-MOTIVO
               EXIT PARAGRAPH
           END-IF.

      *    5.4 La oficina de afectacion de ambos lados debe existir
           IF PAR-OFICINA-AFECTACION(WS-IDX-PARTIDA) = SPACES
              OR PAR-OFICINA-AFECTACION(WS-IDX-PARTIDA) = ZEROES
              OR PAR-OFICINA-AFECTACION(WS-IDX-CONTRA)  = SPACES
              OR PAR-OFICINA-AFECTACION(WS-IDX-CONTRA)  = ZEROES
               ADD 1 TO WS-CONT-RECHAZADOS
               MOVE 'OFICINA AFECT. INDEFINIDA' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'OFICINA DE AFECTACION NO DETERMINADA'
               EXIT PARAGRAPH
           END-IF.

      *    Movimiento correcto: se emiten sus DOS afectaciones
           MOVE WS-IDX-PARTIDA TO WS-SUB.
           PERFORM 1250-EMITIR-ASIENTO.
           ADD MOV-VALOR TO WS-TOTAL-PARTIDAS.

           MOVE WS-IDX-CONTRA TO WS-SUB.
           PERFORM 1250-EMITIR-ASIENTO.
           ADD MOV-VALOR TO WS-TOTAL-CONTRAPART.

           ADD 1 TO WS-CONT-VALIDOS.

      *----------------------------------------------------------------*
      * Arma una afectacion contable (fila del SORT) desde la         *
      * parametrizacion WS-SUB y hace RELEASE.                        *
      *----------------------------------------------------------------*
       1250-EMITIR-ASIENTO.
           MOVE PAR-CUENTA-CONTABLE(WS-SUB)    TO SRT-CUENTA.
           MOVE PAR-OFICINA-AFECTACION(WS-SUB) TO SRT-OFICINA.
           MOVE PAR-TIPO-ENTRADA(WS-SUB)       TO SRT-TIPO-ENTRADA.
           MOVE MOV-FECHA-MOV                  TO SRT-FECHA.
           MOVE MOV-HORA-MOV                   TO SRT-HORA.
           MOVE MOV-COD-CLIENTE                TO SRT-CLIENTE.
           MOVE MOV-COD-MOTIVO                 TO SRT-MOTIVO.
           MOVE MOV-VALOR                      TO SRT-VALOR.
           MOVE PAR-NOMBRE-CUENTA(WS-SUB)      TO SRT-NOMBRE-CTA.
           RELEASE REG-SORT.
           ADD 1 TO WS-CONT-ASIENTOS.

      *----------------------------------------------------------------*
      * Punto 13: registra el movimiento no contabilizado en REPERROR *
      * con su tipo de error.  No afecta los totales del cuadre.      *
      *----------------------------------------------------------------*
       220-GRABAR-ERROR.
      *    MOV-FECHA-MOV viene como AAAA-MM-DD -> se muestra DD/MM/AAAA
           STRING MOV-FECHA-MOV(9:2) '/' MOV-FECHA-MOV(6:2) '/'
                  MOV-FECHA-MOV(1:4)
               DELIMITED BY SIZE INTO WE-FECHA.
           MOVE MOV-HORA-MOV    TO WE-HORA.
           MOVE MOV-COD-CLIENTE TO WE-CLIENTE.
           MOVE MOV-COD-MOTIVO  TO WE-MOTIVO.
           MOVE MOV-OFICINA     TO WE-OFICINA.
           MOVE MOV-VALOR       TO WE-VALOR.
           MOVE WS-TIPO-ERROR   TO WE-TIPO-ERROR.
           WRITE REG-ERROR FROM WS-LINEA-ERROR.

       1000-FIN.
           EXIT.

      *================================================================*
      * OUTPUT PROCEDURE  ---  REPORTE CONTABLE PAGINADO (puntos 7-12) *
      *                                                              *
      * Toma los asientos ya clasificados con RETURN, UNO A UNO, y    *
      * los escribe directo en REPORTE (nunca los junta en memoria).  *
      * En paralelo acumula el RESUMEN POR OFICINA en una tabla       *
      * acotada (una fila por cuenta/oficina parametrizada, max 100). *
      *================================================================*
       3000-GENERAR-REPORTE SECTION.
       3000-A.
           MOVE ZEROES TO WS-PAGINA.
           MOVE ZEROES TO WS-LINEAS.
           MOVE ZEROES TO WS-COUNT-RESUMEN.
           MOVE 'N' TO WS-FIN-MOVIMIENTOS.

           RETURN SORT-FILE AT END
               MOVE 'S' TO WS-FIN-MOVIMIENTOS
           END-RETURN.

           IF WS-FIN-MOVIMIENTOS = 'S'
               MOVE 'SIN MOVIMIENTOS' TO WS-FECHA-MOV-REP
               PERFORM 3100-ENCABEZADO
               PERFORM 3600-RESUMEN-OFICINA
               PERFORM 3700-TOTAL-GENERAL
               GO TO 3000-FIN
           END-IF.

           PERFORM 3950-FMT-FECHA-MOV.
           PERFORM 3100-ENCABEZADO.
           MOVE SRT-CUENTA TO WS-CUENTA-ANTERIOR.
           PERFORM 3200-ABRIR-CUENTA.

           PERFORM UNTIL WS-FIN-MOVIMIENTOS = 'S'
               IF SRT-CUENTA NOT = WS-CUENTA-ANTERIOR
                   PERFORM 3400-CERRAR-CUENTA
                   MOVE SRT-CUENTA TO WS-CUENTA-ANTERIOR
                   PERFORM 3200-ABRIR-CUENTA
               END-IF

               PERFORM 3300-ESCRIBIR-DETALLE
               PERFORM 3500-ACUMULAR-RESUMEN

               RETURN SORT-FILE AT END
                   MOVE 'S' TO WS-FIN-MOVIMIENTOS
               END-RETURN
           END-PERFORM.

           PERFORM 3400-CERRAR-CUENTA.
           PERFORM 3600-RESUMEN-OFICINA.
           PERFORM 3700-TOTAL-GENERAL.
           GO TO 3000-FIN.

      *---- encabezado compacto, se reimprime cada pagina (7.1 / 8) --*
       3100-ENCABEZADO.
           ADD 1 TO WS-PAGINA.
           MOVE WS-PAGINA TO WS-PAGINA-ED.
           MOVE 'CUADRE CONTABLE - MOVIMIENTOS' TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE SPACES TO REG-REPORTE.
           STRING 'PROCESO: TSQL001A   FECHA PROC: ' WS-FECHA-PROC
                  '   FECHA MOV: ' WS-FECHA-MOV-REP
                  '   PAG: ' WS-PAGINA-ED
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SEP TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE 3 TO WS-LINEAS.

       3150-VERIF-PAGINA.
           IF WS-LINEAS >= WS-MAX-LINEAS
               PERFORM 3100-ENCABEZADO
           END-IF.

      *---- cabecera de una cuenta contable (punto 9) ----------------*
       3200-ABRIR-CUENTA.
           PERFORM 3150-VERIF-PAGINA.
           MOVE ZEROES TO WS-SUBTOT-CUENTA.
           MOVE SPACES TO REG-REPORTE.
           STRING 'CUENTA: ' SRT-CUENTA '  ' SRT-NOMBRE-CTA
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-CAB-DET TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SEP TO REG-REPORTE.
           WRITE REG-REPORTE.
           ADD 3 TO WS-LINEAS.

      *---- una linea de detalle (punto 9) --------------------------*
       3300-ESCRIBIR-DETALLE.
           PERFORM 3150-VERIF-PAGINA.
           MOVE SPACES TO WS-DET.
           MOVE SRT-TIPO-ENTRADA TO WD-TIPO.
           STRING SRT-FECHA(9:2) '/' SRT-FECHA(6:2) '/' SRT-FECHA(1:4)
               DELIMITED BY SIZE INTO WD-FECHA.
           MOVE SRT-HORA    TO WD-HORA.
           MOVE SRT-CLIENTE TO WD-CLIENTE.
           MOVE SRT-MOTIVO  TO WD-MOTIVO.
           MOVE SRT-OFICINA TO WD-OFICINA.
           MOVE SRT-VALOR   TO WD-VALOR.
           MOVE WS-DET TO REG-REPORTE.
           WRITE REG-REPORTE.
           ADD 1 TO WS-LINEAS.
           ADD SRT-VALOR TO WS-SUBTOT-CUENTA.

      *---- subtotal de la cuenta al quiebre de control (punto 9) ---*
       3400-CERRAR-CUENTA.
           PERFORM 3150-VERIF-PAGINA.
           MOVE WS-SEP TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SUBTOT-CUENTA TO WS-IMP-CTA.
           MOVE SPACES TO REG-REPORTE.
           STRING 'TOTAL CUENTA: ' WS-IMP-CTA
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE SPACES TO REG-REPORTE.
           WRITE REG-REPORTE.
           ADD 3 TO WS-LINEAS.

      *---- acumula el resumen por (OFICINA, CUENTA) ----------------*
       3500-ACUMULAR-RESUMEN.
           MOVE 'N' TO WS-R-ENCONTRADO.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-COUNT-RESUMEN
               IF WR-OFICINA(WS-I) = SRT-OFICINA
                  AND WR-CUENTA(WS-I) = SRT-CUENTA
                   MOVE 'S' TO WS-R-ENCONTRADO
                   PERFORM 3550-SUMAR-RESUMEN
               END-IF
           END-PERFORM.
           IF WS-R-ENCONTRADO = 'N' AND WS-COUNT-RESUMEN < 100
               ADD 1 TO WS-COUNT-RESUMEN
               MOVE WS-COUNT-RESUMEN TO WS-I
               MOVE SRT-OFICINA    TO WR-OFICINA(WS-I)
               MOVE SRT-CUENTA     TO WR-CUENTA(WS-I)
               MOVE SRT-NOMBRE-CTA TO WR-NOMBRE(WS-I)
               MOVE ZEROES TO WR-PARTIDA(WS-I)
               MOVE ZEROES TO WR-CONTRA(WS-I)
               PERFORM 3550-SUMAR-RESUMEN
           END-IF.

       3550-SUMAR-RESUMEN.
           IF SRT-TIPO-ENTRADA = 'PARTIDA'
               ADD SRT-VALOR TO WR-PARTIDA(WS-I)
           ELSE
               ADD SRT-VALOR TO WR-CONTRA(WS-I)
           END-IF.

      *---- RESUMEN CONTABLE POR OFICINA (punto 11) -----------------*
       3600-RESUMEN-OFICINA.
      *    Ordenamiento simple (burbuja) por OFICINA y luego CUENTA
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I >= WS-COUNT-RESUMEN
               PERFORM VARYING WS-J FROM 1 BY 1
                       UNTIL WS-J >= WS-COUNT-RESUMEN
                   COMPUTE WS-J1 = WS-J + 1
                   IF WR-OFICINA(WS-J) > WR-OFICINA(WS-J1)
                     OR (WR-OFICINA(WS-J) = WR-OFICINA(WS-J1)
                         AND WR-CUENTA(WS-J) > WR-CUENTA(WS-J1))
                       MOVE WR-ITEM(WS-J)  TO WS-TMP-ITEM
                       MOVE WR-ITEM(WS-J1) TO WR-ITEM(WS-J)
                       MOVE WS-TMP-ITEM    TO WR-ITEM(WS-J1)
                   END-IF
               END-PERFORM
           END-PERFORM.

           PERFORM 3150-VERIF-PAGINA.
           MOVE SPACES TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE 'RESUMEN CONTABLE POR OFICINA' TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SEP TO REG-REPORTE.
           WRITE REG-REPORTE.
           ADD 3 TO WS-LINEAS.

           MOVE SPACES TO WS-OFI-ANT.
           MOVE ZEROES TO WS-SUB-OFI-PAR.
           MOVE ZEROES TO WS-SUB-OFI-CON.

           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-COUNT-RESUMEN
               IF WR-OFICINA(WS-I) NOT = WS-OFI-ANT
                   IF WS-OFI-ANT NOT = SPACES
                       PERFORM 3650-TOTAL-OFICINA
                   END-IF
                   MOVE WR-OFICINA(WS-I) TO WS-OFI-ANT
                   MOVE ZEROES TO WS-SUB-OFI-PAR
                   MOVE ZEROES TO WS-SUB-OFI-CON
                   PERFORM 3150-VERIF-PAGINA
                   MOVE SPACES TO REG-REPORTE
                   STRING 'OFICINA: ' WR-OFICINA(WS-I)
                       DELIMITED BY SIZE INTO REG-REPORTE
                   WRITE REG-REPORTE
                   ADD 1 TO WS-LINEAS
               END-IF
               PERFORM 3150-VERIF-PAGINA
               MOVE WR-PARTIDA(WS-I) TO WS-IMP-PAR
               MOVE WR-CONTRA(WS-I)  TO WS-IMP-CON
               COMPUTE WS-DIFERENCIA =
                   WR-PARTIDA(WS-I) - WR-CONTRA(WS-I)
               MOVE WS-DIFERENCIA TO WS-EDT-DIFEREN
               MOVE SPACES TO REG-REPORTE
               STRING '  ' WR-CUENTA(WS-I) ' ' WR-NOMBRE(WS-I)
                      ' P:' WS-IMP-PAR ' C:' WS-IMP-CON
                      ' D:' WS-EDT-DIFEREN
                   DELIMITED BY SIZE INTO REG-REPORTE
               WRITE REG-REPORTE
               ADD 1 TO WS-LINEAS
               ADD WR-PARTIDA(WS-I) TO WS-SUB-OFI-PAR
               ADD WR-CONTRA(WS-I)  TO WS-SUB-OFI-CON
           END-PERFORM.
           IF WS-OFI-ANT NOT = SPACES
               PERFORM 3650-TOTAL-OFICINA
           END-IF.

       3650-TOTAL-OFICINA.
           PERFORM 3150-VERIF-PAGINA.
           MOVE WS-SUB-OFI-PAR TO WS-IMP-PAR.
           MOVE WS-SUB-OFI-CON TO WS-IMP-CON.
           MOVE SPACES TO REG-REPORTE.
           STRING 'TOTAL OFICINA ' WS-OFI-ANT
                  '   PARTIDAS:' WS-IMP-PAR
                  '   CONTRAPARTIDAS:' WS-IMP-CON
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE SPACES TO REG-REPORTE.
           WRITE REG-REPORTE.
           ADD 2 TO WS-LINEAS.

      *---- TOTAL GENERAL (punto 12) -------------------------------*
       3700-TOTAL-GENERAL.
           PERFORM 3150-VERIF-PAGINA.
           COMPUTE WS-DIFERENCIA =
               WS-TOTAL-PARTIDAS - WS-TOTAL-CONTRAPART.
           IF WS-DIFERENCIA = 0
               MOVE 'CUADRADO'    TO WS-ESTADO-CUADRE
           ELSE
               MOVE 'DESCUADRADO' TO WS-ESTADO-CUADRE
           END-IF.
           MOVE WS-SEP TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE '                 TOTAL GENERAL' TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SEP TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-TOTAL-PARTIDAS TO WS-IMP-PAR.
           MOVE SPACES TO REG-REPORTE.
           STRING 'TOTAL PARTIDAS       : ' WS-IMP-PAR
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-TOTAL-CONTRAPART TO WS-IMP-CON.
           MOVE SPACES TO REG-REPORTE.
           STRING 'TOTAL CONTRAPARTIDAS : ' WS-IMP-CON
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-DIFERENCIA TO WS-EDT-DIFEREN.
           MOVE SPACES TO REG-REPORTE.
           STRING 'DIFERENCIA           : ' WS-EDT-DIFEREN
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE SPACES TO REG-REPORTE.
           STRING 'ESTADO DEL CUADRE    : ' WS-ESTADO-CUADRE
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SEP TO REG-REPORTE.
           WRITE REG-REPORTE.

       3950-FMT-FECHA-MOV.
           STRING SRT-FECHA(9:2) '/' SRT-FECHA(6:2) '/' SRT-FECHA(1:4)
               DELIMITED BY SIZE INTO WS-FECHA-MOV-REP.

       3000-FIN.
           EXIT.

      *================================================================*
      * 2000-MOSTRAR-CONTADORES  ---  cuadre contable (puntos 6 y 12)  *
      *================================================================*
       2000-MOSTRAR-CONTADORES SECTION.
       2000-A.
           COMPUTE WS-DIFERENCIA =
               WS-TOTAL-PARTIDAS - WS-TOTAL-CONTRAPART.
           IF WS-DIFERENCIA = 0
               MOVE 'CUADRADO'    TO WS-ESTADO-CUADRE
           ELSE
               MOVE 'DESCUADRADO' TO WS-ESTADO-CUADRE
           END-IF.

           DISPLAY '========================================'.
           DISPLAY '            CUADRE CONTABLE'.
           DISPLAY '========================================'.
           DISPLAY 'PARAMETROS CARGADOS    : ' WS-COUNT-PARAM.
           DISPLAY 'MOVIMIENTOS LEIDOS     : ' WS-CONT-LEIDOS.
           DISPLAY 'MOVIMIENTOS CONTABILIZ.: ' WS-CONT-VALIDOS.
           DISPLAY 'MOVIMIENTOS RECHAZADOS : ' WS-CONT-RECHAZADOS.
           DISPLAY 'MOVIMIENTOS SIN PARAM  : ' WS-CONT-SIN-PARAM.
           DISPLAY 'AFECTACIONES GENERADAS : ' WS-CONT-ASIENTOS.
           MOVE WS-TOTAL-PARTIDAS   TO WS-EDT-IMPORTE.
           DISPLAY 'TOTAL PARTIDAS         : ' WS-EDT-IMPORTE.
           MOVE WS-TOTAL-CONTRAPART TO WS-EDT-IMPORTE.
           DISPLAY 'TOTAL CONTRAPARTIDAS   : ' WS-EDT-IMPORTE.
           MOVE WS-DIFERENCIA       TO WS-EDT-DIFEREN.
           DISPLAY 'DIFERENCIA             : ' WS-EDT-DIFEREN.
           DISPLAY 'ESTADO DEL CUADRE      : ' WS-ESTADO-CUADRE.
           DISPLAY '========================================'.
