       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSQL001A.

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

       01  FS-REPORTE          PIC X(02) VALUE '00'.
       01  FS-ERROR            PIC X(02) VALUE '00'.

      * mov que llena el subprograma 1 x 1
           COPY CPY-MOV.

      * tabla parametros carga
           COPY CPY-PARAM.

       01  WS-SQLCODE          PIC S9(09) COMP-5 VALUE ZEROES.

      * Contadores y acumuladores
       01  WS-TOTAL-PARTIDAS   PIC S9(15)V99 VALUE ZEROES.
       01  WS-TOTAL-CONTRAPART PIC S9(15)V99 VALUE ZEROES.
       01  WS-DIFERENCIA       PIC S9(15)V99 VALUE ZEROES.
       01  WS-SUBTOT-CUENTA    PIC S9(15)V99 VALUE ZEROES.
       01  WS-ESTADO-CUADRE    PIC X(11) VALUE SPACES.
       01  WS-CUENTA-ANTERIOR  PIC X(12) VALUE SPACES.
       01  WS-FIN-MOVIMIENTOS  PIC X(01) VALUE 'N'.

      * Indice de la PARTIDA y de la CONTRAPARTIDA del motivo en curso
       01  WS-IDX-PARTIDA      PIC 9(03) VALUE ZEROES.
       01  WS-IDX-CONTRA       PIC 9(03) VALUE ZEROES.
      * Indice del parametro que se esta contabilizando (par / contra)
       01  IDX-PARAM          PIC 9(03) VALUE ZEROES.

       01  WS-DIFEREN-EDIT    PIC -(14)9.99.

      * REPORTE2
       01  WS-TIPO-ERROR      PIC X(28) VALUE SPACES.
       01  WS-CAB-ERROR.
           05 FILLER  PIC X(01) VALUE SPACES.
           05 FILLER  PIC X(12) VALUE 'FECHA'.
           05 FILLER  PIC X(10) VALUE 'HORA'.
           05 FILLER  PIC X(08) VALUE 'CLIENTE'.
           05 FILLER  PIC X(04) VALUE 'MOT'.
           05 FILLER  PIC X(05) VALUE 'OFI'.
           05 FILLER  PIC X(14) VALUE '         VALOR'.
           05 FILLER  PIC X(02) VALUE SPACES.
           05 FILLER  PIC X(13) VALUE 'TIPO DE ERROR'.

      * WORKING ERROR
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

      *--------------------------------------------------------------
      * VARIABLES PARA ARMAR REPORTE1
      * GUIA DE PREFIJOS / NOMBRES:
      *   DET-      campos de la linea de DETALLE por movimiento
      *   RES-      campos de un item del RESUMEN por oficina/cuenta
      *   IDX-      indice para recorrer una tabla
      *   WS-IMP-.. importe con formato de edicion para imprimir
      *--------------------------------------------------------------
       01  WS-SEPARADOR       PIC X(70) VALUE ALL '-'.
       01  WS-PAGINA         PIC 9(03) VALUE ZEROES.
       01  WS-PAGINA-ED      PIC ZZ9.
       01  WS-LINEAS         PIC 9(03) VALUE ZEROES.
       01  WS-MAX-LINEAS     PIC 9(03) VALUE 60.
       01  WS-FECHA-PROC     PIC X(10) VALUE SPACES.
       01  WS-FECHA-MOV-REP  PIC X(15) VALUE SPACES.
       01  WS-FECHA-SISTEMA  PIC X(21).
       01  WS-IMP-CTA        PIC Z(12)9.99.
       01  WS-IMP-PAR        PIC Z(12)9.99.
       01  WS-IMP-CON        PIC Z(12)9.99.

      * Cabecera de columnas del detalle
       01  WS-CAB-DETALLE.
           05 FILLER  PIC X(15) VALUE 'TIPO'.
           05 FILLER  PIC X(12) VALUE 'FECHA'.
           05 FILLER  PIC X(10) VALUE 'HORA'.
           05 FILLER  PIC X(09) VALUE 'CLIENTE'.
           05 FILLER  PIC X(07) VALUE 'MOTIVO'.
           05 FILLER  PIC X(08) VALUE 'OFICINA'.
           05 FILLER  PIC X(13) VALUE '        VALOR'.

      * Linea de detalle (un movimiento del reporte)
       01  WS-LIN-DETALLE.
           05 DET-TIPO     PIC X(13).
           05 FILLER      PIC X(02) VALUE SPACES.
           05 DET-FECHA    PIC X(10).
           05 FILLER      PIC X(02) VALUE SPACES.
           05 DET-HORA     PIC X(08).
           05 FILLER      PIC X(02) VALUE SPACES.
           05 DET-CLIENTE  PIC X(06).
           05 FILLER      PIC X(03) VALUE SPACES.
           05 DET-MOTIVO   PIC X(02).
           05 FILLER      PIC X(05) VALUE SPACES.
           05 DET-OFICINA  PIC X(03).
           05 FILLER      PIC X(01) VALUE SPACES.
           05 DET-VALOR    PIC Z(12)9.99.

      * Nro de items usados del resumen (tambien acota el SORT de tabla)
       01  WS-COUNT-RESUMEN     PIC 9(03) VALUE ZEROES.

      * Tabla de resumen: un item por cada oficina + cuenta
       01  WS-RESUMEN.
           05 RES-ITEM OCCURS 1 TO 100 TIMES
                       DEPENDING ON WS-COUNT-RESUMEN.
              10 RES-OFICINA  PIC X(03).
              10 RES-CUENTA   PIC X(12).
              10 RES-NOMBRE   PIC X(30).
              10 RES-PARTIDA  PIC S9(15)V99.
              10 RES-CONTRA   PIC S9(15)V99.

       01  WS-RES-ENCONTRADO    PIC X(01) VALUE 'N'.
      * Indice para recorrer la tabla de resumen
       01  IDX-RES             PIC 9(03) VALUE ZEROES.
       01  WS-OFICINA-ANTERIOR  PIC X(03) VALUE SPACES.
       01  WS-SUBTOT-OFI-PAR    PIC S9(15)V99 VALUE ZEROES.
       01  WS-SUBTOT-OFI-CON    PIC S9(15)V99 VALUE ZEROES.

       PROCEDURE DIVISION.

       100-MAIN SECTION.

       100-MAIN-A.
           MOVE ZEROES TO WS-SQLCODE.
           CALL 'DB-CONECTAR' USING WS-SQLCODE.
           IF WS-SQLCODE NOT = 0
               DISPLAY 'ERROR DE CONEXION. SQLCODE=' WS-SQLCODE
               PERFORM 100-EXIT
           END-IF.

           MOVE FUNCTION CURRENT-DATE TO WS-FECHA-SISTEMA.
           STRING WS-FECHA-SISTEMA(7:2) '/'
                  WS-FECHA-SISTEMA(5:2) '/'
                  WS-FECHA-SISTEMA(1:4)
               DELIMITED BY SIZE INTO WS-FECHA-PROC.

      *    REPORTE2 cabecera de lista errores
           OPEN OUTPUT REPORTE REPERROR.
           MOVE 'REPORTE DE ERRORES / INCONSISTENCIAS'
               TO REG-ERROR.
           WRITE REG-ERROR.
           MOVE WS-SEPARADOR TO REG-ERROR.
           WRITE REG-ERROR.
           MOVE WS-CAB-ERROR TO REG-ERROR.
           WRITE REG-ERROR.
           MOVE WS-SEPARADOR TO REG-ERROR.
           WRITE REG-ERROR.

           PERFORM 500-CARGAR-PARAMETROS.

           SORT SORT-FILE
               ON ASCENDING KEY SRT-CUENTA
               ON ASCENDING KEY SRT-OFICINA
               INPUT PROCEDURE IS 1000-PROCESAR-MOVIMIENTOS
               OUTPUT PROCEDURE IS 3000-GENERAR-REPORTE.

           CLOSE REPORTE REPERROR.

           PERFORM 100-EXIT.

       100-EXIT SECTION.
       100-EXIT-A.
           CALL 'DB-DESCONECTAR'.
           STOP RUN.

       500-CARGAR-PARAMETROS SECTION.
       500-A.
           INITIALIZE TB-PARAMETROS.
           MOVE ZEROES TO WS-COUNT-PARAM.
           CALL 'DB-CARGAR-PARAMETROS'
               USING TB-PARAMETROS WS-COUNT-PARAM WS-SQLCODE.
           IF WS-SQLCODE < 0
               DISPLAY 'ERROR CARGANDO PARAMETROS. SQLCODE=' WS-SQLCODE
           END-IF.

       1000-PROCESAR-MOVIMIENTOS SECTION.
       1000-LOOP.
           MOVE ZEROES TO WS-SQLCODE.
           PERFORM UNTIL WS-SQLCODE NOT = 0
               CALL 'DB-FETCH-MOV' USING MOV-REG WS-SQLCODE
               IF WS-SQLCODE = 0
                   PERFORM 1200-VALIDAR-Y-ENVIAR
               END-IF
           END-PERFORM.
           GO TO 1000-FIN.

      * Validaciones del mov
       1200-VALIDAR-Y-ENVIAR.
      *    Valor
           IF MOV-VALOR <= 0
               MOVE 'VALOR INVALIDO' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'MOV RECHAZADO: VALOR <= 0'
               EXIT PARAGRAPH
           END-IF.

      *    oficina
           IF MOV-OFICINA = SPACES OR MOV-OFICINA = ZEROES
               MOVE 'OFICINA INVALIDA' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'MOV RECHAZADO: OFICINA INVALIDA'
               EXIT PARAGRAPH
           END-IF.

      *    Par y Contra
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

      *    motivo si parametrizacion
           IF WS-IDX-PARTIDA = 0 AND WS-IDX-CONTRA = 0
               MOVE 'MOTIVO NO PARAMETRIZADO' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'MOV SIN PARAMETRIZACION. MOTIVO=' MOV-COD-MOTIVO
               EXIT PARAGRAPH
           END-IF.

      *    parametrizacion incompleta, par y contra par del mov
           IF WS-IDX-PARTIDA = 0
               MOVE 'PARTIDA NO ENCONTRADA' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'MOV INCONSISTENTE: FALTA PARTIDA. MOTIVO='
                       MOV-COD-MOTIVO
               EXIT PARAGRAPH
           END-IF.
           IF WS-IDX-CONTRA = 0
               MOVE 'CONTRAPARTIDA NO ENCONTRADA' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'MOV INCONSISTENTE: FALTA CONTRAPARTIDA. MOTIVO='
                       MOV-COD-MOTIVO
               EXIT PARAGRAPH
           END-IF.

      *    oficina para par y contra debe existir
           IF PAR-OFICINA-AFECTACION(WS-IDX-PARTIDA) = SPACES
              OR PAR-OFICINA-AFECTACION(WS-IDX-PARTIDA) = ZEROES
              OR PAR-OFICINA-AFECTACION(WS-IDX-CONTRA)  = SPACES
              OR PAR-OFICINA-AFECTACION(WS-IDX-CONTRA)  = ZEROES
               MOVE 'OFICINA AFECT. INDEFINIDA' TO WS-TIPO-ERROR
               PERFORM 220-GRABAR-ERROR
               DISPLAY 'OFICINA DE AFECTACION NO DETERMINADA'
               EXIT PARAGRAPH
           END-IF.

      *    movimiento correcto, se genera 2 lineas contables
           MOVE WS-IDX-PARTIDA TO IDX-PARAM.
           PERFORM 1250-EMITIR-ASIENTO.
           ADD MOV-VALOR TO WS-TOTAL-PARTIDAS.

           MOVE WS-IDX-CONTRA TO IDX-PARAM.
           PERFORM 1250-EMITIR-ASIENTO.
           ADD MOV-VALOR TO WS-TOTAL-CONTRAPART.

      *    arma la fila de SORT con IDX-PARAM y la libera (RELEASE)
       1250-EMITIR-ASIENTO.
           MOVE PAR-CUENTA-CONTABLE(IDX-PARAM)    TO SRT-CUENTA.
           MOVE PAR-OFICINA-AFECTACION(IDX-PARAM) TO SRT-OFICINA.
           MOVE PAR-TIPO-ENTRADA(IDX-PARAM)       TO SRT-TIPO-ENTRADA.
           MOVE MOV-FECHA-MOV                  TO SRT-FECHA.
           MOVE MOV-HORA-MOV                   TO SRT-HORA.
           MOVE MOV-COD-CLIENTE                TO SRT-CLIENTE.
           MOVE MOV-COD-MOTIVO                 TO SRT-MOTIVO.
           MOVE MOV-VALOR                      TO SRT-VALOR.
           MOVE PAR-NOMBRE-CUENTA(IDX-PARAM)      TO SRT-NOMBRE-CTA.
           RELEASE REG-SORT.

       220-GRABAR-ERROR.
      *    fecha mov
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

      * REPORTE1, encabezado*
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
           MOVE WS-SEPARADOR TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE 3 TO WS-LINEAS.

       3150-VERIF-PAGINA.
           IF WS-LINEAS >= WS-MAX-LINEAS
               PERFORM 3100-ENCABEZADO
           END-IF.

      * ARMAR REPORTE POR CUENTA CONTABLE
       3200-ABRIR-CUENTA.
           PERFORM 3150-VERIF-PAGINA.
           MOVE ZEROES TO WS-SUBTOT-CUENTA.
           MOVE SPACES TO REG-REPORTE.
           STRING 'CUENTA: ' SRT-CUENTA '  ' SRT-NOMBRE-CTA
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-CAB-DETALLE TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SEPARADOR TO REG-REPORTE.
           WRITE REG-REPORTE.
           ADD 3 TO WS-LINEAS.

      * DETALLE DE CADA MOV ASOCIADO A LA CUENTA
       3300-ESCRIBIR-DETALLE.
           PERFORM 3150-VERIF-PAGINA.
           MOVE SPACES TO WS-LIN-DETALLE.
           MOVE SRT-TIPO-ENTRADA TO DET-TIPO.
           STRING SRT-FECHA(9:2) '/' SRT-FECHA(6:2) '/' SRT-FECHA(1:4)
               DELIMITED BY SIZE INTO DET-FECHA.
           MOVE SRT-HORA    TO DET-HORA.
           MOVE SRT-CLIENTE TO DET-CLIENTE.
           MOVE SRT-MOTIVO  TO DET-MOTIVO.
           MOVE SRT-OFICINA TO DET-OFICINA.
           MOVE SRT-VALOR   TO DET-VALOR.
           MOVE WS-LIN-DETALLE TO REG-REPORTE.
           WRITE REG-REPORTE.
           ADD 1 TO WS-LINEAS.
           ADD SRT-VALOR TO WS-SUBTOT-CUENTA.

      * QUIEBRE DE CUENTA Y TOTAL DE CUENTA
       3400-CERRAR-CUENTA.
           PERFORM 3150-VERIF-PAGINA.
           MOVE WS-SEPARADOR TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SUBTOT-CUENTA TO WS-IMP-CTA.
           MOVE SPACES TO REG-REPORTE.
           STRING 'TOTAL CUENTA: ' WS-IMP-CTA
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE SPACES TO REG-REPORTE.
           WRITE REG-REPORTE.
           ADD 3 TO WS-LINEAS.
      
      * ACUMULA PARTIDA Y CONTRAPARTIDA EN EL RESUMEN POR OFICINA/CUENTA
       3500-ACUMULAR-RESUMEN.
           MOVE 'N' TO WS-RES-ENCONTRADO.

           *> ACUMULA EXISTENTE
           PERFORM VARYING IDX-RES FROM 1 BY 1
                   UNTIL IDX-RES > WS-COUNT-RESUMEN
               IF RES-OFICINA(IDX-RES) = SRT-OFICINA
                  AND RES-CUENTA(IDX-RES) = SRT-CUENTA
                   MOVE 'S' TO WS-RES-ENCONTRADO
                   PERFORM 3550-SUMAR-RESUMEN
               END-IF
           END-PERFORM.

           *> REGISTRA Y ACUMULA
           IF WS-RES-ENCONTRADO = 'N' AND WS-COUNT-RESUMEN < 100 
               ADD 1 TO WS-COUNT-RESUMEN
               MOVE WS-COUNT-RESUMEN TO IDX-RES
               MOVE SRT-OFICINA    TO RES-OFICINA(IDX-RES)
               MOVE SRT-CUENTA     TO RES-CUENTA(IDX-RES)
               MOVE SRT-NOMBRE-CTA TO RES-NOMBRE(IDX-RES)
               MOVE ZEROES TO RES-PARTIDA(IDX-RES)
               MOVE ZEROES TO RES-CONTRA(IDX-RES)
               PERFORM 3550-SUMAR-RESUMEN
           END-IF.

      * SUMA EL VALOR EN PARTIDA O CONTRAPARTIDA POR OFICINA Y CUENTA
       3550-SUMAR-RESUMEN.
           IF SRT-TIPO-ENTRADA = 'PARTIDA'
               ADD SRT-VALOR TO RES-PARTIDA(IDX-RES)
           ELSE
               ADD SRT-VALOR TO RES-CONTRA(IDX-RES)
           END-IF.

      * Ordena el resumen por OFICINA y luego por CUENTA
       3600-RESUMEN-OFICINA.
           IF WS-COUNT-RESUMEN > 1
               SORT RES-ITEM ASCENDING KEY RES-OFICINA RES-CUENTA
           END-IF.

           PERFORM 3150-VERIF-PAGINA.
           MOVE SPACES TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE 'RESUMEN CONTABLE POR OFICINA' TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SEPARADOR TO REG-REPORTE.
           WRITE REG-REPORTE.
           ADD 3 TO WS-LINEAS.

           MOVE SPACES TO WS-OFICINA-ANTERIOR.
           MOVE ZEROES TO WS-SUBTOT-OFI-PAR.
           MOVE ZEROES TO WS-SUBTOT-OFI-CON.

           PERFORM VARYING IDX-RES FROM 1 BY 1
                   UNTIL IDX-RES > WS-COUNT-RESUMEN
               IF RES-OFICINA(IDX-RES) NOT = WS-OFICINA-ANTERIOR
                   IF WS-OFICINA-ANTERIOR NOT = SPACES
                       PERFORM 3650-TOTAL-OFICINA
                   END-IF
                   MOVE RES-OFICINA(IDX-RES) TO WS-OFICINA-ANTERIOR
                   MOVE ZEROES TO WS-SUBTOT-OFI-PAR
                   MOVE ZEROES TO WS-SUBTOT-OFI-CON
                   PERFORM 3150-VERIF-PAGINA
                   MOVE SPACES TO REG-REPORTE
                   STRING 'OFICINA: ' RES-OFICINA(IDX-RES)
                       DELIMITED BY SIZE INTO REG-REPORTE
                   WRITE REG-REPORTE
                   ADD 1 TO WS-LINEAS
               END-IF
               PERFORM 3150-VERIF-PAGINA
               MOVE RES-PARTIDA(IDX-RES) TO WS-IMP-PAR
               MOVE RES-CONTRA(IDX-RES)  TO WS-IMP-CON
               COMPUTE WS-DIFERENCIA =
                   RES-PARTIDA(IDX-RES) - RES-CONTRA(IDX-RES)
               MOVE WS-DIFERENCIA TO WS-DIFEREN-EDIT
               MOVE SPACES TO REG-REPORTE
               STRING '  ' RES-CUENTA(IDX-RES) ' ' RES-NOMBRE(IDX-RES)
                      ' P:' WS-IMP-PAR ' C:' WS-IMP-CON
                      ' D:' WS-DIFEREN-EDIT
                   DELIMITED BY SIZE INTO REG-REPORTE
               WRITE REG-REPORTE
               ADD 1 TO WS-LINEAS
               ADD RES-PARTIDA(IDX-RES) TO WS-SUBTOT-OFI-PAR
               ADD RES-CONTRA(IDX-RES)  TO WS-SUBTOT-OFI-CON
           END-PERFORM.
           IF WS-OFICINA-ANTERIOR NOT = SPACES
               PERFORM 3650-TOTAL-OFICINA
           END-IF.

       3650-TOTAL-OFICINA.
           PERFORM 3150-VERIF-PAGINA.
           MOVE WS-SUBTOT-OFI-PAR TO WS-IMP-PAR.
           MOVE WS-SUBTOT-OFI-CON TO WS-IMP-CON.
           MOVE SPACES TO REG-REPORTE.
           STRING 'TOTAL OFICINA ' WS-OFICINA-ANTERIOR
                  '   PARTIDAS:' WS-IMP-PAR
                  '   CONTRAPARTIDAS:' WS-IMP-CON
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE SPACES TO REG-REPORTE.
           WRITE REG-REPORTE.
           ADD 2 TO WS-LINEAS.

      * TOTAL GENERAL Y ESTADO DE CUADRE
       3700-TOTAL-GENERAL.
           PERFORM 3150-VERIF-PAGINA.
           COMPUTE WS-DIFERENCIA =
               WS-TOTAL-PARTIDAS - WS-TOTAL-CONTRAPART.
           IF WS-DIFERENCIA = 0
               MOVE 'CUADRADO'    TO WS-ESTADO-CUADRE
           ELSE
               MOVE 'DESCUADRADO' TO WS-ESTADO-CUADRE
           END-IF.
           MOVE WS-SEPARADOR TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE '                 TOTAL GENERAL' TO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SEPARADOR TO REG-REPORTE.
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
           MOVE WS-DIFERENCIA TO WS-DIFEREN-EDIT.
           MOVE SPACES TO REG-REPORTE.
           STRING 'DIFERENCIA           : ' WS-DIFEREN-EDIT
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE SPACES TO REG-REPORTE.
           STRING 'ESTADO DEL CUADRE    : ' WS-ESTADO-CUADRE
               DELIMITED BY SIZE INTO REG-REPORTE.
           WRITE REG-REPORTE.
           MOVE WS-SEPARADOR TO REG-REPORTE.
           WRITE REG-REPORTE.

       3950-FMT-FECHA-MOV.
           STRING SRT-FECHA(9:2) '/' SRT-FECHA(6:2) '/' SRT-FECHA(1:4)
               DELIMITED BY SIZE INTO WS-FECHA-MOV-REP.

       3000-FIN.
           EXIT.
