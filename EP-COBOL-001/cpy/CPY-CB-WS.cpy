      * status de archivos
       01 FS-MOV PIC XX.
       01 FS-ASI PIC XX.
       01 FS-REP PIC XX.

      * switches de control
       01 SW-FIN-ARCHIVO   PIC X VALUE 'N'.
           88 FIN-ARCHIVO      VALUE 'S'.

       01 SW-MOV-VALIDO    PIC X VALUE 'S'.
           88 MOV-VALIDO      VALUE 'S'.
           88 MOV-RECHAZADO  VALUE 'N'.

      * fecha contable ingresada
       01 IN-FECHA-CONTABLE PIC 9(8) VALUE ZERO.

      * indice
       01 IDX-M PIC 99 VALUE ZERO.

      * contadores generales
       01 WS-LEIDOS         PIC 9(07) VALUE 0.
       01 WS-CONTABILIZADOS PIC 9(07) VALUE 0.
       01 WS-RECHAZADOS     PIC 9(07) VALUE 0.

      * contadores de rechazo por motivo
       01 WS-RECH-SUC-INEX  PIC 9(07) VALUE 0.
       01 WS-RECH-SUC-INAC  PIC 9(07) VALUE 0.
       01 WS-RECH-TIPO-MOV  PIC 9(07) VALUE 0.
       01 WS-RECH-CTA-INAC  PIC 9(07) VALUE 0.
       01 WS-RECH-VALOR     PIC 9(07) VALUE 0.

      * totales del reporte
       01 WS-TOT-DEBITO     PIC 9(13)V99 VALUE 0.
       01 WS-TOT-CREDITO    PIC 9(13)V99 VALUE 0.


      * lineas del reporte etiqueta y valor
       01 WS-LIN-NUM.
           05 WS-LN-TXT     PIC X(36).
           05 WS-LN-VAL     PIC ZZZZZZZZ9.

       01 WS-LIN-MON.
           05 WS-LM-TXT     PIC X(36).
           05 WS-LM-VAL     PIC ZZZZZZZZ9.99.

       01 WS-LIN-FEC.
           05 FILLER        PIC X(36) VALUE 'FECHA CONTABLE'.
           05 WS-LF-VAL     PIC 9(08).
