
       01 TB-CONCEPTOS-COBRO.
          05 CONCEPTOS-COBRO OCCURS 50 TIMES INDEXED BY IDX-A.
              10 CC-CODIGO-CONCEPTO PIC X(4).
              10 CC-DESCRIPCION PIC X(30).
              10 CC-TIPO-COBRO PIC X(1).
                  88 CC-FIJO VALUE 'F'.
                  88 CC-VARIABLE VALUE 'V'.
              10 CC-ESTADO PIC X(1).
                  88 CC-ACTIVO VALUE 'A'.
                  88 CC-INACTIVO VALUE 'I'.

       01 CONCEPTOS-COBRO-COUNT PIC 9(2) VALUE ZERO.
       