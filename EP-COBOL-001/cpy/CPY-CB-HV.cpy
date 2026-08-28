      * host vars de validacion
       01 HV-CUENTA-CREDITO PIC 9(10).
       01 HV-CUENTA-DEBITO  PIC 9(10).
       01 HV-ESTADO         PIC X(01).

      * host vars para el insert de asientos
       01 HV-ASI-FECHA      PIC 9(08).
       01 HV-ASI-NUMMOV     PIC 9(10).
       01 HV-ASI-CUENTA     PIC 9(10).
       01 HV-ASI-TIPOAFEC   PIC X(01).
       01 HV-ASI-VALOR      PIC 9(09)V99.
       01 HV-ASI-REF        PIC X(20).
