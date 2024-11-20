DEF TEMP-TABLE t-gn-vehic LIKE gn-vehic.

FOR EACH gn-vehic NO-LOCK WHERE codcia = 1:
    CREATE t-gn-vehic.
    BUFFER-COPY gn-vehic TO t-gn-vehic.
END.

DEF VAR x-linea AS CHAR NO-UNDO.
INPUT FROM d:\vehiculos.csv.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF TRUE <> (x-linea > '') THEN LEAVE.
    IF x-linea BEGINS 'PLACA;' THEN NEXT.
    FIND FIRST t-gn-vehic WHERE t-gn-vehic.codcia = 1
        AND t-gn-vehic.placa = ENTRY(1,x-linea,';')
        NO-ERROR.
    IF NOT AVAILABLE t-gn-vehic THEN NEXT.
    ASSIGN
        t-gn-vehic.tipo = ENTRY(2,x-linea,';')
        t-gn-vehic.libre_c05 = ENTRY(3,x-linea,';')
        t-gn-vehic.marca = ENTRY(4,x-linea,';')
        t-gn-vehic.libre_c03 = ENTRY(5,x-linea,';')
        t-gn-vehic.codpro = ENTRY(6,x-linea,';')
        t-gn-vehic.carga = DECIMAL(ENTRY(7,x-linea,';'))
        t-gn-vehic.libre_d01 = DECIMAL(ENTRY(8,x-linea,';'))
        t-gn-vehic.volumen = DECIMAL(ENTRY(9,x-linea,';'))
        t-gn-vehic.libre_c02 = ENTRY(10,x-linea,';')
        t-gn-vehic.libre_c04 = ENTRY(11,x-linea,';')
        t-gn-vehic.nroestibadores = INTEGER(ENTRY(12,x-linea,';'))
        t-gn-vehic.vtorevtecnica = DATE(ENTRY(13,x-linea,';'))
        t-gn-vehic.vtoextintor = DATE(ENTRY(14,x-linea,';'))
        t-gn-vehic.vtosoat = DATE(ENTRY(15,x-linea,';'))
        t-gn-vehic.libre_d02 = (IF ENTRY(16,x-linea,';') = "NO" THEN 0 ELSE 1)
        .

END.
INPUT CLOSE.

FOR EACH t-gn-vehic NO-LOCK:
    FIND FIRST gn-vehic OF t-gn-vehic EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE gn-vehic THEN BUFFER-COPY t-gn-vehic TO gn-vehic NO-ERROR.
    RELEASE gn-vehic.
END.

