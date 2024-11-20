DEF TEMP-TABLE t-vtatabla LIKE vtatabla.

FOR EACH vtatabla NO-LOCK WHERE codcia = 1 AND Tabla = "BREVETE":
    CREATE t-vtatabla.
    BUFFER-COPY vtatabla TO t-vtatabla.
END.

DEF VAR x-linea AS CHAR NO-UNDO.
INPUT FROM d:\choferes.csv.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF TRUE <> (x-linea > '') THEN LEAVE.
    IF x-linea BEGINS 'Activo;' THEN NEXT.
    FIND FIRST t-vtatabla WHERE t-vtatabla.codcia = 1
        AND t-vtatabla.tabla = "BREVETE"
        AND t-vtatabla.llave_c1 = ENTRY(2,x-linea,';')
        NO-ERROR.
    IF NOT AVAILABLE t-vtatabla THEN NEXT.
    ASSIGN
        t-vtatabla.llave_c8 =  ENTRY(1,x-linea,';')
        t-vtatabla.llave_c2 =  ENTRY(3,x-linea,';')
        t-vtatabla.libre_c01 =  ENTRY(4,x-linea,';')
        t-vtatabla.libre_c02 =  ENTRY(5,x-linea,';')
        t-vtatabla.libre_c03 =  ENTRY(6,x-linea,';')
        t-vtatabla.rango_fecha[1] =  DATE(ENTRY(7,x-linea,';'))
        .

END.
INPUT CLOSE.

FOR EACH t-vtatabla NO-LOCK:
    FIND FIRST vtatabla WHERE vtatabla.codcia = t-vtatabla.codcia 
        AND vtatabla.tabla = t-vtatabla.tabla
        AND vtatabla.llave_c1 = t-vtatabla.llave_c1
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE vtatabla THEN DO:
        BUFFER-COPY t-vtatabla TO vtatabla.
        RELEASE vtatabla.
    END.
END.



