DEF TEMP-TABLE t-gn-prov LIKE gn-prov.
DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-tipo AS CHAR NO-UNDO.

INPUT FROM d:\proveedores.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF TRUE <> (x-linea > '') THEN LEAVE.
    IF x-linea BEGINS 'CODIGO' THEN NEXT.
    FIND FIRST gn-prov WHERE gn-prov.codcia = 0
        AND gn-prov.codpro = SUBSTRING(x-linea,1,15)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-prov THEN NEXT.
    CREATE t-gn-prov.
    BUFFER-COPY gn-prov TO t-gn-prov.
END.
INPUT CLOSE.

INPUT FROM d:\proveedores.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF TRUE <> (x-linea > '') THEN LEAVE.
    IF x-linea BEGINS 'CODIGO' THEN NEXT.
    FIND FIRST t-gn-prov WHERE t-gn-prov.codcia = 0
        AND t-gn-prov.codpro = SUBSTRING(x-linea,1,15)
        NO-ERROR.
    IF NOT AVAILABLE t-gn-prov THEN NEXT.
    x-Tipo = SUBSTRING(x-linea,161,10).
    ASSIGN
        t-gn-prov.flgsit = SUBSTRING(x-linea,16,1)
        t-gn-prov.libre_d01 = (IF x-Tipo = "Publico" THEN 1
            ELSE (IF x-Tipo = "Privado" THEN 2 ELSE 0))
        .
END.
INPUT CLOSE.

FOR EACH t-gn-prov NO-LOCK:
    FIND FIRST gn-prov WHERE gn-prov.codcia = 0
        AND gn-prov.codpro = t-gn-prov.codpro 
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN BUFFER-COPY t-gn-prov TO gn-prov NO-ERROR.
    RELEASE gn-prov.
END.


