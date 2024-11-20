DEF TEMP-TABLE detalle
    FIELD programa AS CHAR
    FIELD sesiones AS INTE
    FIELD etiqueta AS CHAR.
DEF VAR x-linea AS CHAR.
DEF VAR X AS INTE.

INPUT FROM d:\resumen.txt.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF TRUE <> (x-linea > '') THEN LEAVE.
    X = NUM-ENTRIES(x-linea,'|').
    CREATE detalle.
    IF X = 2 THEN DO:
        ASSIGN
            detalle.programa = ENTRY(1,x-linea,'|').
        ASSIGN
            detalle.sesiones = INTEGER(ENTRY(2,x-linea,'|')).
    END.
    ELSE DO:
        ASSIGN
            detalle.programa = SUBSTRING(x-linea,1,1 + LENGTH(x-linea) - LENGTH(ENTRY(X,x-linea,'|'))).
        ASSIGN
            detalle.sesiones = INTEGER(ENTRY(X,x-linea,'|')) NO-ERROR.
    END.
    FIND FIRST pf-g002 WHERE pf-g002.programa = detalle.programa
        NO-LOCK NO-ERROR.
    IF AVAILABLE pf-g002 THEN detalle.etiqueta = pf-g002.etiqueta.
END.

OUTPUT TO d:\ocurrencias.txt.
FOR EACH detalle:
    PUT UNFORMATTED
        detalle.programa ';'
        detalle.etiqueta ';'
        detalle.sesiones SKIP.
END.
