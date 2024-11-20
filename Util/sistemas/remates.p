DEF VAR x-linea AS CHAR NO-UNDO.

FOR EACH vtatabla EXCLUSIVE-LOCK WHERE vtatabla.codcia = 001 AND
    vtatabla.tabla = "REMATES":
    DELETE vtatabla.
END.

INPUT FROM d:\remateSet2024.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF TRUE <> (x-linea > "") THEN LEAVE.
    FIND Almmmatg WHERE Almmmatg.codcia = 1 AND 
        Almmmatg.codmat = SUBSTRING(x-linea,1,6)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN NEXT.
    CREATE vtatabla.
    ASSIGN
        vtatabla.codcia = 001
        vtatabla.tabla = "REMATES"
        vtatabla.llave_c1 = SUBSTRING(x-linea,1,6)
        vtatabla.valor[1] = DECIMAL(SUBSTRING(x-linea,7))
        .
    IF Almmmatg.monvta = 2 
        THEN vtatabla.valor[1] = DECIMAL(SUBSTRING(x-linea,7)) / Almmmatg.TpoCmb.
END.
INPUT CLOSE.
