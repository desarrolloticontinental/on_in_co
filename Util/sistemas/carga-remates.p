DEF VAR x-linea AS CHAR.

INPUT FROM d:\remate2.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    FIND almmmatg WHERE codcia = 1
        AND codmat = SUBSTRING(x-linea,1,6)
        NO-LOCK.
    FIND vtatabla WHERE vtatabla.codcia = 1
        AND tabla = 'REMATES'
        AND llave_c1 = SUBSTRING(x-linea,1,6)
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE vtatabla THEN DO:
        CREATE vtatabla.
        ASSIGN
            VtaTabla.CodCia = 001
            VtaTabla.Tabla = 'REMATES'
            vtatabla.llave_c1  = SUBSTRING(x-linea,1,6).
    END.
    ASSIGN
        vtatabla.valor[1] = DECIMAL(SUBSTRING(x-linea,7)).
    IF almmmatg.monvta = 2 THEN DO:
        vtatabla.valor[1] = vtatabla.valor[1] / almmmatg.tpocmb.
    END.
END.
