DEF VAR x-linea AS CHAR.

DEF VAR x-codalm AS CHAR.
DEF VAR x-codubi AS CHAR.
DEF VAR x-codzona AS CHAR.

INPUT FROM d:\ubicaciones.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    x-codalm = SUBSTRING(x-linea,1,10).
    x-codubi = SUBSTRING(x-linea,11,10).
    x-codzona = SUBSTRING(x-linea,21,10).
        
    FIND FIRST almtubic WHERE codcia = 1
        AND codalm = x-codalm
        AND codubi = x-codubi
        NO-ERROR.
    IF NOT AVAILABLE almtubic THEN DO:
        CREATE almtubic.
    END.
    ASSIGN
        almtubic.CodCia = 1
        almtubic.CodAlm = x-codalm
        almtubic.CodUbi = x-codubi
        almtubic.CodZona = x-codzona.
        
END.
