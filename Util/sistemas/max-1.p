DEF VAR x-linea AS CHAR.
INPUT FROM d:\cambiar.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF TRUE <> (x-linea > '') THEN LEAVE.
    FIND faccpedi WHERE codcia = 1
        AND coddoc = SUBSTRING(x-linea,1,5)
        AND nroped = SUBSTRING(x-linea,6,15).
    fchent = DATE(SUBSTRING(x-linea,21)).
END.
