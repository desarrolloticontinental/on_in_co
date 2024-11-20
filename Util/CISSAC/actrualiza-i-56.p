DEF VAR x-linea AS CHAR NO-UNDO.

INPUT FROM c:\tmp\valorizacion.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND almcmov WHERE codcia = 1
        AND codalm ='21'
        AND tipmov = 'i'
        AND codmov = 56
        AND nrodoc = INTEGER(SUBSTRING(x-linea,1,6)).
    CREATE almdmov.
    BUFFER-COPY almcmov 
        TO almdmov
        ASSIGN
        almdmov.codmon = 2
        almdmov.tpocmb = 2.9
        almdmov.codmat = SUBSTRING(x-linea,11,6)
        almdmov.candes = DECIMAL(SUBSTRING(x-linea,21,15))
        almdmov.factor = 1
        almdmov.codund = SUBSTRING(x-linea,66)
        almdmov.impcto = DECIMAL(SUBSTRING(x-linea,51,15))
        almdmov.preuni = almdmov.impcto / almdmov.candes.
END.
INPUT CLOSE.

