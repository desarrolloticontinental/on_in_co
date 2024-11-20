DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-codalm AS CHAR INIT '11'.
DEF VAR s-tipmov AS CHAR INIT 'S'.
DEF VAR s-codmov AS INT INIT 03.
DEF VAR s-nrodoc-1 AS INT INIT 70070.
DEF VAR s-nrodoc-2 AS INT INIT 70140.
DEF VAR s-nroser AS INT INIT 014.
DEF VAR s-fchdoc AS DATE.
DEF BUFFER CMOV FOR almcmov.
DEF BUFFER DMOV FOR almdmov.

s-fchdoc = 01/03/2015.

/* creamos documentos anulado y cambiamos de fecha */
FIND FIRST almcmov WHERE codcia = s-codcia
    AND codalm = s-codalm
    AND tipmov = s-tipmov
    AND nroser = s-nroser
    AND nrodoc = s-nrodoc-1 - 1
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE almcmov THEN DO:
    MESSAGE 'error buscando plantilla'.
    RETURN.
END.

DEF VAR k AS INT.
DO k = s-nrodoc-1 TO s-nrodoc-2:
    FIND FIRST CMOV WHERE CMOV.codcia = s-codcia
        AND CMOV.codalm = s-codalm
        AND CMOV.tipmov = s-tipmov
        AND CMOV.nroser = s-nroser
        AND CMOV.nrodoc = k
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE CMOV THEN DO:
        CREATE CMOV.
        BUFFER-COPY almcmov
            TO CMOV
            ASSIGN 
            CMOV.nroser = s-nroser 
            CMOV.codmov = s-codmov
            CMOV.nrodoc = k 
            CMOV.flgest = 'A'.
    END.
    ASSIGN CMOV.fchdoc = s-fchdoc.
    DISPLAY CMOV.nroser CMOV.nrodoc CMOV.fchdoc CMOV.flgest
        WITH STREAM-IO NO-BOX. 
    PAUSE 0.
    FOR EACH DMOV OF CMOV:
        ASSIGN DMOV.fchdoc = CMOV.fchdoc.
    END.
END.
