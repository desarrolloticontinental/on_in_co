DEF VAR x-codmat AS CHAR FORMAT 'x(6)'.

DEF BUFFER b-cmov FOR almcmov.
DEF STREAM pantalla.
INPUT FROM d:\tmp\codigos.prn.
OUTPUT TO d:\tmp\transferencias.txt.
REPEAT:
    IMPORT x-codmat.
    IF x-codmat = '' THEN LEAVE.
    FOR EACH almdmov NO-LOCK WHERE almdmov.codcia = 1
        AND almdmov.codmat = x-codmat
        AND almdmov.fchdoc >= 01/01/17
        AND tipmov = 'i'
        AND codmov = 03,
        FIRST almcmov OF almdmov NO-LOCK:
        FIND FIRST b-cmov WHERE b-cmov.codcia = 1
            AND b-cmov.tipmov = 's'
            AND b-cmov.codmov = 03
            AND b-cmov.codalm = almcmov.almdes
            AND b-cmov.nroser = INTEGER(SUBSTRING(almcmov.nrorf1,1,3))
            AND b-cmov.nrodoc = INTEGER(SUBSTRING(almcmov.nrorf1,4))
            NO-LOCK.
        IF AVAILABLE b-cmov AND b-cmov.fchdoc < 01/01/17 THEN DO:
            FIND LAST almstkge WHERE almstkge.codcia = 001
                AND almstkge.codmat = x-codmat AND almstkge.fecha < 01/01/17 NO-LOCK.
            PUT UNFORMATTED
                almcmov.codalm '|' 
                almcmov.tipmov '|'
                almcmov.codmov '|'
                almcmov.fchdoc '|'
                almcmov.nrodoc '|'
                b-cmov.codalm '|'
                b-cmov.nroser '|'
                b-cmov.nrodoc '|'
                b-cmov.fchdoc '|'
                almdmov.codmat '|'
                almdmov.candes * almdmov.factor '|'
                AlmStkge.CtoUni
                SKIP.

        END.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.

