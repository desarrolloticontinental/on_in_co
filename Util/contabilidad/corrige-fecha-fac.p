/* CAMBIAR DE FECHA */

DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddoc AS CHAR INIT 'BOL'.
DEF VAR s-nrodoc AS CHAR INIT '74400000138'.

FIND ccbcdocu WHERE codcia = s-codcia
    AND coddoc = s-coddoc
    AND nrodoc = s-nrodoc.
UPDATE ccbcdocu.fchdoc.
FOR EACH ccbddocu OF ccbcdocu:
    ccbddocu.fchdoc = ccbcdocu.fchdoc.
END.

FOR EACH almcmov WHERE almcmov.codcia = s-codcia 
    AND almcmov.codref = ccbcdocu.coddoc
    AND almcmov.nroref = ccbcdocu.nrodoc
    AND almcmov.tipmov = 'S':
    almcmov.fchdoc = ccbcdocu.fchdoc.
    FOR EACH almdmov OF almcmov:
        almdmov.fchdoc = almcmov.fchdoc.
    END.
END.
