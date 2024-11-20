DEF VAR x-fecha AS DATE NO-UNDO.
OUTPUT TO d:\tmp\series.txt.
FOR EACH faccorre NO-LOCK WHERE codcia = 001
    AND LOOKUP(faccorre.coddoc, 'FAC,BOL,N/C,N/D,G/R') > 0,
    FIRST gn-divi OF faccorre NO-LOCK:
    x-fecha = ?.
    FOR LAST ccbcdocu NO-LOCK USE-INDEX Llave01 WHERE ccbcdocu.codcia = 001
        AND ccbcdocu.coddoc = faccorre.coddoc
        AND ccbcdocu.nrodoc BEGINS STRING(faccorre.nroser, '999'):
        IF x-Fecha = ? THEN x-Fecha = Ccbcdocu.fchdoc.
        x-fecha = MAXIMUM(x-fecha, ccbcdocu.fchdoc).
    END.
    IF faccorre.coddoc = "G/R" THEN DO:
        FOR EACH almacen NO-LOCK WHERE almacen.codcia = 001:
            FOR LAST almcmov NO-LOCK USE-INDEX Almc02 WHERE almcmov.codcia = 001
                AND almcmov.codalm = almacen.codalm
                AND almcmov.tipmov = 'S'
                AND almcmov.codmov = 03
                AND almcmov.nroser = faccorre.nroser:
                IF x-fecha = ? THEN x-fecha = almcmov.fchdoc.
                x-fecha = MAXIMUM(x-fecha, almcmov.fchdoc).
            END.
        END.
    END.
    PUT UNFORMATTED
        faccorre.coddiv '|'
        gn-divi.desdiv '|'
        faccorre.coddoc '|'
        faccorre.nroser '|'
        faccorre.flgest '|'
        x-fecha
        SKIP.
END.
