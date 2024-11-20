DEF var x-fecha-1 AS DATE NO-UNDO.
DEF var x-fecha-2 AS DATE NO-UNDO.
DEF VAR x-nroser AS INT NO-UNDO.
DEF VAR x-nrodoc AS INT NO-UNDO.
DEF VAR x-nrolimite AS INT NO-UNDO.
DEF STREAM reporte.

ASSIGN
    x-fecha-1 = DATE(03,01,2020)
    x-fecha-2 = DATE(03,31,2020).
OUTPUT STREAM reporte TO d:\tmp\errores.txt.
FOR EACH faccorre NO-LOCK WHERE faccorre.codcia = 1
    AND LOOKUP(faccorre.coddoc, 'FAC,BOL') > 0
    AND faccorre.flgest = YES:
    DISPLAY faccorre.coddiv faccorre.coddoc faccorre.nroser WITH STREAM-IO NO-BOX.
    PAUSE 0.
    FIND LAST ccbcdocu USE-INDEX llave00 WHERE ccbcdocu.codcia = faccorre.codcia AND
        ccbcdocu.coddiv = faccorre.coddiv AND
        ccbcdocu.coddoc = faccorre.coddoc AND
        ccbcdocu.nrodoc BEGINS STRING(faccorre.nroser, '999') AND
        ccbcdocu.fchdoc <= x-fecha-2
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ccbcdocu OR ccbcdocu.fchdoc < x-fecha-1 THEN NEXT.
    x-nrolimite = INTEGER(SUBSTRING(ccbcdocu.nrodoc,4)).
    FIND LAST ccbcdocu USE-INDEX llave00 WHERE ccbcdocu.codcia = faccorre.codcia AND
        ccbcdocu.coddiv = faccorre.coddiv AND
        ccbcdocu.coddoc = faccorre.coddoc AND
        ccbcdocu.nrodoc BEGINS STRING(faccorre.nroser, '999') AND
        ccbcdocu.fchdoc < x-fecha-1
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ccbcdocu THEN NEXT.
    /* Buscamos faltantes */
    ASSIGN
        x-nroser = INTEGER(SUBSTRING(ccbcdocu.nrodoc,1,3))
        x-nrodoc = INTEGER(SUBSTRING(ccbcdocu.nrodoc,4))
        .

    REPEAT:
        ASSIGN
            x-nrodoc = x-nrodoc + 1.
        IF x-nrodoc > x-nrolimite THEN LEAVE.
        FIND ccbcdocu WHERE ccbcdocu.codcia = faccorre.codcia AND
            ccbcdocu.coddiv = faccorre.coddiv AND
            ccbcdocu.coddoc = faccorre.coddoc AND
            ccbcdocu.nrodoc = STRING(x-nroser,'999') +
                                STRING(x-nrodoc, '99999999')
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbcdocu THEN DO:
            PUT STREAM reporte UNFORMATTED faccorre.coddiv '|'
                faccorre.coddoc '|'
                x-nroser FORMAT '999' '|'
                x-nrodoc FORMAT '99999999' SKIP.
            NEXT.
        END.
        IF ccbcdocu.fchdoc > x-fecha-2 THEN LEAVE.
    END.
END.
OUTPUT CLOSE.

