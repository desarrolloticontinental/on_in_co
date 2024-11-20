DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'I/C' NO-UNDO.

FOR EACH ccbccaja WHERE codcia = s-codcia
    AND coddoc = s-coddoc
    AND nrodoc >= '015128806'
    AND nrodoc <= '015128810':
    ASSIGN
        fchdoc = 10/31/2012.
    FOR EACH ccbdcaja OF ccbccaja:
        ccbdcaja.fchdoc = ccbccaja.fchdoc.
    END.
END.
