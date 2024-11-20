DEF VAR x-docs AS CHAR INIT 'FAC,BOL,G/R,N/C,N/D'.
DEF VAR x-coddoc AS CHAR.

DEF VAR k AS INT.

OUTPUT TO c:\tmp\series.txt.
DO k = 1 TO 5:
    x-coddoc = ENTRY(k, x-docs).
    FOR EACH ccbcdocu NO-LOCK WHERE codcia = 001
        AND coddoc = x-coddoc
        BREAK BY SUBSTRING(nrodoc,1,3) BY nrodoc DESC:
        IF FIRST-OF(SUBSTRING(nrodoc,1,3)) THEN
            DISPLAY
            ccbcdocu.coddoc     FORMAT 'x(3)'
            SUBSTRING(nrodoc,1,3)   FORMAT 'x(3)'
            ccbcdocu.nrodoc     FORMAT 'x(10)'
            WITH STREAM-IO NO-BOX NO-LABELS.
    END.
END.
FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = 1
    AND almcmov.tipmov = 's'
    AND almcmov.codmov = 02
    BREAK BY nroser BY nrodoc DESC:
    IF FIRST-OF(nroser) THEN
        DISPLAY
        'G/R'           FORMAT 'x(3)'
        almcmov.nroser FORMAT '999'
        almcmov.nrodoc FORMAT '9999999'
        WITH STREAM-IO NO-BOX NO-LABELS.
END.
