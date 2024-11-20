DEF VAR x-nroini AS CHAR NO-UNDO.
DEF VAR x-nrofin AS CHAR NO-UNDO.
DEF VAR x-nroitems AS INT NO-UNDO.

OUTPUT TO c:\tmp\utilex-comprobantes.txt.
PUT UNFORMATTED
    'DIVISION|INICIAL|FINAL|VALIDOS' SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'tck'
    AND LOOKUP(coddiv, '00023,00027,00501,00502,00503') > 0
    AND fchdoc >= 01/01/2013
    AND fchdoc <= 12/31/2013
    BREAK BY coddiv BY coddoc BY substring(nrodoc,1,3) BY nrodoc:
    IF FIRST-OF(coddiv) OR FIRST-OF(coddoc) 
        OR FIRST-OF(SUBSTRING(nrodoc,1,3)) THEN DO:
        ASSIGN
            x-nroini = ccbcdocu.nrodoc
            x-nroitems = 0.
    END.
    x-nrofin = ccbcdocu.nrodoc.
    IF ccbcdocu.flgest <> 'a' THEN x-nroitems = x-nroitems + 1.
    IF LAST-OF(coddiv) OR LAST-OF(coddoc) 
        OR LAST-OF(SUBSTRING(nrodoc,1,3)) THEN DO:
        PUT UNFORMATTED
            ccbcdocu.coddiv '|'
            x-nroini '|'
            x-nrofin '|'
            x-nroitems
            SKIP.
    END.
END.
