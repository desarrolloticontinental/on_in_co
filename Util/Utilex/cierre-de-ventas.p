DEF VAR k AS  DATE NO-UNDO.

OUTPUT TO c:\tmp\utilex-cierre-ventas.txt.
FOR EACH gn-divi NO-LOCK WHERE codcia = 001
    AND LOOKUP(coddiv, '00023,00027,00501,00502,00503') > 0:
    DO k = 01/01/2011 TO TODAY:
        FOR LAST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
            AND ccbcdocu.coddiv = gn-divi.coddiv
            AND ccbcdocu.coddoc = 'TCK'
            AND ccbcdocu.fchdoc = k
            AND ccbcdocu.flgest <> 'A'
            BY ccbcdocu.fchdoc BY ccbcdocu.horcie:
            PUT UNFORMATTED
                gn-divi.coddiv '|'
                ccbcdocu.coddoc '|'
                ccbcdocu.nrodoc '|'
                ccbcdocu.fchdoc '|'
                ccbcdocu.horcie
                SKIP.
        END.
    END.
END.
OUTPUT CLOSE.

