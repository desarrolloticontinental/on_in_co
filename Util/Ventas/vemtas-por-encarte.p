DEF STREAM reporte.
OUTPUT STREAM reporte TO d:\tmp\porencarte.txt.
FOR EACH gn-divi NO-LOCK WHERE codcia = 1 AND canalventa = 'MIN',
    EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.coddiv = gn-divi.coddiv
    AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,TCK') > 0
    AND flgest <> 'A'
    AND ccbcdocu.libre_c05 = 'CD|728814':
    DISPLAY ccbcdocu.coddiv ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.fchdoc
        WITH STREAM-IO NO-BOX WIDTH 320.
    PAUSE 0.
    IF ( ccbcdocu.fchdoc >= DATE(07,23,2015) AND ccbcdocu.fchdoc <= DATE(08,31,2015)
         OR ccbcdocu.fchdoc >= DATE(07,23,2016) )
        THEN DO:
        FOR EACH ccbddocu OF ccbcdocu NO-LOCK,
            FIRST almmmatg OF ccbddocu NO-LOCK:
            PUT STREAM reporte UNFORMATTED
                gn-divi.coddiv ' ' gn-divi.desdiv '|'
                ccbcdocu.ruccli '|'
                ccbcdocu.nomcli '|'
                ccbcdocu.fchdoc '|'
                almmmatg.codfam '|'
                almmmatg.desmar '|'
                almmmatg.desmat '|'
                ccbddocu.candes * ccbddocu.factor '|'
                ccbddocu.implin - ccbddocu.impdto2
                SKIP.
        END.
    END.
END.
OUTPUT STREAM reporte CLOSE.
