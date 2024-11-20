OUTPUT TO d:\tmp\roxanamar2017.txt.
PUT UNFORMATTED
    'DIVISION|DOC|NUMERO|EMISION|FMA.PGO.|CLIENTE|NOMBRE|ARTICULO|DESCRIPCION|CANTIDAD|UNIDAD|CV UNIT|CV'
    SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND fchdoc >= 03/01/2017
    AND fchdoc <= 03/31/2017
    AND flgest <> 'A'
    AND LOOKUP(coddoc, 'FAC,BOL,TCK') > 0
    AND LOOKUP(fmapgo,'900,899') > 0,
    EACH ccbddocu OF ccbcdocu NO-LOCK,
    FIRST almmmatg OF ccbddocu NO-LOCK:
    PUT UNFORMATTED
        ccbcdocu.divori '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.fmapgo '|'
        ccbcdocu.codcli '|'
        ccbcdocu.nomcli '|'
        ccbddocu.codmat '|'
        almmmatg.desmat '|'
        ccbddocu.candes '|'
        ccbddocu.undvta '|'
        ccbddocu.preuni '|'
        ccbddocu.implin 
        SKIP.
END.


