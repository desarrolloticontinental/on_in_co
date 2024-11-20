OUTPUT TO c:\tmp\promocion.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'FAC,BOL,TCK') > 0
    AND fchdoc >= 02/20/2015 AND fchdoc <= 03/31/2015,
    FIRST faccpedi NO-LOCK WHERE faccpedi.codcia = 001
    AND faccpedi.coddoc = ccbcdocu.codped
    AND faccpedi.nroped = ccbcdocu.nroped,
    EACH ccbddocu OF ccbcdocu NO-LOCK WHERE ccbddocu.codmat = '053840',
    FIRST facdpedi OF faccpedi NO-LOCK WHERE facdpedi.codmat = ccbddocu.codmat
    AND facdpedi.libre_c05 = 'OF':
    PUT UNFORMATTED
        ccbcdocu.coddiv '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbddocu.codmat '|'
        ccbddocu.candes '|'
        ccbddocu.factor '|'
        ccbcdocu.codped '|'
        ccbcdocu.nroped '|'
        SKIP.
END.

