

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1,
    EACH ccbcdocu WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.coddoc = 'bol'
    AND ccbcdocu.coddiv = gn-divi.coddiv
    AND ccbcdocu.fchdoc >= DATE(10,01,2021)
    AND ccbcdocu.impexo > 0:
    IF ccbcdocu.imptot <> (ccbcdocu.impvta + ccbcdocu.impexo + ccbcdocu.impigv +
                           ccbcdocu.acubon[10])
        THEN DISPLAY ccbcdocu.coddiv ccbcdocu.fchdoc ccbcdocu.coddoc ccbcdocu.nrodoc.
END.
