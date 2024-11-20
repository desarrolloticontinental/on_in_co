DEF TEMP-TABLE cdespacho LIKE faccpedi.
DEF TEMP-TABLE ddespacho LIKE facdpedi.

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'o/d'
    AND coddiv = '00018'
    AND flgest = 'C'
    AND fchped >= 10/01/2012
    AND fchped < TODAY:
    CREATE cdespacho.
    BUFFER-COPY faccpedi TO cdespacho.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE ddespacho.
        BUFFER-COPY facdpedi
            TO ddespacho
            ASSIGN
            ddespacho.canate = 0.
    END.
END.

FOR EACH cdespacho:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
        AND ccbcdocu.divori = cdespacho.coddiv
        AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
        AND ccbcdocu.libre_c01 = cdespacho.coddoc
        AND ccbcdocu.libre_c02 = cdespacho.nroped
        AND ccbcdocu.flgest <> "A",
        EACH ccbddocu OF ccbcdocu NO-LOCK,
        FIRST ddespacho OF cdespacho WHERE ddespacho.codmat = ccbddocu.codmat:
        ASSIGN
            ddespacho.canate = ddespacho.canate + ccbddocu.candes.
    END.
END.


FOR EACH cdespacho NO-LOCK,
    FIRST faccpedi OF cdespacho NO-LOCK,
    EACH ddespacho OF cdespacho NO-LOCK,
    FIRST facdpedi OF faccpedi WHERE facdpedi.codmat = ddespacho.codmat:
    IF facdpedi.canate <> ddespacho.canate THEN DO:
        DISPLAY
            facdpedi.canped FORMAT '->>>,>>>,>>9.99'
            facdpedi.canate FORMAT '->>>,>>>,>>9.99'
            ddespacho.canate  FORMAT '->>>,>>>,>>9.99'
            cdespacho.nroped 
            cdespacho.codref
            cdespacho.nroref
            ddespacho.codmat
            WITH STREAM-IO NO-BOX WIDTH 320.
        PAUSE 0.
        ASSIGN
            facdpedi.canate = ddespacho.canate.
    END.
END.


