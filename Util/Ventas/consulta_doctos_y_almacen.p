DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.

FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'fac'
    AND nrodoc >= '002003382'
    AND nrodoc <= '002003382':
    DISPLAY coddoc nrodoc codalm fchdoc fchvto flgest imptot ccbcdocu.impbrt ccbcdocu.impdto ccbcdocu.impvta ccbcdocu.impigv ccbcdocu.imptot.
    FOR EACH ccbddocu OF ccbcdocu, FIRST Almmmatg OF Ccbddocu NO-LOCK:
        DISPLAY ccbddocu.coddoc ccbddocu.nrodoc ccbddocu.codmat Ccbddocu.ImpLin Ccbddocu.ImpIgv Almmmatg.aftigv.
    END.
    FIND LAST almcmov WHERE almcmov.codcia = ccbcdocu.codcia
        AND almcmov.codref = ccbcdocu.coddoc
        AND almcmov.nroref = ccbcdocu.nrodoc        
        NO-ERROR.
    IF AVAILABLE almcmov THEN DO:
        DISPLAY SKIP 'Cia  Alm  tipmov  codmov  serie  nrodoc   std   fchdoc' SKIP almcmov.codcia almcmov.codalm almcmov.tipmov almcmov.codmov almcmov.nroser almcmov.nrodoc almcmov.flgest almcmov.fchdoc.
        FOR EACH almdmov OF almcmov:
            DISPLAY almcmov.flgest almdmov.codalm almdmov.codmat almdmov.candes.
        END.
    END.
END.

