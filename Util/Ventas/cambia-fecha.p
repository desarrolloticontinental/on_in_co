DISABLE TRIGGERS FOR LOAD OF almcmov.
    DISABLE TRIGGERS FOR LOAD OF almdmov.
FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'bol'
    AND nrodoc = '75800001001':
    DISPLAY coddoc nrodoc fchdoc.
    UPDATE fchdoc.
    /*ccbcdocu.fchvto = ccbcdocu.fchdoc.*/
    FOR EACH ccbddocu OF ccbcdocu:
        ccbddocu.fchdoc = ccbcdocu.fchdoc.
    END.
    FIND almcmov WHERE almcmov.codcia = 1
        AND almcmov.codref = ccbcdocu.coddoc
        AND almcmov.nroref = ccbcdocu.nrodoc.
    almcmov.fchdoc = ccbcdocu.fchdoc.
    FOR EACH almdmov OF almcmov:
        almdmov.fchdoc = almcmov.fchdoc.
    END.
    FOR EACH ccbdcaja WHERE ccbdcaja.codcia = 1
        AND ccbdcaja.codref = ccbcdocu.coddoc
        AND ccbdcaja.nroref = ccbcdocu.nrodoc,
        FIRST ccbccaja OF ccbdcaja.
        ccbccaja.fchdoc = ccbcdocu.fchdoc.
        ccbdcaja.fchdoc = ccbcdocu.fchdoc.
    END.
END.
