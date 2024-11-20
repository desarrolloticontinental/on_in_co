DEF STREAM report.
OUTPUT STREAM report TO c:\tmp\diferencias-fac.txt.
FOR EACH gn-divi NO-LOCK WHERE codcia = 001:
    DISPLAY gn-divi.coddiv.
    PAUSE 0.
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
        AND ccbcdocu.coddoc = 'fac'
        AND ccbcdocu.coddiv = gn-divi.coddiv
        AND ccbcdocu.fchdoc >= 07/01/2012
        AND ccbcdocu.fchdoc <= TODAY
        AND ccbcdocu.flgest <> 'A':
        FIND almcmov WHERE almcmov.codcia = 001
            AND almcmov.codalm = ccbcdocu.codalm
            AND almcmov.codref = ccbcdocu.coddoc
            AND almcmov.nroref = ccbcdocu.nrodoc
            NO-LOCK NO-ERROR.
        IF AVAILABLE almcmov THEN DO:
            FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
                FIND almdmov OF almcmov WHERE almdmov.codmat = ccbddocu.codmat
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE almdmov THEN NEXT.
                IF ccbddocu.candes <> almdmov.candes
                    OR ccbddocu.factor <> almdmov.factor
                    OR ccbddocu.undvta <> almdmov.codund 
                    OR ccbddocu.preuni <> almdmov.preuni
                    THEN
                    PUT STREAM report
                    UNFORMATTED
                    ccbcdocu.coddiv '|'
                    ccbcdocu.fchdoc '|'
                    ccbcdocu.coddoc '|'
                    ccbcdocu.nrodoc '|'
                    ccbddocu.codmat '|'
                    ccbddocu.candes '|'
                    almdmov.candes '|'
                    ccbddocu.factor '|'
                    almdmov.factor '|'
                    ccbddocu.undvta '|'
                    almdmov.codund '|'
                    ccbddocu.preuni '|'
                    almdmov.preuni 
                    SKIP.
            END.
        END.
    END.
END.
