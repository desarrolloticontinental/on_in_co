DEF BUFFER cdocu FOR ccbcdocu.
DEF BUFFER ddocu FOR ccbddocu.

FIND ccbcdocu WHERE codcia = 1
    AND coddoc = 'fac'
    AND nrodoc = '002335096'
    NO-LOCK.
CREATE cdocu.
BUFFER-COPY ccbcdocu
    TO cdocu
    ASSIGN cdocu.nrodoc = '002335098'.
FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
    CREATE ddocu.
    BUFFER-COPY ccbddocu
        TO ddocu
        ASSIGN ddocu.nrodoc = cdocu.nrodoc.
END.
